package webui


import common.types.{SessionId, TestInRepo}
import data.{ImplTestsRepo}
import error.ResponseMessage
import runner.{TestRunner, TestRunnerImpl}
import tmodel.{RespTest, RespTestModel, Session, TestModel, TestsToRun}
import zio.http._
import zio.http.model.{Method, Status}
import zio.json.{DecoderOps, EncoderOps}
import zio.{Scope, ZIO, ZLayer}

import java.io.IOException
import scala.io._


/**
 * https://zio.github.io/zio-http/docs/v1.x/dsl/request
 * https://zio.github.io/zio-http/docs/v1.x/examples/advanced-examples/stream-file
*/
object WebUiApp {

  def acquire(name: => String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(Source.fromFile(name))

  def release(source: => Source): ZIO[Any, Nothing, Unit] =
    ZIO.succeedBlocking(source.close())

  def source(name: => String): ZIO[Scope, IOException, Source] =
    ZIO.acquireRelease(acquire(name))(release(_))

  val mainPagePath: String = "E:\\PROJECTS\\up_db_test\\up_db\\src\\main\\scala\\html\\index.html"

  def getMainPage: ZIO[Any, IOException, Response] =
    for {
      _ <- ZIO.logInfo("getMainPage ")
      mainPageContent <- ZIO.scoped{ source(mainPagePath).flatMap { source =>
        ZIO.attemptBlockingIO(source.toList)
      }}
      resp <- ZIO.succeed(Response.html(mainPageContent.mkString))
    } yield resp

  import data.EncDeccheckTestRepoDataImplicits._
  def checkTestsRepo: ZIO[ImplTestsRepo, IOException, Response] =
    for {
      _ <- ZIO.logInfo("checkTestsRepo ")
      tr <- ZIO.service[ImplTestsRepo]
      info <- tr.checkTestRepoData
      resp <- ZIO.succeed(info match {
        case Some(i) => Response.json(i.toJson)
        case _ => Response.json(ResponseMessage("OK start tests").toJson)
      })

    } yield resp

  import TestInRepo._
  import tmodel.EncDecTestModelImplicits._
  /**
   * todo: change response, if BadRerquest then JSON with error, if successful then HTML response.
  */
  def getTestInfo(sid: SessionId, testId: Int): ZIO[ImplTestsRepo, IOException, Response] =
    for {
      _ <- ZIO.logInfo("getTestInfo ")
      tr <- ZIO.service[ImplTestsRepo]
      tests <- tr.testsList(sid)
      resp = (tests match {
        case Some(testsList) => testsList.find(_.id == testId) match {
          case Some(thisTest) =>
            //Response.html(thisTest.getTestAsHtml, Status.Ok)
             Response.json(thisTest.toJson)
          case None => Response.json(ResponseMessage(s"Test [$testId] not found in repo.").toJson)
            .setStatus(Status.BadRequest)
        }
        case None => Response.json(ResponseMessage(s"Test [$testId] not found in repo.").toJson)
          .setStatus(Status.BadRequest)
      })
    } yield resp

  import tmodel.EncDecTestModelImplicits._
  import tmodel.EncDecRespTestModelImplicits._
  import data.TestsRepo
  //CharsetUtil.UTF_8
  def loadTests(req: Request): ZIO[ImplTestsRepo, Exception, Response] =
    for {
      tr <- ZIO.service[ImplTestsRepo]
      /*      bodyAsStr <- req.body.asString
      _ <- ZIO.logInfo(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") *>
        ZIO.logInfo(s"bodyAsStr = $bodyAsStr") *>
        ZIO.logInfo(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")*/

      u <- req.body.asString.map(_.fromJson[TestModel])
        .catchAll{
          case e: Exception => ZIO.succeed(Left(e.getMessage))
        }

      resp <- u match {
        case Left(exp_str) => ZioResponseMsgBadRequest(exp_str)
        case Right(testsWithMeta) =>
          tr.create(testsWithMeta).flatMap { sid =>
            ZIO.logInfo(s"SID = $sid") *>
              tr.testsList(sid).map {
                optTests =>
                  Response.json(RespTestModel(
                    Session(sid),
                    optTests.map { trp => trp.map { t => RespTest(t.id, s"[${t.id}] ${t.name}") } }
                  ).toJson)
              }
          }.foldZIO(
            error   => ZioResponseMsgBadRequest(error.getMessage),
            success => ZIO.succeed(success)
          )
      }
    } yield resp

  /**
   * todo: in JS move const tests_container = document.getElementById("test_list"); and for containers in global area
  */
  private def startTestsLogic(testsToRun: TestsToRun): ZIO[ImplTestsRepo with TestRunner, Exception, Unit] = for {
    tr <- ZIO.service[ImplTestsRepo]
    _ <- ZIO.logInfo(s" testsToRun = ${testsToRun.sid} - ${testsToRun.ids}")
    _ <- ZIO.logInfo(s" Call disableAllTest for sid=${testsToRun.sid}")
      .when(testsToRun.ids.getOrElse(List[Int]()).isEmpty)
    _ <- tr.disableAllTestAndClearExecRes(testsToRun.sid)
    _ <- ZIO.foreachDiscard(testsToRun.ids.getOrElse(List[Int]())) {
      testId => tr.enableTest(testsToRun.sid, testId)
    }
    testRunner <- ZIO.service[TestRunner]
    _ <- testRunner.run()
  } yield ()

  //todo: Analyze and remove boilerplate
  //todo: ZIO.succeed(Response.json(ResponseMessage(err.getMessage).toJson).setStatus(Status.BadRequest)) =>
  //todo: ZioResponseMsgBadRequest(message: String)

  def ZioResponseMsgBadRequest(message: String): ZIO[Any,Nothing,Response] =
    ZIO.succeed(Response.json(ResponseMessage(message).toJson).setStatus(Status.BadRequest))

  /**
   * Start selected tests (array of id) from Tests set identified by sid.
  */
  def startTests(req: Request): ZIO[ImplTestsRepo, Exception, Response] =
    for {
      tr <- ZIO.service[ImplTestsRepo]
      u <- req.body.asString.map(_.fromJson[TestsToRun])
        .catchAll{
          case e: Exception => ZIO.succeed(Left(e.getMessage))
        }
      resp <- u match {
        case Left(exp_str) => ZioResponseMsgBadRequest(exp_str)
        case Right(testsToRun) =>
          startTestsLogic(testsToRun).provide(ZLayer.succeed(tr),TestRunnerImpl.layer, ZLayer.succeed(testsToRun.sid))
          .foldZIO(
            err => ZioResponseMsgBadRequest(err.getMessage),
            _ => ZIO.succeed(Response.json(ResponseMessage("OK").toJson))
          )
      }
    } yield resp

  /**
   * Add catchAll common part to effect.
  */
  def catchCover[A](eff: ZIO[A, Exception, Response]): ZIO[A, Nothing, Response] =
    eff.catchAll { e: Exception =>
      ZIO.logError(e.getMessage) *> ZioResponseMsgBadRequest(e.getMessage)
    }

  //todo: Everywhere common equal part - .catchAll, remove in function
  val app: Http[ImplTestsRepo, Nothing, Request, Response] = Http.collectZIO[Request] {
     case Method.GET  -> !! / "test_info" / sid / testId => catchCover(getTestInfo(sid, testId.toInt))
     case Method.GET  -> !! / "main" => catchCover(getMainPage)
     case Method.GET  -> !! / "check" => catchCover(checkTestsRepo)
     case req@(Method.POST -> !! / "load_test") => catchCover(loadTests(req))
     case req@(Method.POST -> !! / "start_test") => catchCover(startTests(req))
  }


}

