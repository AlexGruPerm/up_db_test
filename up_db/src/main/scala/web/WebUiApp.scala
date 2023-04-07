package web

import common.types.{SessionId}
import data.ImplTestsRepo
import error.ResponseMessage
import runner.{TestRunner, TestRunnerImpl}
import tmodel.{RespTest, RespTestModel, Session, TestModel, TestsToRun}
import zio.http._
import zio.http.model.{Method, Status}
import zio.json.{DecoderOps, EncoderOps}
import zio.metrics.connectors.prometheus.PrometheusPublisher
import zio.{Scope, ZIO, ZLayer}

import java.io.IOException
import scala.io._

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
  def checkTestsRepo(sid: SessionId): ZIO[ImplTestsRepo, IOException, Response] =
    for {
      _ <- ZIO.logInfo("checkTestsRepo ")
      tr <- ZIO.service[ImplTestsRepo]
      info <- tr.checkTestRepoData(sid)
      resp <- ZIO.succeed(info match {
        case Some(i) => Response.json(i.toJson)
        case _ => Response.json(ResponseMessage("OK start tests").toJson)
      })

    } yield resp

  import tmodel.EncDecTestModelImplicits._

  def getTestInfo(sid: SessionId, testId: Int): ZIO[ImplTestsRepo, IOException, Response] =
    for {
      _ <- ZIO.logInfo("getTestInfo ")
      tr <- ZIO.service[ImplTestsRepo]
      tests <- tr.testsList(sid)
      resp = (tests match {
        case Some(testsList) => testsList.find(_.id == testId) match {
          case Some(thisTest) =>
            Response.html(thisTest.getTestAsHtml, Status.Ok)
            //Response.json(thisTest.toJson)
          case None => Response.json(ResponseMessage(s"Test [$testId] not found in repo.").toJson)
            .setStatus(Status.BadRequest)
        }
        case None => Response.json(ResponseMessage(s"Test [$testId] not found in repo.").toJson)
          .setStatus(Status.BadRequest)
      })
    } yield resp


  import tmodel.EncDecRespTestModelImplicits._
  def loadTests(req: Request): ZIO[ImplTestsRepo, Exception, Response] =
    for {
      tr <- ZIO.service[ImplTestsRepo]
/*      bodyAsStr <- req.body.asString.catchAll{
        case e: Exception => ZIO.logError(s"updb-0 error parsing input file with tests : ${e.getMessage}") *>
          ZIO.succeed("{}")
      }
      _ <- ZIO.logInfo(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") *>
        ZIO.logInfo(s"bodyAsStr = $bodyAsStr") *>
        ZIO.logInfo(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")*/

      u <- req.body.asString.map(_.fromJson[TestModel])
        .catchAllDefect{
          case e: Exception => ZIO.logError(s"updb-1 error parsing input file with tests : ${e.getMessage}") *>
            ZIO.succeed(Left(e.getMessage))
        }
        .catchAll{
          case e: Exception => ZIO.logError(s"updb-2 error parsing input file with tests : ${e.getMessage}") *>
            ZIO.succeed(Left(e.getMessage))
        }

      resp <- u match {
        case Left(exp_str) =>  ZioResponseMsgBadRequest(exp_str)
        case Right(testsWithMeta) =>
          tr.create(testsWithMeta).flatMap { sid =>
            ZIO.logInfo(s"SID = $sid") *>
              tr.testsList(sid).map {
                optTests =>
                  Response.json(RespTestModel(
                    Session(sid),
                    optTests.map { trp => trp.map { testInRepo =>
                      RespTest(testInRepo.id, s"[${testInRepo.id}] ${testInRepo.name}") } }
                  ).toJson)
              }
          }.foldZIO(
            error   => ZIO.logError(s"updb-3 error parsing input file with tests : ${error.getMessage}") *>
              ZioResponseMsgBadRequest(error.getMessage),
            success => ZIO.succeed(success)
          )
      }
    } yield resp

  private def startTestsLogic(testsToRun: TestsToRun): ZIO[ImplTestsRepo with TestRunner, Exception, Unit] = for {
    tr <- ZIO.service[ImplTestsRepo]
    _ <- ZIO.logInfo(s" testsToRun = ${testsToRun.sid} - ${testsToRun.ids}")
    _ <- ZIO.logInfo(s" Call disableAllTest for sid = ${testsToRun.sid}")
      .when(testsToRun.ids.getOrElse(List[Int]()).isEmpty)
    _ <- tr.disableAllTestAndClearExecRes(testsToRun.sid)
    _ <- ZIO.foreachDiscard(testsToRun.ids.getOrElse(List[Int]())) {
      testId => tr.enableTest(testsToRun.sid, testId)
    }
    testRunner <- ZIO.service[TestRunner]
    _ <- testRunner.run()
  } yield ()


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
            err => ZIO.logError("-- this point --") *> ZioResponseMsgBadRequest(err.getMessage),
            _ => ZIO.succeed(Response.json(ResponseMessage("OK").toJson))
          )
      }
    } yield resp

  /**
   * Add catchAll common part to effect.
  */
  private def catchCover[C](eff: ZIO[C, Exception, Response]): ZIO[C, Nothing, Response] =
    eff.catchAll { e: Exception =>
      ZIO.logError(e.getMessage) *> ZioResponseMsgBadRequest(e.getMessage)
    }

  val app: Http[ImplTestsRepo with PrometheusPublisher, Nothing, Request, Response] = Http.collectZIO[Request] {
     case Method.GET  -> !! / "test_info" / sid / testId => catchCover(getTestInfo(sid, testId.toInt))
     case Method.GET  -> !! / "main" => catchCover(getMainPage)
     case Method.GET  -> !! / "check" / sid => catchCover(checkTestsRepo(sid))
     case req@(Method.POST -> !! / "load_test") => catchCover(loadTests(req))
     case req@(Method.POST -> !! / "start_test") => catchCover(startTests(req))
  }

}

