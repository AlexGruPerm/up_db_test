package webui


import common.types.TestInRepo
import data.{ImplTestsRepo, TestsRepo, checkTestRepoData}
import error.InputJsonParsingError
import tmodel.{RespTest, RespTestModel, Session, TestModel, TestsToRun}
import zhttp.html.Html
import zhttp.http._
import zio.json.{DecoderOps, EncoderOps}
import zio.{Scope, ZIO}

import java.io.IOException
import scala.io._
import java.io.{FileInputStream, IOException}
import java.nio.charset.Charset


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

  def getGreet(name: String): ZIO[Any, Nothing, Response] =
    for {
      _ <- ZIO.logInfo(s"[greet] name = $name")
      resp <- ZIO.succeed(Response.text(s"Hello $name!"))
    } yield resp

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
      cnt <- tr.elementsCnt
      resp <- ZIO.succeed(Response.json(checkTestRepoData(cnt).toJson))
    } yield resp

  import tmodel.EncDecTestModelImplicits._
  import tmodel.EncDecRespTestModelImplicits._
  import data.TestsRepo
  //CharsetUtil.UTF_8
  def loadTests(req: Request): ZIO[ImplTestsRepo, Throwable, Response] =
    for {
      tr <- ZIO.service[ImplTestsRepo]
/*      bodyAsStr <- req.body.asString
      _ <- ZIO.logInfo(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") *>
        ZIO.logInfo(s"bodyAsStr = $bodyAsStr") *>
        ZIO.logInfo(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")*/

      u <- req.body.asString.map(_.fromJson[TestModel])
        .catchAllDefect{
        case e: Exception => ZIO.succeed(Left(e.getMessage))
      }

      resp <- u match {
        case Left(e) =>
          ZIO.logError(s"Failed to parse the input: $e").as(
            Response.json(InputJsonParsingError(s"Failed to parse the input: $e").toJson).setStatus(Status.BadRequest)
          )
        case Right(testsWithMeta) =>
          tr.create(testsWithMeta).flatMap{sid =>
            ZIO.logInfo(s"SID = $sid") *>
              tr.testsList(sid).map{
                optTests =>
                    Response.json(RespTestModel(
                      Session(sid),
                      optTests.map{trp => trp.map{t => RespTest(t.id, t.name)}}
                    ).toJson)
              }
         }
      }
    } yield resp

  /**
   * Start selected tests (array of id) from Tests set identified by sid.
  */
  def startTests(req: Request): ZIO[ImplTestsRepo, Throwable, Response] =
    for {
      u <- req.body.asString.map(_.fromJson[TestsToRun])
        .catchAllDefect {
          case e: Exception => ZIO.succeed(Left(e.getMessage))
        }
      resp <- u match {
        case Left(e) =>
          ZIO.logError(s"Failed to parse the input: $e").as(
            Response.json(InputJsonParsingError(s"Failed to parse the input: $e").toJson).setStatus(Status.BadRequest)
          )
        case Right(testsToRun) =>
          ZIO.logInfo(s" testsToRun = ${testsToRun.sid} - ${testsToRun.ids}") *>
          //todo: Here we need create and fork effect (Throwable. add .catchAllDefect) that will execute tests and save results back into ref.
          // depends on ref and SID + List(id) as input parameters.
          // return ().
          ZIO.succeed(Response.json(InputJsonParsingError("OK start tests").toJson))
      }
    } yield resp

  def apply(): Http[ImplTestsRepo, Throwable, Request, Response] =
    Http.collectZIO[Request] {
      case Method.GET  -> !! / "greet" / name => getGreet(name)
      case Method.GET  -> !! / "main" => getMainPage
      case Method.GET  -> !! / "check" => checkTestsRepo
      case req@(Method.POST -> !! / "load_test") => loadTests(req)
      case req@(Method.POST -> !! / "start_test") => startTests(req)
    }

}

