package webui


import data.{ImplTestsRepo, TestsRepo, checkTestRepoData}
import error.InputJsonParsingError
import tmodel.{RespTest, RespTestModel, Session, TestModel}
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
  def loadTest(req: Request): ZIO[ImplTestsRepo, Throwable, Response] =
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
          //ZIO.logInfo(s"Success send response with ${u.copy(u.name,u.age+10).toJson}") *>
          //tr.create(testsWithMeta).map { sid =>
          tr.create(testsWithMeta).flatMap{sid =>
            ZIO.logInfo(s"SID=$sid") *>
          ZIO.succeed(Response.json(RespTestModel(
            Session(sid),
            Some(List(
              RespTest(1, "Test of cursor"),
              RespTest(2, "Check exception existence"),
              RespTest(3, "Check returned rows")))
          ).toJson))
      }
         //}
      }
    } yield resp



  def apply(): Http[ImplTestsRepo, Throwable, Request, Response] =
    Http.collectZIO[Request] {
      case Method.GET  -> !! / "greet" / name => getGreet(name)
      case Method.GET  -> !! / "main" => getMainPage
      case req@(Method.POST -> !! / "load_test") => loadTest(req)
      case Method.GET  -> !! / "check" => checkTestsRepo
    }

}

