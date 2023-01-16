package webui

import data.User
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

  import tmodel.EncDecTestModelImplicits._
  import tmodel.EncDecRespTestModelImplicits._
  //CharsetUtil.UTF_8
  def loadTest(req: Request): ZIO[Any, Throwable, Response] =
    for {
/*      _ <- ZIO.foreach(req.headers.toList) {elm =>
         ZIO.logInfo(s"header = $elm")
      }*/
      bodyAsStr <- req.body.asString
      _ <- ZIO.logInfo(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~") *>
        ZIO.logInfo(s"bodyAsStr = $bodyAsStr") *>
        ZIO.logInfo(s"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

      u <- req.body.asString.map(_.fromJson[TestModel])
        .catchAllDefect{
        case e: Exception => ZIO.succeed(Left(e.getMessage))
      }

      resp <- u match {
        case Left(e) =>
          ZIO.logError(s"Failed to parse the input: $e").as(
            Response.text(e).setStatus(Status.BadRequest)
            //Response.json(InputJsonParsingError(s"Failed to parse the input: $e").toJson).setStatus(Status.BadRequest)
          )
/*          ZIO.logError(s"Failed to parse the input: $e").as(
            Response.text(e).setStatus(Status.BadRequest)
          )*/
        case Right(_)/*Right(u)*/ =>
          //ZIO.logInfo(s"Success send response with ${u.copy(u.name,u.age+10).toJson}") *>
          ZIO.succeed(Response.json(RespTestModel(
            Session("12Rt3eGTgr46fr"),
            Some(List(
              RespTest(1,"Test of cursor"),
              RespTest(2,"Check exception existence"),
              RespTest(3,"Check returned rows")))
          ).toJson))
      }
    } yield resp

  /*
  def loadTest(req: Request): ZIO[Any, Throwable, Response] =
    for {
      bodyStr <- req.body.asString
      _ <- ZIO.logInfo(s"req JSON str = $bodyStr")
      u <- req.body.asString.map(_.fromJson[User])
      resp <- u match {
        case Left(e) =>
          ZIO.debug(s"Failed to parse the input: $e").as(
            Response.text(e).setStatus(Status.BadRequest)
          )
        case Right(u) =>
          ZIO.succeed(Response.json(u.toJson))
      }
    } yield resp
  */

  def apply(): Http[Any, Throwable, Request, Response] =
    Http.collectZIO[Request] {
      case Method.GET  -> !! / "greet" / name => getGreet(name)
      case Method.GET  -> !! / "main" => getMainPage
      case req@(Method.POST -> !! / "load_test") => loadTest(req)//.catchAllDefect(case e: Exception => )
    }


  /*
  u <- req.body.asString.map(_.fromJson[User])
  _ <- ZIO.logInfo(s"req JSON user = $u")
  resp <- ZIO.succeed(Response.json("""{"greetings": "Hello World!"}"""))
  */

}

