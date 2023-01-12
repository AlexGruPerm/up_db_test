package webui

import data.User
import zhttp.html.Html
import zhttp.http._
import zio.json.DecoderOps
import zio.{Scope, ZIO}

import java.io.IOException
import scala.io._
import java.io.{FileInputStream, IOException}
import java.nio.charset.Charset



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

  def loadTest(req: Request): ZIO[Any, Throwable, Response] =
    for {/*
      u <- req.bodyAsString.map(_.fromJson[User])
      r <- u match
      case Left(e) =>
      ZIO.debug(s"Failed to parse the input: $e").as(
      Response.text(e).setStatus(Status.BadRequest)
      )

      case Right(u) =>
      UserRepo.register(u)
      .map(id => Response.text(id))
      */
      //u <- req.body.asString.map(_.fromJson[User])
      //_ <- ZIO.logInfo(s"req JSON = $js")
      //reqJs <- req.body.asString
      //resp <- ZIO.succeed(Response.json(reqJs))
      resp <- ZIO.succeed(Response.json("""{"greetings": "Hello World!"}"""))
    } yield resp


  def apply(): Http[Any, Throwable, Request, Response] =
    Http.collectZIO[Request] {
      case Method.GET  -> !! / "greet" / name => getGreet(name)
      case Method.GET  -> !! / "main" => getMainPage
      case req@(Method.POST -> !! / "load_test") => loadTest(req)
    }



}

