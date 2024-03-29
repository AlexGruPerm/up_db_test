package app

import conf.{ConfigHelper, WebUiConfig}
import data.ImplTestsRepo
import web.webServer.startWebServer
import zio.http.Server
import zio.{Config, ConfigProvider, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

/**
 * up_db\src\main\resources\control.conf
*/
object MainApp extends ZIOAppDefault{

  /**
   * http://localhost:8081/main
   * http://localhost:8081/check
   * http://localhost:8081/metrics
  */

  val logic: ZIO[WebUiConfig with ImplTestsRepo,Throwable,Unit] =
    for {
      webuiConf <- ZIO.service[WebUiConfig]
      _ <- ZIO.logInfo(s"port = ${webuiConf.port}")
      _ <- startWebServer().provideSome(ImplTestsRepo.layer/*,Server.default*/)
    } yield ()

  val mainApp: ZIO[ZIOAppArgs, Throwable, Unit] = for {
    _ <- ZIO.logInfo(s"Begin mainApp")
    args <- ZIO.service[ZIOAppArgs]
    confLayer <- ConfigHelper.ConfigZLayer(args)
    _ <- logic.provide(confLayer,ImplTestsRepo.layer)
  } yield ()

    def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
      mainApp.foldZIO(
        err => ZIO.logError(s"Exception - ${err.getMessage}").as(0),
        suc => ZIO.succeed(suc)
      )

}
