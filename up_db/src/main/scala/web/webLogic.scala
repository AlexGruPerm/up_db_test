package web

import conf.WebUiConfig
import data.ImplTestsRepo
import webui.WebUiApp
import zio.http.Server
import zio.http._
import zio.http.ServerConfig.LeakDetectionLevel
import zio.ZIO

object webLogic {

  def startWebServer(): ZIO[ImplTestsRepo with WebUiConfig, Throwable, Unit] = for {
    conf <- ZIO.service[WebUiConfig]
    config = ServerConfig.default
      .port(conf.port)
      .leakDetection(LeakDetectionLevel.PARANOID)
      .maxThreads(conf.nThreads)
    _ <-  (Server.install(WebUiApp.app).flatMap { port =>
      ZIO.logInfo(s"Started server on port: $port with nThreads=${conf.nThreads}")
    } *> ZIO.never)
      .provide(ServerConfig.live(config), Server.live, ImplTestsRepo.layer)
  } yield ()

}
