package web

import conf.WebUiConfig
import data.ImplTestsRepo
import webui.{PrometheusPublisherApp, WebUiApp}
import zio.http.Server
import zio.http._
import zio.http.ServerConfig.LeakDetectionLevel
import zio.metrics.connectors.{MetricsConfig, prometheus}
import zio.{ZIO, ZLayer, durationInt}

object webLogic {

  private val metricsConfig = ZLayer.succeed(MetricsConfig(1.seconds))

  def startWebServer(): ZIO[ImplTestsRepo with WebUiConfig, Throwable, Unit] = for {
    conf <- ZIO.service[WebUiConfig]
    config = ServerConfig.default
      .port(conf.port)
      .leakDetection(LeakDetectionLevel.PARANOID)
      .maxThreads(conf.nThreads)
    _ <-  (Server.install(WebUiApp.app ++ PrometheusPublisherApp()).flatMap { port =>
      ZIO.logInfo(s"Started server on port: $port with nThreads=${conf.nThreads}")
    } *> ZIO.never)
      .provide(
        ServerConfig.live(config),
        Server.live,
        ImplTestsRepo.layer,
        metricsConfig,
        prometheus.publisherLayer,
        prometheus.prometheusLayer)
  } yield ()

}
