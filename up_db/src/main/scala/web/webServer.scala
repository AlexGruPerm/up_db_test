package web

import conf.WebUiConfig
import data.ImplTestsRepo
import zio.http._
import zio.metrics.connectors.{MetricsConfig, prometheus}
import zio.{ZIO, ZLayer, durationInt}

object webServer {

  private val metricsConfig = ZLayer.succeed(MetricsConfig(1.seconds))

  def startWebServer(): ZIO[ImplTestsRepo with WebUiConfig, Throwable, Unit] = for {
    config <- ZIO.service[WebUiConfig]
    defaultConfig = ServerConfig.default
      .port(config.port)
      .maxThreads(config.nThreads)
    _ <-  (Server.install(WebUiApp.app ++ PrometheusPublisherApp()).flatMap { port =>
      ZIO.logInfo(s"Started server on port: $port with nThreads=${config.nThreads}")
    } *> ZIO.never)
      .provide(
        ServerConfig.live(defaultConfig),
        Server.live,
        ImplTestsRepo.layer,
        metricsConfig,
        prometheus.publisherLayer,
        prometheus.prometheusLayer)
  } yield ()

}
