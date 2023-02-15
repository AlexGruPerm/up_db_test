package webui

import zio.http._
import zio.http.model.{Method}
import zio.metrics.connectors.prometheus.PrometheusPublisher
import zio.{ZIO}

object PrometheusPublisherApp {
  def apply(): Http[PrometheusPublisher, Nothing, Request, Response] = {
    Http.collectZIO[Request] {
      case req@(Method.GET -> !! / "metrics") => ZIO.serviceWithZIO[PrometheusPublisher](_.get.map(Response.text))
    }
  }
}

