package web

import conf.WebUiConfig
import webui.WebUiApp
import zhttp.service.Server
import zio.ZIO

object webLogic {

  def startWebServer(): ZIO[WebUiConfig, Throwable, Nothing] = for {
    webuiConf <- ZIO.service[WebUiConfig]
    srv <- Server.start(
    port = webuiConf.port,
    http = WebUiApp()
   )
  } yield srv

}
