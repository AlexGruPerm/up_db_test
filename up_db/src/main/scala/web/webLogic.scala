package web

import conf.WebUiConfig
import data.ImplTestsRepo
import webui.WebUiApp
import zhttp.service.Server
import zio.ZIO

object webLogic {

  def startWebServer(): ZIO[ImplTestsRepo with WebUiConfig, Throwable, Nothing] = for {
    webuiConf <- ZIO.service[WebUiConfig]
    srv <- Server.start(
      port = webuiConf.port,
      http = WebUiApp()
   )
  } yield srv

}
