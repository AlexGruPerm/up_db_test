package app

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object MainApp extends ZIOAppDefault{

  val mainApp: ZIO[Any, Throwable, Unit] = for {
    _ <- ZIO.logInfo("Begin mainApp")
  } yield ()

    def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    mainApp.exitCode
  }
}
