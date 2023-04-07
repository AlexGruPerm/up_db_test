import zio.{Runtime, ZIO, ZIOAppDefault, ZLayer}

case class SomeType(name: String)

trait ResponseCatcher[A]{
  def catchCover(eff: A): ZIO[SomeType, Nothing, String]
}

object ResponseCatcherInstances {

  implicit val catchCoverImplTestsRepo = new ResponseCatcher[ZIO[SomeType, Exception, String]] {
    def catchCover(eff: ZIO[SomeType, Exception, String]): ZIO[SomeType, Nothing, String] = {
      eff.catchAll { e: Exception =>
        ZIO.logError(e.getMessage) *> ZIO.succeed(e.getMessage)
      }
    }
  }

}

object ResponseCatcherSyntax {
  implicit class ResponseCatcherOps[ZIO[SomeType, Exception, String]](value: zio.ZIO[SomeType, Exception, String]){
    def catchCover(implicit instance: ResponseCatcher[zio.ZIO[SomeType, Exception, String]]): zio.ZIO[SomeType, Nothing, String] =
      instance.catchCover(value)
  }
}
import ResponseCatcherInstances.catchCoverImplTestsRepo
import ResponseCatcherSyntax.ResponseCatcherOps

val vOk: ZIO[SomeType, Exception, String] = ZIO.succeed("OK - success")
val vError: ZIO[SomeType, Exception, String] = ZIO.fail(new Exception("error here ..."))

object MainApp extends App {
  def run = for {
    _ <- ZIO.logInfo("Begin ...........")
   res <- vOk.catchCover.provide(ZLayer.succeed(SomeType("John"))).as(0)
   //_ <- vError.provide(ZLayer.succeed(SomeType("John")).catchCover
  } yield res
}

val runtime = Runtime.default
runtime.run(MainApp.run)