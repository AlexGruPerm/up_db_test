package data

import common.types.{SessionId, TestModelRepo}
import tmodel.TestModel
import scala.collection.mutable
import zio._

trait TestsRepo {
  /**
   * Create new Tests set in repo and return SessionId
  */
  def create(testModel: TestModel) :Task[SessionId]

  def lookup(sid: SessionId): UIO[Option[TestModelRepo]]

  def elementsCnt :UIO[Int]

}

case class ImplTestsRepo(ref: Ref[mutable.Map[SessionId, TestModelRepo]]) extends TestsRepo {
  def create(testModel: TestModel) :Task[SessionId] = for {
    sid <- Random.nextUUID.map(_.toString)
    _ <- ZIO.logInfo(s"ImplTestsRepo.create generated sid = $sid")
    _ <- ref.update(test => test + (sid -> TestModelRepo(testModel)))
    _ <- ZIO.logInfo(s"ref COUNT = ${ref.get.map(_.size)}")
  } yield sid

  def lookup(sid: SessionId): UIO[Option[TestModelRepo]] =
    ref.get.map(_.get(sid))

  def elementsCnt :UIO[Int] = ref.get.map(_.size)

}

object ImplTestsRepo {
  def layer: ZLayer[Any, Nothing, ImplTestsRepo] =
    ZLayer.fromZIO(
      Ref.make(mutable.Map.empty[SessionId, TestModelRepo]).map(new ImplTestsRepo(_))
    )

}