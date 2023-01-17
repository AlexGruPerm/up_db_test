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

}
/*
case class TestRepo() extends TestRepo{
  def create(testModel: TestModel) : ZIO[TestRepo, Throwable, String]  =
    ZIO.serviceWithZIO[TestRepo](_.create(testModel))

  def lookup(sid: SessionId): ZIO[TestRepo, Throwable, Option[TestModelRepo]] =
    ZIO.serviceWithZIO[TestRepo](_.lookup(sid))
}*/

case class ImplTestsRepo(ref: Ref[mutable.Map[SessionId, TestModelRepo]]) extends TestsRepo {
  def create(testModel: TestModel) :Task[SessionId] = for {
    sid <- Random.nextUUID.map(_.toString)
    _ <- ref.update(test => test + (sid -> TestModelRepo(testModel)))
  } yield sid

  def lookup(sid: SessionId): UIO[Option[TestModelRepo]] = {
    ref.get.map(_.get(sid))
  }

}

object ImplTestsRepo {
  def layer: ZLayer[Any, Nothing, ImplTestsRepo] =
    ZLayer.fromZIO(
      Ref.make(mutable.Map.empty[SessionId, TestModelRepo]).map(new ImplTestsRepo(_))
    )

}