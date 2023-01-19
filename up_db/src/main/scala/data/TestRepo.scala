package data

import common.types.{SessionId, TestInRepo, TestModelRepo}
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

  /**
   * Return list of tests from concrete TestModelRepo by sid.
   * For output in div.test_list
  */
  def testsList(sid: SessionId): UIO[Option[List[TestInRepo]]]

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

  def testsList(sid: SessionId): UIO[Option[List[TestInRepo]]] = for {
    test <- lookup(sid)
    tests = test.flatMap(tst => tst.tests)
  } yield tests

}

object ImplTestsRepo {
  def layer: ZLayer[Any, Nothing, ImplTestsRepo] =
    ZLayer.fromZIO(
      Ref.make(mutable.Map.empty[SessionId, TestModelRepo]).map(new ImplTestsRepo(_))
    )

}