package data

import common.types.{SessionId, TestInRepo, TestModelRepo}
import tmodel.TestModel

import scala.collection.mutable
import zio.{UIO, _}

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

  /**
   * Aggregated information for debug purpose
  */
  def checkTestRepoData: UIO[Option[checkTestRepoInfo]]

  /**
   * Enable one test in the tests set identified by sid.
  */
  def enableTest(sid: SessionId, id: Int):  UIO[Unit]

  /**
   * Disable one test in the tests set identified by sid.
   */
  def disableTest(sid: SessionId, id: Int): UIO[Unit]

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

  def checkTestRepoData: UIO[Option[checkTestRepoInfo]] = for {
    lst <- ref.get.map { m => m.map {
      case (k, v) =>
        (k,
          TestsStatus(
            v.tests.getOrElse(List[TestInRepo]()).size,
            v.tests.getOrElse(List[TestInRepo]()).count(t => t.isEnabled),
            v.tests.getOrElse(List[TestInRepo]()).count(t => !t.isEnabled)))
     }(collection.breakOut).toList
    }
   res <- ZIO.succeed(Some(checkTestRepoInfo(lst)))
  } yield res

  def enableTest(sid: SessionId, testId: Int): UIO[Unit] = for {
    test <- lookup(sid)
    _ <- test match {
      case Some(s) =>
        ref.update{
          tests => tests +
            (sid -> TestModelRepo(
              s.meta,
            s.tests.map{
              tr => tr.map{t =>
                if (t.id == testId)
                  t.copy(isEnabled = true)
                else
                  t
              }
            }
          ))}
      case None => ZIO.unit
    }
    //res <- ZIO.succeed(testId)
  } yield ()

  def disableTest(sid: SessionId, testId: Int): UIO[Unit] = for {
    test <- lookup(sid)
    _ <- test match {
      case Some(s) =>
        ref.update{
          tests => tests +
            (sid -> TestModelRepo(
              s.meta,
              s.tests.map{
                tr => tr.map{t =>
                  if (t.id == testId)
                    t.copy(isEnabled = false)
                  else
                    t
                }
              }
            ))}
      case None => ZIO.unit
    }
    //res <- ZIO.succeed(testId)
  } yield ()



    /*
        updatedTestModelRepo = test.map{otmr =>
      TestModelRepo(otmr,
        otmr.tests.map{tests =>
      tests.
      collect{
        case t if t.id == testId => t.copy(isEnabled = true)
        case t if t.id != testId => t
      })
    */

/*
    updatedTests = test.flatMap(
      testsList => testsList.tests.map{t =>
        t.filter(testInRep => testInRep.id == id).map(foundedTest => (sid,foundedTest.copy(isEnabled = true)))
        })
  */

  /*
        .map{testsInRepo => testsInRepo.tests.getOrElse(List[TestInRepo]()).
        .map{
          t => if (t.id == testId)
            (sid -> (testsInRepo.meta,t.copy(isEnabled = true)))
          else (sid -> (testsInRepo.meta,t))
        }
      }
  */

}

object ImplTestsRepo {
  def layer: ZLayer[Any, Nothing, ImplTestsRepo] =
    ZLayer.fromZIO(
      Ref.make(mutable.Map.empty[SessionId, TestModelRepo]).map(new ImplTestsRepo(_))
    )

}