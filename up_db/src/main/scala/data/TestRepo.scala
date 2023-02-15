package data

import common.types.{SessionId, TestExecutionResult, TestInRepo, TestModelRepo}
import tmodel.{SucCondElement, TestModel, testStateFailure, testStateSuccess, testStateUndefined}

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
  def checkTestRepoData(sid: SessionId): UIO[Option[checkTestRepoInfo]]

  /**
   * Update one test in TestModelRepo, test must be with execution results - look TestExecutionResult
  */
  def updateTestWithResults(sid: SessionId, testWithResults: TestInRepo): UIO[Unit]

  /**
   * Enable one test in the tests set identified by sid.
  */
  def enableTest(sid: SessionId, id: Int):  UIO[Unit]

  /**
   * Disable one test in the tests set identified by sid.
   */
  def disableTest(sid: SessionId, id: Int): UIO[Unit]

  /**
   * Disable all tests in the tests set identified by sid.
   */
  def disableAllTestAndClearExecRes(sid: SessionId): UIO[Unit]

  def testsListEnabled(sid: SessionId): UIO[Option[List[TestInRepo]]]

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

  def testsListEnabled(sid: SessionId): UIO[Option[List[TestInRepo]]] = for {
    test <- lookup(sid)
    tests = test.flatMap(olt => olt.tests.map(t => t.filter(_.isEnabled==true)))
  } yield tests

  //todo: eliminate v.tests.getOrElse(List[TestInRepo]())
  def checkTestRepoData(sid: SessionId): UIO[Option[checkTestRepoInfo]] = for {
    tests <- lookup(sid)
    res = tests.map{v =>
      checkTestRepoInfo(TestsStatus(
        v.tests.getOrElse(List[TestInRepo]()).size,
        v.tests.getOrElse(List[TestInRepo]()).count(t => t.isEnabled),
        v.tests.getOrElse(List[TestInRepo]()).count(t => !t.isEnabled),
        v.tests.getOrElse(List[TestInRepo]()).count(t => t.isExecuted),
        v.tests.getOrElse(List[TestInRepo]()).count(t => t.testState == testStateSuccess),
        v.tests.getOrElse(List[TestInRepo]()).count(t => t.testState == testStateFailure)
       ))
      }
    res <- ZIO.succeed(res)

    /*    lst <- ref.get.map { m => m.map {
      case (k, v) =>
        (k,
          TestsStatus(
            v.tests.getOrElse(List[TestInRepo]()).size,
            v.tests.getOrElse(List[TestInRepo]()).count(t => t.isEnabled),
            v.tests.getOrElse(List[TestInRepo]()).count(t => !t.isEnabled),
            v.tests.getOrElse(List[TestInRepo]()).count(t => t.isExecuted),
            v.tests.getOrElse(List[TestInRepo]()).count(t => t.testState == testStateSuccess),
            v.tests.getOrElse(List[TestInRepo]()).count(t => t.testState == testStateFailure)
            ))
     }(collection.breakOut).toList
    }
   res <- ZIO.succeed(Some(checkTestRepoInfo(lst)))*/
  } yield res


  def updateTestWithResults(sid: SessionId, testWithResults: TestInRepo): UIO[Unit] = for {
    test <- lookup(sid)
    _ <- test match {
      case Some(s) =>
        ref.update{
          tests => tests +
            (sid -> TestModelRepo(
              s.meta,
              s.tests.map{
                tr => tr.map{t =>
                  if (t.id == testWithResults.id)
                    testWithResults
                  else
                    t
                }
              }
            ))}
      case None => ZIO.unit
    }
  } yield ()

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
  } yield ()

  def uncheckConditions(success_condition: Option[List[SucCondElement]]): Option[List[SucCondElement]] =
    success_condition.fold(Some(List[SucCondElement]())){
      listSucCond => Some(listSucCond.map(sc => sc.uncheck()))
    }

  def disableTest(sid: SessionId, testId: Int): UIO[Unit] = for {
    test <- lookup(sid)
    _ <- test match {
      case Some(s) =>
        //ZIO.logInfo(s"~~~DISABLE~~  ${testId}  ~~~~~~~~~~~~~~~~~~~~~~~") *>
        ref.update{
          tests => tests +
            (sid -> TestModelRepo(
              s.meta,
              s.tests.map{
                tr => tr.map{t =>
                  if (t.id == testId)
                    t.copy(isEnabled = false,
                      isExecuted = false,
                      testState = testStateUndefined,
                      success_condition = uncheckConditions(t.success_condition),
                      testRes = TestExecutionResult())
                  else
                    t
                }
              }
            ))}
      case None => ZIO.unit
    }
  } yield ()

  def disableAllTestAndClearExecRes(sid: SessionId): UIO[Unit] = for {
    testModelRepo <- lookup(sid)
    //_ <- ZIO.logInfo(s"begin disable tests for sid=${sid}")
    _ <- testModelRepo match {
      case Some(test) => ZIO.foreachDiscard(test.tests.getOrElse(List[TestInRepo]())){t => disableTest(sid,t.id)
      }
      case None => ZIO.unit
    }
  } yield ()

}

object ImplTestsRepo {
  def layer: ZLayer[Any, Nothing, ImplTestsRepo] =
    ZLayer.fromZIO(
      Ref.make(mutable.Map.empty[SessionId, TestModelRepo]).map(new ImplTestsRepo(_))
    )

}