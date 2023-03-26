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
    _ <- ref.update(test => test.concat(List(sid -> TestModelRepo(testModel))))
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

  def checkTestRepoData(sid: SessionId): UIO[Option[checkTestRepoInfo]] = for {
    tests <- lookup(sid)
    res = tests.map(v => v.tests.fold(TestsStatus.undefined)(TestsStatus.calculated).getCheckTestRepoInfo)
    res <- ZIO.succeed(res)
  } yield res

  def updateTestWithResults(sid: SessionId, testWithResults: TestInRepo): UIO[Unit] = for {
    test <- lookup(sid)
    _ <- test.fold(ZIO.unit){testsSet =>
      ref.update{
        tests => tests.concat(
          List(sid -> TestModelRepo(testsSet.meta,testsSet.tests).updateOneTest(testWithResults))
        )
      }
    }
  } yield ()

  def enableTest(sid: SessionId, testId: Int): UIO[Unit] = for {
    test <- lookup(sid)
    _ <- test match {
      case Some(s) =>
        ref.update{
          tests => tests.concat(
            List(sid -> TestModelRepo(
              s.meta,
            s.tests.map{
              tr => tr.map{t =>
                if (t.id == testId)
                  t.copy(isEnabled = true)
                else
                  t
              }
            }
          )))}
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
          tests => tests.concat(
            List(sid -> TestModelRepo(
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
            )))}
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