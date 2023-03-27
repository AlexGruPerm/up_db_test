package common

import tmodel.{SucCondElement, Test, TestModel, TestState, TestsMeta, exec_exception,
  fields_exists, testStateFailure, testStateSuccess, testStateUndefined}
import zio.http.html.{pre, td, _}
import _root_.data.TestRepoTypes.TestID
import common.types.Columns

  object types {
    type SessionId = String
    type ColumnName = String
    type ColumnType = String
    type Column = (ColumnName, ColumnType)
    type Columns = IndexedSeq[Column]
    type ListRows = List[IndexedSeq[String]]
  }

  case class CallTimings(tBegin: Long, tExec: Long, tFetch: Long)

  case class TestExecutionResult(totalMs: Long,
                                 fetchMs: Long,
                                 execMs: Long,
                                 cols : Columns,
                                 rowCount: Int,
                                 err: Option[TestExecutionException] = None)

  object TestExecutionResult {
    def apply(): TestExecutionResult =
      TestExecutionResult(0L, 0L, 0L, IndexedSeq[(String, String)](), 0)

    def apply(excType:String, errMsg: String): TestExecutionResult =
      TestExecutionResult(0L, 0L, 0L, IndexedSeq[(String, String)](), 0,
        err = Some(TestExecutionException(excType,errMsg)))

    def apply(timings: CallTimings, cols: Columns, rowCount: Int): TestExecutionResult =
      TestExecutionResult(timings.tFetch - timings.tBegin, timings.tFetch - timings.tExec,
        timings.tExec - timings.tBegin, cols, rowCount)

  }

  case class TestInRepo(test: Test,
                        testState: TestState = testStateUndefined,
                        isExecuted: Boolean = false,
                        testRes: TestExecutionResult,
                        countOfExecuted: Int = 0) {

    private def getState(checked_success_condition: Option[List[SucCondElement]]): TestState =
      checked_success_condition match {
        case Some(listSucCond) =>
          if (listSucCond.exists(_.conditionResult.getOrElse(false) == false))
            testStateFailure
          else testStateSuccess
        case None => testStateUndefined
      }

    /**
     * Used for analyze executions results in testRes
     * with test's conditions (success_condition).
     */
    def checkConditions: TestInRepo = {
      val checked_success_conditions: Option[List[SucCondElement]] = {
       test.success_condition.map(lst => lst.map(sc => sc.check(testRes)))
      }
      val newTestState: TestState = getState(checked_success_conditions)
      println(s"checkConditions newTestState=${newTestState}")
      this.copy(test = test.copy(success_condition = checked_success_conditions),testState = newTestState,
        testRes = this.testRes)
    }

    def getTestAsHtml: Html =
      div(
         table(
           borderAttr := "1px solid black",
           css := (if (testState == testStateFailure)
                     "test_state_failure" :: Nil
                   else if (testState == testStateSuccess)
                     "test_state_success" :: Nil
                   else
                     "test_state_undef" :: Nil),
           idAttr := s"table_test_$id",
           testRes.err match {
             case Some(err) =>
               tr(bgColorAttr := "#FF4500;",
                 td(
                   colSpanAttr:= "2",
                   pre(wrapAttr:= "pre-wrap", widthAttr := "200px", s"TYPE = ${err.exceptionType}"),
                   pre(wrapAttr:= "pre-wrap", widthAttr := "200px", err.exceptionMsg),
                   (if ((err.exceptionMsg contains "MESSAGE TEXT:") &&
                     (err.exceptionMsg contains "CONTEXT")
                   ) {
                     pre(wrapAttr := "pre-wrap", widthAttr := "200px", err.exceptionMsg.substring(
                       err.exceptionMsg.indexOf("MESSAGE TEXT:") + 14,
                       err.exceptionMsg.indexOf("CONTEXT")
                     ))
                   })
                 ))
             case None => br()
           },
          tr(
            td(
              colSpanAttr:= "2",
              pre(wrapAttr:= "pre-wrap", widthAttr := "200px", test.call)
            )),
           tr(
             td(
               colSpanAttr:= "2",
               div(
                 s"[${test.id}] ${test.name}"
               ),br()
             )
           ),
           tr(
             td(
               colSpanAttr:= "2",
               div(
                 s"Test was executed $countOfExecuted times."
               ),br()
             )
           ),
           tr(td("Call type :"),td(test.call_type.toString)),
           tr(td("Return type :"),td(test.ret_type.toString)),
           if (test.use_commit.getOrElse(false))
             tr(bgColorAttr := "#F9E79F",
               td(
                 colSpanAttr:= "2",
                 div(
                   "Commit"
                 )
               )
             ) else ()
           ,
           tr(td(colSpanAttr:= "2",div("Call :"))),
           tr(td(colSpanAttr:= "2", pre(wrapAttr:= "pre-wrap", widthAttr := "200px", test.call) )),
           tr(td(colSpanAttr:= "2","Success conditions:")),
           //table with success conditions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           tr(td(colSpanAttr:= "2",
             //condition: SucCond, checkValue: Int, execResultValue: Option[Int], conditionResult: Option[Boolean]
               table(
                 borderAttr := "1px",
                 idAttr := s"table_test_conditions_$id",
                 tr(
                   td("Exec value"),
                   td("Condition"),
                   td("Check value"),
                   td("Result")
                 ),
                 test.success_condition.getOrElse(List[SucCondElement]()).map {sc =>
                   tr(bgColorAttr := (if (sc.conditionResult.getOrElse(false))
                                       "#228B22;"
                                      else
                                       "#FF4500;"),
                     td(
                       sc.condition match {
                         case _:fields_exists.type => testRes.cols.map(_._1).mkString("</br>")
                         case _:exec_exception.type =>
                           testRes.err.fold("no exception but expected")(_ => "exception exist, it's OK")
                         case _ => sc.execResultValue.getOrElse(0).toString
                       }
                     ),
                     td(sc.condition.toString),
                     td(
                       sc.condition match {
                         case _:fields_exists.type => sc.fields.getOrElse(List[String]()).mkString("</br>")
                         case _:exec_exception.type => "true"
                         case _ => sc.checkValue.toString
                       }
                     ),
                     td(sc.conditionResult.getOrElse(false).toString)
                   )
                 }
               )
           ))
           // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         )
      )
  }

  case class TestModelRepo(meta: TestsMeta, optListTestInRepo: Option[List[TestInRepo]]){

    private def uncheckConditions(success_condition: Option[List[SucCondElement]]): Option[List[SucCondElement]] =
      success_condition.fold(Some(List[SucCondElement]())){
        listSucCond => Some(listSucCond.map(sc => sc.uncheck()))
      }

    //todo: In all 3 methods don't do update in case of test in testInRepo not affected
    def enableOneTest(testId: TestID): TestModelRepo = {
      val updatedTests: Option[List[TestInRepo]] = optListTestInRepo.map{
        tr => tr.map {t=>
          if (t.test.id == testId)
            t.copy(test = t.test.copy(isEnabled = true))
          else
            t
        }}
      this.copy(optListTestInRepo = updatedTests)
    }

    //todo: Need refactoring to eliminate if
    def updateOneTest(testWithResults: TestInRepo): TestModelRepo ={
      val updatedTests: Option[List[TestInRepo]] = optListTestInRepo.map{
        listTestsInRepo => listTestsInRepo.collect { case testInRepo =>
          if (testInRepo.test.id == testWithResults.test.id)
            testWithResults
          else
            testInRepo
        }}
      this.copy(optListTestInRepo = updatedTests)
    }

  /*
    def updateOneTest(testWithResults: TestInRepo): TestModelRepo ={
      val updatedTests: Option[List[TestInRepo]] = optListTestInRepo.map{
        listTestsInRepo => listTestsInRepo.collect { case testInRepo =>
          if (testInRepo.test.id == testWithResults.test.id)
            testWithResults
          else
            testInRepo
        }}
      this.copy(optListTestInRepo = updatedTests)
    }
*/

    def disableOneTest(testId: TestID): TestModelRepo = {
      val updatedTests: Option[List[TestInRepo]] = optListTestInRepo.map{
        tr => tr.map {t=>
          if (t.test.id == testId)
            t.copy(test = t.test.copy(isEnabled = false,success_condition = uncheckConditions(t.test.success_condition)),
              isExecuted = false,
              testState = testStateUndefined,
              testRes = TestExecutionResult()
            )
          else
            t
        }}
      this.copy(optListTestInRepo = updatedTests)
    }

  }

  object TestModelRepo {

    def apply(tm: TestModel) : TestModelRepo = {
      val testsInRepo: Option[List[TestInRepo]] = tm.tests.map{tst => tst.map{test =>
        TestInRepo(test,
          testStateUndefined ,
          testRes = TestExecutionResult(0L, 0L, 0L, IndexedSeq[(String, String)](), 0))
      }}
      TestModelRepo(tm.meta, testsInRepo)
    }

  }

  case class TestExecutionException(exceptionType: String, exceptionMsg: String)




