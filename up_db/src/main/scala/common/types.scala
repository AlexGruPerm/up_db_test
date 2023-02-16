package common

import tmodel.{CallType, RetType, SucCondElement, TestModel, TestState, TestsMeta, testStateFailure, testStateSuccess, testStateUndefined}
import zio.http.html.{td, _}
import zio.http.{Handler, Response}
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

object types {
  type SessionId = String

  /**
   * success_condition - List of SucCondElement contains execution results in fields:
   * execResultValue: Option[Int]     For example, for "condition":"rows_eq",      "checkValue":12  execResultValue=Some(x)
   * conditionResult: Option[Boolean] is this condition successful.
  */
  case class TestInRepo(
                         id: Int,
                         name: String,
                         call_type: CallType,
                         ret_type: RetType,
                         call: String,
                         success_condition: Option[List[SucCondElement]],
                         isEnabled: Boolean,
                         testState: TestState = testStateUndefined,
                         isExecuted: Boolean = false,
                         testRes: TestExecutionResult
                 ) {

    private def getState(checked_success_condition: Option[List[SucCondElement]]): TestState = {
      val testState: TestState = checked_success_condition match {
        case Some(listSucCond) =>
          if (listSucCond.exists(_.conditionResult.getOrElse(false) == false))
            testStateFailure
          else testStateSuccess
        case None => testStateUndefined
      }
      testState
    }


    /**
     * Used for analyze executions results in testRes
     * with test's conditions (success_condition).
     */
    def checkConditions: TestInRepo = {
      val checked_success_conditions: Option[List[SucCondElement]] = {
        success_condition.map(lst => lst.map(sc => sc.check(testRes)))
      }
      val newTestState: TestState = getState(checked_success_conditions)
      this.copy(success_condition = checked_success_conditions,testState = newTestState, testRes = this.testRes)
    }




    //  case class SucCondElement(condition: SucCond, checkValue: Int, execResultValue: Option[Int], conditionResult: Option[Boolean])
    //  case class TestExecutionResult(totalMs: Long, fetchMs: Long, execMs: Long, cols : List[(String,String)], rowCount: Int, errMsg: Option[String] = None)

    def getTestAsHtml: Html = //div(s"[$id] $name")
      div(
         table(
           borderAttr := "1px",
           css := (if (testState == testStateFailure)
                     "test_state_failure" :: Nil
                   else if (testState == testStateSuccess)
                     "test_state_success" :: Nil
                   else
                     "test_state_undef" :: Nil),
           idAttr := s"table_test_$id",
           tr(
             td(
               colSpanAttr:= "2",
               div(
                 s"[$id] $name"
               ),br()
             )
           ),
           tr(td("Call type :"),td(call_type.toString)),
           tr(td("Return type :"),td(ret_type.toString)),
           tr(td(colSpanAttr:= "2",div("Call :"))),
           tr(td(colSpanAttr:= "2",call)),
           tr(td(colSpanAttr:= "2","Success conditions:")),
           tr(td(colSpanAttr:= "2",
             //table with success conditions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
                 success_condition.getOrElse(List[SucCondElement]()).map {sc =>
                   tr(bgColorAttr := (if (sc.conditionResult.getOrElse(false))
                                       "#228B22;"
                                      else
                                       "#FF4500;"),
                     td(sc.execResultValue.getOrElse(0).toString),
                     td(sc.condition.toString),
                     td(sc.checkValue.toString),
                     td(sc.conditionResult.getOrElse(false).toString)
                   )
                 }
               )
             // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ))//,
           //tr(td("111111"),td("2222222")),
           //tr(td("111111"),td("2222222"))
         )
      )
  }

  case class TestModelRepo( meta: TestsMeta, tests: Option[List[TestInRepo]])

  object TestModelRepo {
    def apply(tm: TestModel) : TestModelRepo = {
      val testsInRepo: Option[List[TestInRepo]] = tm.tests.map{tst => tst.map{t =>
        TestInRepo(t.id, t.name, t.call_type, t.ret_type, t.call, t.success_condition, t.isEnabled, testStateUndefined , isExecuted= false,
          testRes = TestExecutionResult(0L, 0L, 0L, List[(String, String)](), 0))
      }}
      TestModelRepo(tm.meta, testsInRepo)
    }

  }

  case class TestExecutionResult(totalMs: Long, fetchMs: Long, execMs: Long, cols : List[(String,String)], rowCount: Int, errMsg: Option[String] = None)

  object TestExecutionResult {
    def apply(): TestExecutionResult =
      TestExecutionResult(0L, 0L, 0L, List[(String, String)](), 0)

    def apply(errMsg: String): TestExecutionResult =
      TestExecutionResult(0L, 0L, 0L, List[(String, String)](), 0,errMsg = Some(errMsg))
  }

}
