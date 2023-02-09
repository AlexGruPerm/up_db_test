package common

import tmodel.{CallType, RetType, SucCondElement, TestModel, TestState, TestsMeta, testStateFailure, testStateSuccess, testStateUndefined}
import zio.http.html._
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
      val checked_success_condition: Option[List[SucCondElement]] = {
        success_condition.map(lst => lst.map(sc => sc.check(testRes)))
      }
      val newTestState: TestState = getState(checked_success_condition)
      this.copy(success_condition = checked_success_condition,testState = newTestState)
    }




    //  case class SucCondElement(condition: SucCond, checkValue: Int, execResultValue: Option[Int], conditionResult: Option[Boolean])
    //  case class TestExecutionResult(totalMs: Long, fetchMs: Long, execMs: Long, cols : List[(String,String)], rowCount: Int, errMsg: Option[String] = None)

    def getTestAsHtml: Html = div(s"[$id] $name") //.attr("id","div_1")
    /*        div(
         table(
           tr(
             td(
               div(
                 s"[$id] $name"
               ),br()
             )
           ),
           tr(),
           tr(),
           tr(),
           tr(),
           tr(),
           tr(),
           tr(),
           tr()
         )
      )*/
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
