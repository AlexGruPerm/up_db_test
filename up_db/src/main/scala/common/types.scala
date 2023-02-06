package common

import tmodel.{CallType, RetType, SucCondElement, TestModel, TestState, TestsMeta, undefined}
import zio.http.html.{_}
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
                   testState: TestState = undefined,
                   isExecuted: Boolean = false,
                   testRes: TestExecutionResult
                 ){
    def getTestAsHtml : Html =
        div(
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

      )
  }

  case class TestModelRepo( meta: TestsMeta, tests: Option[List[TestInRepo]])

  object TestModelRepo {
    def apply(tm: TestModel) : TestModelRepo = {
      val testsInRepo: Option[List[TestInRepo]] = tm.tests.map{tst => tst.map{t =>
        TestInRepo(t.id, t.name, t.call_type, t.ret_type, t.call, t.success_condition, t.isEnabled, undefined , isExecuted= false,
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
