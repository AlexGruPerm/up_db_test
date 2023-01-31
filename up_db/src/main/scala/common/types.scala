package common

import tmodel.{CallType, RetType, SucCondElement, TestModel, TestState, TestsMeta, undefined}

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
                   testState: TestState = undefined
                 )

  case class TestModelRepo( meta: TestsMeta, tests: Option[List[TestInRepo]])

  object TestModelRepo {
    def apply(tm: TestModel) : TestModelRepo = {
      val testsInRepo: Option[List[TestInRepo]] = tm.tests.map{tst => tst.map{t =>
        TestInRepo(t.id, t.name, t.call_type, t.ret_type, t.call, t.success_condition, t.isEnabled, undefined )
      }}
      TestModelRepo(tm.meta, testsInRepo)
    }
  }

  case class TestExecutionResult(totalMs: Long, fetchMs: Long, execMs: Long, cols : List[(String,String)], colSize: Int, rowCount: Int)

}
