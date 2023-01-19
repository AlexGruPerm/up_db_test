package common

import tmodel.{CallType, RetType, SucCondElement, Test, TestModel, TestState, TestsMeta, undefined}

object types {
  type SessionId = String

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
  }//getOrElse(List[Test]())

}