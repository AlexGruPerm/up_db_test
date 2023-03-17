package common

import tmodel.{CallType, RetType, SucCondElement, TestModel, TestState, TestsMeta, exec_exception, fields_exists, testStateFailure, testStateSuccess, testStateUndefined}
import zio.http.html.{pre, td, _}
import zio.http.{Handler, Response}
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

import scala.::

object types {
  type SessionId = String

  type ColumnName = String
  type ColumnType = String
  type Column = (ColumnName,ColumnType)
  type Columns = IndexedSeq[Column]
  type ListRows = List[IndexedSeq[String]]

  case class CallTimings(tBegin: Long, tExec: Long, tFetch: Long)

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
                         use_commit: Option[Boolean],
                         call: String,
                         success_condition: Option[List[SucCondElement]],
                         isEnabled: Boolean,
                         testState: TestState = testStateUndefined,
                         isExecuted: Boolean = false,
                         testRes: TestExecutionResult,
                         countOfExecuted: Int = 0
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
               /*
               if (success_condition.getOrElse(List[SucCondElement]()).exists(sc => sc.condition match {
                 case _:exec_exception.type => true
                 case _ => false
               })) {
                 tr(bgColorAttr := "#FF45FF;",
                   td(
                     colSpanAttr:= "2",
                     pre(wrapAttr:= "pre-wrap", widthAttr := "200px", s"TYPE = ${err.exceptionType}"),
                     pre(wrapAttr:= "pre-wrap", widthAttr := "200px", err.exceptionMsg),
                     pre(wrapAttr:= "pre-wrap", widthAttr := "200px", err.exceptionMsg.substring(
                       err.exceptionMsg.indexOf("MESSAGE TEXT:")+14,
                       err.exceptionMsg.indexOf("CONTEXT")
                     ))
                   ))
               } else {
                 tr(bgColorAttr := "#FF4500;",
                 td(
                   colSpanAttr:= "2",
                     pre(wrapAttr:= "pre-wrap", widthAttr := "200px", s"TYPE = ${err.exceptionType}"),
                     pre(wrapAttr:= "pre-wrap", widthAttr := "200px", err.exceptionMsg),
                     pre(wrapAttr:= "pre-wrap", widthAttr := "200px", err.exceptionMsg.substring(
                       err.exceptionMsg.indexOf("MESSAGE TEXT:")+14,
                       err.exceptionMsg.indexOf("CONTEXT")
                     ))
                 ))
               }*/
             case None => br()
           },
          tr(
            td(
              colSpanAttr:= "2",
              pre(wrapAttr:= "pre-wrap", widthAttr := "200px", call)
            )),
           /*
           testRes.err match {
             case Some(_) =>
               tr(bgColorAttr := "#FF4500;",
                 td(
                   colSpanAttr:= "2",
                     pre(wrapAttr:= "pre-wrap", widthAttr := "200px", call)
                 ))
             case None => br()
           },
           */
           tr(
             td(
               colSpanAttr:= "2",
               div(
                 s"[$id] $name"
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
           tr(td("Call type :"),td(call_type.toString)),
           tr(td("Return type :"),td(ret_type.toString)),
           if (use_commit.getOrElse(false))
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
           tr(td(colSpanAttr:= "2", pre(wrapAttr:= "pre-wrap", widthAttr := "200px", call) )),
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
                     td(
                       sc.condition match {
                         case _:fields_exists.type => testRes.cols.map(_._1).mkString("</br>")
/*                         case _:exec_exception.type =>
                           testRes.err match {
                             case Some(_) => "exception exist"
                             case None => "no exception"
                           }*/
                         case _ => sc.execResultValue.getOrElse(0).toString
                       }

                     ),
                     td(sc.condition.toString),
                     td(
                       sc.condition match {
                         case _:fields_exists.type => sc.fields.getOrElse(List[String]()).mkString("</br>")
/*                         case _:exec_exception.type => sc.is_exists match {
                           case Some(v) => v.toString
                           case None => "-"
                         }*/
                         case _ => sc.checkValue.toString
                       }
                     ),
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
        TestInRepo(t.id, t.name, t.call_type, t.ret_type, t.use_commit, t.call, t.success_condition, t.isEnabled, testStateUndefined , isExecuted= false,
          testRes = TestExecutionResult(0L, 0L, 0L, IndexedSeq[(String, String)](), 0))
      }}
      TestModelRepo(tm.meta, testsInRepo)
    }

  }

  case class TestExecutionException(exceptionType: String, exceptionMsg: String)

  case class TestExecutionResult(totalMs: Long, fetchMs: Long, execMs: Long, cols : Columns, rowCount: Int, err: Option[TestExecutionException] = None)

  object TestExecutionResult {
    def apply(): TestExecutionResult =
      TestExecutionResult(0L, 0L, 0L, IndexedSeq[(String, String)](), 0)

    def apply(excType:String, errMsg: String): TestExecutionResult =
      TestExecutionResult(0L, 0L, 0L, IndexedSeq[(String, String)](), 0,err = Some(TestExecutionException(excType,errMsg)))

    def apply(timings: CallTimings, cols: Columns, rowCount: Int): TestExecutionResult =
      TestExecutionResult(timings.tFetch - timings.tBegin, timings.tFetch - timings.tExec, timings.tExec - timings.tBegin, cols, rowCount)

  }

}
