package tmodel

import common.types.{TestExecutionResult, TestInRepo}
import zio.http.html.div
import zio.http.{Handler, Response}
import zio.{Random, ZIO}
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

import scala.annotation.nowarn

/*sealed abstract class CallType(@nowarn name: String)
  object CallType{
    case object procedure extends CallType("procedure")
    case object function extends CallType("function")
  }*/

/**
 * to produce JSON from our data we define a JsonEncoder
 * to parse JSON in our Types we use a JsonDecoder
*/

sealed trait CallType
  case object procedure extends CallType{
    override def toString: String = "procedure"
  }
  case object function extends CallType{
    override def toString: String = "function"
  }
  case object select_function extends CallType{
    override def toString: String = "select_function"
  }

sealed trait RetType
  case object cursor extends RetType{
    override def toString: String = "cursor"
  }
  case object select_dataset extends RetType{
    override def toString: String = "select_dataset"
  }
  case object integer_value extends RetType{
    override def toString: String = "integer_value"
  }

sealed trait SucCond
  case object rows_gt extends SucCond{
    override def toString: String = "rows >"
  }
  case object rows_lt extends SucCond{
    override def toString: String = "rows <"
  }
  case object rows_eq extends SucCond{
    override def toString: String = "rows ="
  }
  case object rows_ne extends SucCond{
    override def toString: String = "rows <> "
  }
  case object exec_time_ms  extends SucCond{
    override def toString: String = "Exec. time (ms.) < "
  }
  case object fetch_time_ms extends SucCond{
    override def toString: String = "Fetch time (ms.) < "
  }
  case object full_time_ms  extends SucCond{
    override def toString: String = "Full time (ms.) < "
  }
  //case object no_exception extends SucCond("no_exception")
  //case object exception extends SucCond("exception")

sealed trait TestState
  case object testStateUndefined extends TestState
  case object testStateSuccess extends TestState
  case object testStateFailure extends  TestState
  case object testStateExecuting extends TestState

//  case class SucCondElement(condition: SucCond, checkValue: Int, execResultValue: Option[Int], conditionResult: Option[Boolean])
//  case class TestExecutionResult(totalMs: Long, fetchMs: Long, execMs: Long, cols : List[(String,String)], rowCount: Int, errMsg: Option[String] = None)
/*
  execResultValue: row count, scalar value (Int,String,....), is_exception = 0,1, execution_Time
  */
  //todo: late change Int to Long
  case class SucCondElement(condition: SucCond, checkValue: Int, execResultValue: Option[Long], conditionResult: Option[Boolean]){

    def check(testRes: TestExecutionResult):SucCondElement = {
      val (checkConditionRes,testResVal): (Boolean,Option[Long]) =
        condition match {
        case _:rows_gt.type => (checkValue < testRes.rowCount,Some(testRes.rowCount))
        case _:rows_lt.type => (checkValue > testRes.rowCount,Some(testRes.rowCount))
        case _:rows_eq.type => (checkValue == testRes.rowCount,Some(testRes.rowCount))
        case _:rows_ne.type => (checkValue != testRes.rowCount,Some(testRes.rowCount))
        case _:exec_time_ms.type  => (testRes.execMs <= checkValue,Some(testRes.execMs))
        case _:fetch_time_ms.type => (testRes.fetchMs <= checkValue,Some(testRes.fetchMs))
        case _:full_time_ms.type  => (testRes.totalMs <= checkValue,Some(testRes.totalMs))
        }
      this.copy(execResultValue = testResVal, conditionResult = Some(checkConditionRes))
    }

    /**
     * remove information about execResultValue and conditionResult
    */
    def uncheck():SucCondElement = {
      this.copy(execResultValue = None, conditionResult = None)
    }

  }


  case class TestsMeta(connect_ip: String,
                       db_name: String,
                       db_user: String,
                       db_password: String) {
     val driver = "org.postgresql.Driver"
     def url: String =  s"jdbc:postgresql://$connect_ip/$db_name?user=$db_user&password=$db_password"
     def urlMsg: String =  s"jdbc:postgresql://$connect_ip/$db_name?user=*****&password=*****"

  }

  case class Test(
                   id: Int,
                   name: String,
                   call_type: CallType,
                   ret_type: RetType,
                   call: String,
                   success_condition: Option[List[SucCondElement]],
                   isEnabled: Boolean = false
                 )

  case class TestModel(meta: TestsMeta, tests: Option[List[Test]]) {
      val listID : List[Int] = tests.getOrElse(List[Test]()).map(t => t.id)
      if (listID.distinct.size != listID.size)
        throw new Exception("Not unique id in tests. Must be unique.")
  }

  case class TestsToRun(sid: String, ids: Option[List[Int]])

  object EncDecTestModelImplicits{

    implicit val encoderCallType: JsonEncoder[CallType] = DeriveJsonEncoder.gen[CallType]
    implicit val decoder: JsonDecoder[CallType] = JsonDecoder[String].map {
      case "procedure" => procedure
      case "function" => function
      case "select_function" => select_function
      case anyValue => throw new Exception(s"Invalid value in field call_type = $anyValue")
    }

    implicit val encoderRetType: JsonEncoder[RetType] = DeriveJsonEncoder.gen[RetType]
    implicit val decoderRetType: JsonDecoder[RetType] = JsonDecoder[String].map {
      case "cursor" => cursor
      case "select_dataset" => select_dataset
      case "integer_value" => integer_value
      case anyValue => throw new Exception(s"Invalid value in field ret_type = $anyValue")
    }

    implicit val encoderSucCond: JsonEncoder[SucCond] = DeriveJsonEncoder.gen[SucCond]
    implicit val decoderSucCond: JsonDecoder[SucCond] = JsonDecoder[String].map {
      case "rows_gt" => rows_gt
      case "rows_lt" => rows_lt
      case "rows_eq" => rows_eq
      case "rows_ne" => rows_ne
      case "exec_time_ms" => exec_time_ms
      case "fetch_time_ms" => fetch_time_ms
      case "full_time_ms" => full_time_ms
      case anyValue => throw new Exception(s"Invalid value in field inside success_condition = $anyValue")
    }

    implicit val encoderSucCondElement: JsonEncoder[SucCondElement] = DeriveJsonEncoder.gen[SucCondElement]
    implicit val decoderSucCondElement: JsonDecoder[SucCondElement] = DeriveJsonDecoder.gen[SucCondElement]

    implicit val encoderTestsMeta: JsonEncoder[TestsMeta] = DeriveJsonEncoder.gen[TestsMeta]
    implicit val decoderTestsMeta: JsonDecoder[TestsMeta] = DeriveJsonDecoder.gen[TestsMeta]

    implicit val encoderTest: JsonEncoder[Test] = DeriveJsonEncoder.gen[Test]
    implicit val decoderTest: JsonDecoder[Test] = DeriveJsonDecoder.gen[Test]

    implicit val encoderTestModel: JsonEncoder[TestModel] = DeriveJsonEncoder.gen[TestModel]
    implicit val decoderTestModel: JsonDecoder[TestModel] = DeriveJsonDecoder.gen[TestModel]

    implicit val encoderTestsToRun: JsonEncoder[TestsToRun] = DeriveJsonEncoder.gen[TestsToRun]
    implicit val decoderTestsToRun: JsonDecoder[TestsToRun] = DeriveJsonDecoder.gen[TestsToRun]

    implicit val encoderTestState: JsonEncoder[TestState] = DeriveJsonEncoder.gen[TestState]

    implicit val decoderTestState: JsonDecoder[TestState] = JsonDecoder[String].map {
      case "testStateUndefined" => testStateUndefined
      case "testStateSuccess" => testStateSuccess
      case "testStateFailure" => testStateFailure
      case "testStateExecuting" => testStateExecuting
      case anyValue => throw new Exception(s"Invalid value in field call_type = $anyValue")
    }

    implicit val encoderTestExecutionResult: JsonEncoder[TestExecutionResult] = DeriveJsonEncoder.gen[TestExecutionResult]
    implicit val decoderTestExecutionResult: JsonDecoder[TestExecutionResult] = DeriveJsonDecoder.gen[TestExecutionResult]

    implicit val encoderTestInRepo: JsonEncoder[TestInRepo] = DeriveJsonEncoder.gen[TestInRepo]
    implicit val decoderTestInRepo: JsonDecoder[TestInRepo] = DeriveJsonDecoder.gen[TestInRepo]

  }

