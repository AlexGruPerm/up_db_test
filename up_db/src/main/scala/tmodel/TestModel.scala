package tmodel

import common.types.{TestExecutionException, TestExecutionResult, TestInRepo}
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
  case object select extends CallType{
    override def toString: String = "select"
  }
  case object function extends CallType{
    override def toString: String = "function"
  }
  case object select_function extends CallType{
    override def toString: String = "select_function"
  }
  case object func_inout_cursor extends CallType{
    override def toString: String = "func_inout_cursor"
  }

sealed trait RetType
  case object cursor extends RetType{
    override def toString: String = "cursor"
  }
  case object dataset extends RetType{
    override def toString: String = "dataset"
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
  case object fields_exists  extends SucCond{
    override def toString: String = "fields exists "
  }
  //case object no_exception extends SucCond("no_exception")
  //case object exception extends SucCond("exception")

sealed trait TestState
  case object testStateUndefined extends TestState
  case object testStateSuccess extends TestState
  case object testStateFailure extends  TestState
  case object testStateExecuting extends TestState


  //todo: late change Int to Long
  case class SucCondElement(condition: SucCond, checkValue: Option[Int], fields: Option[List[String]],
                            execResultValue: Option[Long], conditionResult: Option[Boolean]){

    def check(testRes: TestExecutionResult):SucCondElement = {
      val (checkConditionRes,testResVal): (Boolean,Option[Long]) =
        condition match {
        case _:rows_gt.type => (checkValue.getOrElse(0) < testRes.rowCount,Some(testRes.rowCount))
        case _:rows_lt.type => (checkValue.getOrElse(0) > testRes.rowCount,Some(testRes.rowCount))
        case _:rows_eq.type => (checkValue.getOrElse(0) == testRes.rowCount,Some(testRes.rowCount))
        case _:rows_ne.type => (checkValue.getOrElse(0) != testRes.rowCount,Some(testRes.rowCount))
        case _:exec_time_ms.type  => (testRes.execMs <= checkValue.getOrElse(0),Some(testRes.execMs))
        case _:fetch_time_ms.type => (testRes.fetchMs <= checkValue.getOrElse(0),Some(testRes.fetchMs))
        case _:full_time_ms.type  => (testRes.totalMs <= checkValue.getOrElse(0),Some(testRes.totalMs))
        // ._1 - column name, _.2 - column type
        case _:fields_exists.type  => (fields.getOrElse(List[String]()).forall(testRes.cols.map(cls => cls._1).contains),Some(1))
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
                   use_commit: Option[Boolean],
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

    implicit val encoderTestExecutionException: JsonEncoder[TestExecutionException] = DeriveJsonEncoder.gen[TestExecutionException]
    implicit val decoderTestExecutionException: JsonDecoder[TestExecutionException] = DeriveJsonDecoder.gen[TestExecutionException]

    implicit val encoderCallType: JsonEncoder[CallType] = DeriveJsonEncoder.gen[CallType]
    implicit val decoder: JsonDecoder[CallType] = JsonDecoder[String].map {
      case "select" => select
      case "function" => function
      case "select_function" => select_function
      case "func_inout_cursor" => func_inout_cursor
      case anyValue => throw new Exception(s"Invalid value in field call_type = $anyValue")
    }

    implicit val encoderRetType: JsonEncoder[RetType] = DeriveJsonEncoder.gen[RetType]
    implicit val decoderRetType: JsonDecoder[RetType] = JsonDecoder[String].map {
      case "cursor" => cursor
      case "dataset" => dataset
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
      case "fields_exists" => fields_exists
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

