package tmodel

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
  case object procedure extends CallType
  case object function extends CallType

sealed trait RetType
  case object cursor extends RetType
  case object select_dataset extends RetType
  case object integer_value extends RetType

sealed trait SucCond
  case object rows_gt extends SucCond
  //case object rows_eq extends SucCond("rows_eq")
  case object exec_time_ms  extends SucCond
  case object fetch_time_ms extends SucCond
  case object full_time_ms  extends SucCond
  //case object no_exception extends SucCond("no_exception")
  //case object exception extends SucCond("exception")

sealed trait TestState
  case object undefined extends TestState
  case object success extends TestState
  case object failure extends  TestState
  case object executing extends TestState


/*
  execResultValue: row count, scalar value (Int,String,....), is_exception = 0,1, execution_Time
  */
  case class SucCondElement(condition: SucCond, checkValue: Int, execResultValue: Option[Int], conditionResult: Option[Boolean])


  case class TestsMeta(connect_ip: String,
                       db_name: String,
                       db_user: String,
                       db_password: String) {
     val driver = "org.postgresql.Driver"
     def url: String =
        s"jdbc:postgresql://$connect_ip/$db_name?user=$db_user&password=$db_password"
  }

  case class Test(
                   id: Int,
                   name: String,
                   call_type: CallType,
                   ret_type: RetType,
                   call: String,
                   success_condition: Option[List[SucCondElement]],
                   isEnabled: Boolean = true
                 )

  case class TestModel(meta: TestsMeta, tests: Option[List[Test]]) {
      val listID : List[Int] = tests.getOrElse(List[Test]()).map(t => t.id)
      if (listID.distinct.size != listID.size)
        throw new Exception("Not unique id in tests. Must be unique.")
  }

  object EncDecTestModelImplicits{

    implicit val encoderCallType: JsonEncoder[CallType] = DeriveJsonEncoder.gen[CallType]
    implicit val decoder: JsonDecoder[CallType] = JsonDecoder[String].map {
      case "procedure" => procedure
      case "function" => function
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

  }
