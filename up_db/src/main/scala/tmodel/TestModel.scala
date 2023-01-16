package tmodel

import zio.ZIO
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

import scala.annotation.nowarn

/*sealed abstract class CallType(@nowarn name: String)
  object CallType{
    case object procedure extends CallType("procedure")
    case object function extends CallType("function")
  }*/

sealed trait CallType
  case object procedure extends CallType
  case object function extends CallType



/*
  sealed abstract class RetType(@nowarn name: String)
  object RetType {
   case object cursor extends RetType("cursor")
  }

  sealed abstract class SucCond(@nowarn name:String)
  object SucCond{
    case object rows_gt extends SucCond("SucCond")
    case object rows_eq extends SucCond("rows_eq")
    case object resp_time_ms extends SucCond("resp_time_ms")
    case object no_exception extends SucCond("no_exception")
    case object exception extends SucCond("exception")
  }
*/


  case class TestsMeta(connect_ip: String,
                       db_name: String,
                       db_user: String,
                       db_password: String)
/*
  object TypeAliases {
    type SucCondType = (SucCond, Int)
  }
*/

  case class Test(name: String,
                  call_type: CallType
                  /*,
                  ret_type: RetType,
                  call: String,
                  success_condition: List[Option[TypeAliases.SucCondType]]
                 */
                 )
/*
  object Test{
    def apply(name: String, call_type: String): Test = {
      call_type match {
        case "procedure" => Test(name,procedure)
        case "function" => Test(name,function)
      }
    }
  }
*/

  case class TestModel(meta: TestsMeta, tests: Option[List[Test]])

  object EncDecTestModelImplicits{
    implicit val encoderCallType: JsonEncoder[CallType] = DeriveJsonEncoder.gen[CallType]

    implicit val decoder: JsonDecoder[CallType] = JsonDecoder[String].map {
      case "procedure" => procedure
      case "function" => function
      case anyValue => throw new Exception(s"Invalid value in field call_type = $anyValue")
    }

    //implicit val decoderCallType: JsonDecoder[CallType] = DeriveJsonDecoder.gen[CallType]

    /*
    implicit val encoderRetType: JsonEncoder[RetType] = DeriveJsonEncoder.gen[RetType]
    implicit val decoderRetType: JsonDecoder[RetType] = DeriveJsonDecoder.gen[RetType]

    implicit val encoderSucCond: JsonEncoder[SucCond] = DeriveJsonEncoder.gen[SucCond]
    implicit val decoderSucCond: JsonDecoder[SucCond] = DeriveJsonDecoder.gen[SucCond]
    */

    implicit val encoderTestsMeta: JsonEncoder[TestsMeta] = DeriveJsonEncoder.gen[TestsMeta]
    implicit val decoderTestsMeta: JsonDecoder[TestsMeta] = DeriveJsonDecoder.gen[TestsMeta]

    implicit val encoderTest: JsonEncoder[Test] = DeriveJsonEncoder.gen[Test]
    implicit val decoderTest: JsonDecoder[Test] = DeriveJsonDecoder.gen[Test]

    implicit val encoderTestModel: JsonEncoder[TestModel] = DeriveJsonEncoder.gen[TestModel]
    implicit val decoderTestModel: JsonDecoder[TestModel] = DeriveJsonDecoder.gen[TestModel]

  }
