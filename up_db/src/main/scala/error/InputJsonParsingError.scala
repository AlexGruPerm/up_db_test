package error

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class InputJsonParsingError(msg: String)

object InputJsonParsingError{
  implicit val encoderInputJsonParsingError: JsonEncoder[InputJsonParsingError] = DeriveJsonEncoder.gen[InputJsonParsingError]
  implicit val decoderInputJsonParsingError: JsonDecoder[InputJsonParsingError] = DeriveJsonDecoder.gen[InputJsonParsingError]
}
