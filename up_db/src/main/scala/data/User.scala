package data

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}
import java.util.UUID
import zio.json._

case class User(name: String, age: Int)

object User{
  implicit val encoder: JsonEncoder[User] = DeriveJsonEncoder.gen[User]
  implicit val decoder: JsonDecoder[User] = DeriveJsonDecoder.gen[User]
}