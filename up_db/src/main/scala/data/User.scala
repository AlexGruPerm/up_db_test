package data

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}
import java.util.UUID
import zio.json._

case class User(name: String, age: Int)

/*
object User{
  implicit object JsonEncoder[User] = DeriveJsonEncoder.gen[User]
  implicit JsonDecoder[User] = DeriveJsonDecoder.gen[User]
}*/