package data

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class checkTestRepoData(testModelsCntInRepo: Int)

object EncDeccheckTestRepoDataImplicits {
  implicit val encoderSession: JsonEncoder[checkTestRepoData] = DeriveJsonEncoder.gen[checkTestRepoData]
  implicit val decoderSession: JsonDecoder[checkTestRepoData] = DeriveJsonDecoder.gen[checkTestRepoData]
}