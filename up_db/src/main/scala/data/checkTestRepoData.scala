package data

import common.types.SessionId
import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class TestsStatus(total: Int, enabled: Int, disabled: Int, executed: Int, success: Int, failure: Int)

case class checkTestRepoInfo(tests: List[(SessionId,TestsStatus)])

object EncDeccheckTestRepoDataImplicits {

  implicit val encoderTestsStatus: JsonEncoder[TestsStatus] = DeriveJsonEncoder.gen[TestsStatus]
  implicit val decoderTestsStatus: JsonDecoder[TestsStatus] = DeriveJsonDecoder.gen[TestsStatus]

  implicit val encoderSession: JsonEncoder[checkTestRepoInfo] = DeriveJsonEncoder.gen[checkTestRepoInfo]
  implicit val decoderSession: JsonDecoder[checkTestRepoInfo] = DeriveJsonDecoder.gen[checkTestRepoInfo]
}