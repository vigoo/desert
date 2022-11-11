package io.github.vigoo.desert.shapeless

import io.github.vigoo.desert._
import io.github.vigoo.desert.codecs._

object DefaultTypeRegistrySpec extends DefaultTypeRegistrySpecBase {
  implicit val codec: BinaryCodec[TestProd] = DerivedBinaryCodec.derive()
}
