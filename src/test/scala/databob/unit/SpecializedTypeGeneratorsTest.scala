package databob.unit

import io.github.databob.generators._
import org.scalatest.{FunSpec, Matchers}

// test types

object SomeObjectType

sealed trait AdtV1
case object AdtV1_1 extends AdtV1
case object AdtV1_2 extends AdtV1

sealed abstract class AdtV2
case object AdtV2_1 extends AdtV2
case object AdtV2_2 extends AdtV2

case class Nested(v1: AdtV1, v2: AdtV2)

// =====================

class SpecializedTypeGeneratorsTest extends FunSpec with Matchers with GeneratorSpecs {

  describe("default specialized type") {
    implicit val g = SpecializedTypeGenerators.Defaults

    itSupports[SomeObjectType.type](SomeObjectType)
    itSupportsRandom[AdtV1]
    itSupportsRandom[AdtV2]
    itSupportsRandom[Nested]

  }


}
