package databob.unit

import io.github.databob.generators.Generators
import org.scalatest.{FunSpec, Matchers}

sealed trait ADT

case object ObjectADT extends ADT

case class ClassADT(a: Int, b: String) extends ADT

case class Nested(value: ADT)

class EnumTypeGeneratorsTest extends FunSpec with Matchers with GeneratorSpecs {

  describe("Sum type") {
    implicit val g = Generators.Random

    itSupports[SomeObjectType.type](SomeObjectType)
    itSupportsRandom[ADT]
    itSupportsRandom[Nested]

  }

}
