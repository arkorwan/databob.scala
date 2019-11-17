package databob.unit

import databob.unit.NestedInPackage.NestedPackageEnum._
import databob.unit.PackageEnum._
import io.github.databob.Databob
import io.github.databob.generators.{EnumTypeGenerators, Generators}
import org.scalatest.{FunSpec, Inside, Matchers}

import scala.collection.mutable.ArrayBuffer

object TopLevelEnum extends Enumeration {
  type TopLevelEnum = Value
  val EINS, ZWEI, DREI = Value
}

object NestedTopLevel {

  object NestedEnum extends Enumeration {
    type NestedEnum = Value
    val ICHI, NI, SAN = Value
  }

}

case class WithNestedEnum(v1: ADT, v2: PackageEnum, v3: NestedPackageEnum, v4: TopLevelEnum.TopLevelEnum,
                          v5: NestedTopLevel.NestedEnum.NestedEnum)

case class WithEnumArray(v1: Array[ADT], v2: Array[PackageEnum], v3: Array[NestedPackageEnum],
                         v4: Array[TopLevelEnum.TopLevelEnum], v5: Array[NestedTopLevel.NestedEnum.NestedEnum])

class EnumTypeGeneratorsTest extends FunSpec with Matchers with GeneratorSpecs with Inside {

  import NestedTopLevel.NestedEnum._
  import TopLevelEnum._

  describe("default - always pick the first item of available instances") {

    implicit val g = Generators.Defaults

    itSupports[SomeObjectType.type](SomeObjectType)
    itSupports[ADT](ClassADT(0, "")) // first item by full class name
    itSupports[PackageEnum](ONE) // first item by id
    itSupports[NestedPackageEnum](UN) // first item by id
    itSupports[TopLevelEnum](EINS) // first item by id
    itSupports[NestedEnum](ICHI) // first item by id
    itSupports[Planets](Planets.Earth)
    itSupports[WithNestedEnum](WithNestedEnum(ClassADT(0, ""), ONE, UN, EINS, ICHI))

    inside(Databob.mk[WithEnumArray]) { case WithEnumArray(v1, v2, v3, v4, v5) =>
      v1 shouldBe empty
      v2 shouldBe empty
      v3 shouldBe empty
      v4 shouldBe empty
      v5 shouldBe empty
    }

  }

  describe("random - pick items randomly") {
    implicit val g = Generators.Random

    itSupportsRandom[ADT]
    itSupportsRandom[PackageEnum]
    itSupportsRandom[NestedPackageEnum]
    itSupportsRandom[Planets]
    itSupportsRandom[WithNestedEnum]
    itSupportsRandom[WithEnumArray]

  }

  describe("enclosing enum search") {

    import EnumTypeGenerators.EnumerationModuleClass.tryCreateClass

    import scala.reflect.runtime.universe._

    val buffer = ArrayBuffer.empty[String]

    def collectorQuery(name: String): Option[Type] = {
      buffer.append(name)
      None
    }

    // databob.unit.NestedTopLevel.NestedEnum.Value
    tryCreateClass(typeOf[NestedEnum], collectorQuery) shouldBe empty

    buffer should contain theSameElementsAs Seq(
      "databob.unit.NestedTopLevel.NestedEnum$",
      "databob.unit.NestedTopLevel$NestedEnum$",
      "databob.unit$NestedTopLevel$NestedEnum$",
      "databob$unit$NestedTopLevel$NestedEnum$",

      "databob.unit.NestedTopLevel.package$NestedEnum$",
      "databob.unit.package$NestedTopLevel$NestedEnum$",
      "databob.package$unit$NestedTopLevel$NestedEnum$"
    )

  }

}
