package databob

import scala.collection.mutable

package object unit {

  // custom types for testing
  type Longer = Long
  type LongerList = List[Longer]

  class CustomVal(val a: Int) extends AnyVal

  object SomeObjectType

  case class AliasMember(l: Longer)

  case class AliasNestedMember(l: LongerList)

  case class WithAliasTypeArg(l: Option[Longer])

  case class OfCollections(a: List[Int], b: Vector[String], c: Map[String, Double], d: mutable.Map[Int, Boolean])

  sealed trait ADT

  case object ObjectADT extends ADT

  case class ClassADT(a: Int, b: String) extends ADT

  object PackageEnum extends Enumeration {
    type PackageEnum = Value
    val ONE, TWO, THREE = Value
  }

  object NestedInPackage {
    object NestedPackageEnum extends Enumeration {
      type NestedPackageEnum = Value
      val UN, DOS, TRES = Value
    }
  }


}