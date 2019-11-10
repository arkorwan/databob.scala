package databob.unit

import io.github.databob.generators.Generators
import org.scalatest.{FunSpec, Matchers}

object SomeObjectType

case class AliasMember(l: Longer)

case class AliasNestedMember(l: LongerList)

case class WithAliasTypeArg(l: Option[Longer])

class AliasTypeGeneratorsTest extends FunSpec with Matchers with GeneratorSpecs {

  describe("alias type args") {

    implicit val g = Generators.Defaults

    itSupports[Longer](0L)
    itSupports[Option[Longer]](Some(0L))
    itSupports[AliasMember](AliasMember(0L))
    itSupports[WithAliasTypeArg](WithAliasTypeArg(Some(0L)))

  }

}
