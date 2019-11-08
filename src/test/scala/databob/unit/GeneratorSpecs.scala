package databob.unit

import io.github.databob.Databob
import io.github.databob.generators._
import org.scalatest.{FunSpec, Matchers}

import scala.reflect.runtime.universe.TypeTag

trait GeneratorSpecs {
  self: FunSpec with Matchers =>
  def itSupports[A: TypeTag](expected: Any)(implicit generators: Generators, mf: TypeTag[A]): Unit = {
    it(mf.tpe.typeSymbol + " at " + System.nanoTime()) {
      Databob.mk[A](generators, mf) shouldBe expected
    }
  }

  def itSupportsRandom[A: TypeTag](implicit generators: Generators, mf: TypeTag[A]): Unit = {
    it(mf.tpe.typeSymbol + " at " + System.nanoTime()) {
      Range(1, 50).map(i => Databob.mk[A](generators, mf)).toSet.size should not be 1
    }
  }
}