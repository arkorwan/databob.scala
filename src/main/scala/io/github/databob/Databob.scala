package io.github.databob

import io.github.databob.generators.Generators._
import io.github.databob.generators.{CollectionSizeRange, Generators, MonadGenerators}

import scala.reflect.runtime.{universe => ru}
import scala.util.{Failure, Success, Try}

class Databob(generators: Generators = new Generators()) {

  lazy val mirror = ru.runtimeMirror(getClass.getClassLoader)

  def mk[A](implicit mf: ru.TypeTag[A]): A = {
    try {
      mk(mf.tpe).asInstanceOf[A]
    } catch {
      case e: GeneratorFailure => throw e
      case e: Exception => throw GeneratorFailure(s"Unexpected generation error: ${e.getMessage}", e)
    }
  }

  private[databob] def mk(tpe: ru.Type): Any = {

    val scalaType = tpe.dealias
    val r = generators.pf(this)
    if (r.isDefinedAt(scalaType)) r(scalaType)
    else {
      val constructor = scalaType.typeSymbol.asClass.primaryConstructor
      val attempt = if (constructor == ru.NoSymbol) {
        Failure(new NoSuchElementException(s"$scalaType has no primary constructor"))
      } else Try {
        val classMirror = mirror.reflectClass(scalaType.typeSymbol.asClass)
        val ctorSym = constructor.asMethod
        val ctor = classMirror.reflectConstructor(ctorSym)
        val arguments = ctorSym.paramLists.flatten.map { sym => mk(sym.asTerm.typeSignature) }
        ctor.apply(arguments: _*)
      }
      attempt match {
        case Success(instance) => instance
        case Failure(e) =>
          throw GeneratorFailure("Could not find a generator to match " + scalaType, e)
      }
    }

  }


}

/**
 * Main entry point for object generation
 */
object Databob {
  /**
   * Make an object using the set of generators provided with no fallbacks
   *
   * @param generators the set of generators to use for the generation
   * @param mf         manifest for the object to generate
   * @return the generated object
   */
  def mk[A](implicit generators: Generators = EmptyGenerators, mf: ru.TypeTag[A]): A = new Databob(generators).mk[A]

  /**
   * Make a default object using the overrides provided and falling back to the set of Default generators. Generated collections are empty.
   *
   * @param overrides the set of override generators to apply to the generation
   * @param mf        manifest for the object to generate
   * @return the generated object
   */
  def default[A](implicit overrides: Generators = EmptyGenerators, mf: ru.TypeTag[A]): A = mk[A](overrides ++ Defaults, mf)

  /**
   * Make a random object using the overrides provided and falling back to the set of Random generators. Generated collections are randomly empty.
   *
   * @param overrides the set of override generators to apply to the generation
   * @param mf        manifest for the object to generate
   * @return the generated object
   */
  def random[A](implicit overrides: Generators = EmptyGenerators, mf: ru.TypeTag[A]): A = mk[A](overrides ++ Random, mf)

  /**
   * Make a random object using the overrides provided and falling back to the set of Random generators, but with collections guaranteed to be non-empty
   *
   * @param overrides the set of override generators to apply to the generation
   * @param mf        manifest for the object to generate
   * @return the generated object
   */
  def randomNotEmpty[A](implicit overrides: Generators = EmptyGenerators, mf: ru.TypeTag[A]): A = mk[A](overrides ++ (CollectionSizeRange.between(1, 5) +: Random), mf)

  def randomFull[A](implicit overrides: Generators = EmptyGenerators, mf: ru.TypeTag[A]): A = mk[A](overrides ++ MonadGenerators.Happy ++ (CollectionSizeRange.between(1, 5) +: Random), mf)
}
