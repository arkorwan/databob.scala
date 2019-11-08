package io.github.databob

import io.github.databob.generators.Generators._
import io.github.databob.generators.{CollectionSizeRange, Generators}
import org.json4s.reflect._

import scala.reflect.{ClassTag, ManifestFactory}
import scala.reflect.runtime.universe._

class Databob(generators: Generators = new Generators()) {

  // https://stackoverflow.com/questions/29127557/how-to-maintain-type-parameter-during-typetag-to-manifest-conversion
  def toManifest[T: TypeTag]: Manifest[T] = {
    val t = typeTag[T]
    val mirror = t.mirror

    def toManifestRec(t: Type): Manifest[_] = {
      val clazz = ClassTag[T](mirror.runtimeClass(t)).runtimeClass
      if (t.typeArgs.length == 1) {
        val arg = toManifestRec(t.typeArgs.head)
        ManifestFactory.classType(clazz, arg)
      } else if (t.typeArgs.length > 1) {
        val args = t.typeArgs.map(x => toManifestRec(x))
        ManifestFactory.classType(clazz, args.head, args.tail: _*)
      } else {
        ManifestFactory.classType(clazz)
      }
    }

    toManifestRec(t.tpe).asInstanceOf[Manifest[T]]
  }

  def mk[A](implicit tt: TypeTag[A]): A = {
    try {
      implicit val mf = toManifest[A]
      mk(Reflector.scalaTypeOf[A]).asInstanceOf[A]
    } catch {
      case e: GeneratorFailure => throw e
      case e: Exception => throw new GeneratorFailure(s"Unexpected generation error: ${e.getMessage}", e)
    }
  }

  private[databob] def mk(scalaType: ScalaType): Any = {
    val generatorType = GeneratorType(scalaType.typeInfo, scalaType.erasure, scalaType.typeArgs)
    val r = generators.pf(this)

    if (r.isDefinedAt(generatorType)) r(generatorType)
    else {
      Reflector.describe(scalaType) match {
        case cd: ClassDescriptor => {
          val constructor = cd.constructors.headOption.getOrElse(throw new GeneratorFailure("No constructor found for type " + cd.erasure))
          constructor.constructor.invoke(cd.companion, constructor.params.map(a => mk(a.argType))).asInstanceOf[AnyRef]
        }
        case unknown => throw new GeneratorFailure("Could not find a generator to match " + unknown)
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
  def mk[A](implicit generators: Generators = EmptyGenerators, mf: TypeTag[A]): A = new Databob(generators).mk[A]

  /**
   * Make a default object using the overrides provided and falling back to the set of Default generators. Generated collections are empty.
   *
   * @param overrides the set of override generators to apply to the generation
   * @param mf        manifest for the object to generate
   * @return the generated object
   */
  def default[A](implicit overrides: Generators = EmptyGenerators, mf: TypeTag[A]): A = mk[A](overrides ++ Defaults, mf)

  /**
   * Make a random object using the overrides provided and falling back to the set of Random generators. Generated collections are randomly empty.
   *
   * @param overrides the set of override generators to apply to the generation
   * @param mf        manifest for the object to generate
   * @return the generated object
   */
  def random[A](implicit overrides: Generators = EmptyGenerators, mf: TypeTag[A]): A = mk[A](overrides ++ Random, mf)

  /**
   * Make a random object using the overrides provided and falling back to the set of Random generators, but with collections guaranteed to be non-empty
   *
   * @param overrides the set of override generators to apply to the generation
   * @param mf        manifest for the object to generate
   * @return the generated object
   */
  def randomNotEmpty[A](implicit overrides: Generators = EmptyGenerators, mf: TypeTag[A]): A =
    mk[A](overrides ++ (CollectionSizeRange.between(1, 5) +: Random), mf)
}
