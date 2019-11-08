package io.github.databob

import io.github.databob.generators.Generators._
import io.github.databob.generators.{ErasureMatchingGenerator, Generators, TypeMatchingGenerator}

import scala.reflect.ClassTag

/**
 * A generator for a particular type. Essentially a wrapped partial function which when matched will instantiate an A
 * @tparam A the type to be created if the partial function matches
 */
trait Generator[A] {
  /**
   * create the the partial function which will match the given type A and generate an instance of it
   * @param databob to use for generating dependant objects
   * @return a generated A instance
   */
  def pf(databob: Databob): PartialFunction[GeneratorType, A]

  /**
   * Combine this generator and with the passed generator, with this generator taking precedence
   * @param that the generator to append
   * @return the combined Generators
   */
  def +(that: Generator[_]): Generators = this +: (that +: EmptyGenerators)
}

object Generator {
  def apply[A: ClassTag](mk: Databob => A): Generator[A] = new TypeMatchingGenerator[A](mk)
  def typeIs[A: ClassTag](mk: Databob => A): Generator[A] = new TypeMatchingGenerator[A](mk)

  def erasureIsAssignableFrom[R: ClassTag](fn: (GeneratorType, Databob) => R) =
    new ErasureMatchingGenerator(_.isAssignableFrom(implicitly[ClassTag[R]].runtimeClass), fn)

  def erasureIs[R: ClassTag](fn: Databob => R) =
    new ErasureMatchingGenerator(_ == implicitly[ClassTag[R]].runtimeClass, (gt, databob) => fn(databob))

  def erasureIsWithGen[R: ClassTag](fn: (GeneratorType, Databob) => R) =
    new ErasureMatchingGenerator(_ == implicitly[ClassTag[R]].runtimeClass, fn)

}