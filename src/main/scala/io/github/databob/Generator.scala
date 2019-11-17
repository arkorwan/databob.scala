package io.github.databob

import io.github.databob.generators.Generators._
import io.github.databob.generators.{Generators, TypeMatchingGenerator, TypePredicateGenerator}

import scala.reflect.runtime.{universe => ru}

/**
 * A generator for a particular type. Essentially a wrapped partial function which when matched will instantiate an A
 * @tparam A the type to be created if the partial function matches
 */
trait Generator {
  /**
   * create the the partial function which will match the given type A and generate an instance of it
   * @param databob to use for generating dependant objects
   * @return a generated A instance
   */
  def pf(databob: Databob): PartialFunction[ru.Type, Any]

  /**
   * Combine this generator and with the passed generator, with this generator taking precedence
   * @param that the generator to append
   * @return the combined Generators
   */
  def +(that: Generator): Generators = this +: (that +: EmptyGenerators)
}

object Generator {
  def typeIs[A: ru.TypeTag](fn: Databob => A): Generator =
    new TypeMatchingGenerator[A]((_, databob) => fn(databob))
  def typeIsWithType[A: ru.TypeTag](fn: (ru.Type, Databob) => A) =
    new TypeMatchingGenerator[A](fn)

  def typeMatches[A](predicate: (ru.Type, Databob) => Boolean, fn: (ru.Type, Databob) => A) =
    new TypePredicateGenerator[A](predicate, fn)

}