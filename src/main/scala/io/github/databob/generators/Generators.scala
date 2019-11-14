package io.github.databob.generators

import io.github.databob.{Databob, Generator}

import scala.reflect.runtime.{universe => ru}

/**
 * Represents an entire Partial Function for generating instances. As such, the ordering of the component
 * generators will determine order of application (ie. earlier generators matched first). Immutable
 */
class Generators(generators: Iterable[Generator] = Nil) extends Iterable[Generator] {

  /**
   * Create a new Generators instance with the passed generator appended (ie. lower precedence)
   * @param newGenerator the generator to append
   * @return Newly combined generators instance
   */
  def +(newGenerator: Generator): Generators = new Generators(generators ++ Seq(newGenerator))

  /**
   * Create a new Generators instance with the passed generator prepended (ie. higher precedence)
   * @param newGenerator the generator to prepend
   * @return Newly combined generators instance
   */
  def +:(newGenerator: Generator): Generators = new Generators(Seq(newGenerator) ++ generators)

  /**
   * Create a new Generators instance with the passed generators appended (ie. lower precedence)
   * @param that the generators to append
   * @return Newly combined generators instance
   */
  def ++(that: Generators): Generators = new Generators(generators ++ that)

  /**
   * Get the overall Partial Function represented by the constituent generators
   * @param databob to use for generating dependant objects
   * @return PF
   */
  def pf(databob: Databob) =
    generators.foldLeft(Map(): PartialFunction[ru.Type, Any]) { (acc, x) =>
      acc.orElse(x.pf(databob))
    }

  /**
   * The constituent generators
   */
  override def iterator: Iterator[Generator] = generators.iterator
}

object Generators {

  lazy val EmptyGenerators = new Generators()

  lazy val Defaults =
    PrimitiveGenerators.Defaults ++
      MonadGenerators.Happy ++
      DateTimeGenerators.Epoch ++
      CollectionGenerators.Empty ++
      EnumTypeGenerators.Defaults

  lazy val Random =
    PrimitiveGenerators.Random ++
      MonadGenerators.Random ++
      DateTimeGenerators.Random ++
      CollectionGenerators.Random ++
      EnumTypeGenerators.Random

}
