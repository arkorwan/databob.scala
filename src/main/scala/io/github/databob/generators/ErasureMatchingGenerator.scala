package io.github.databob.generators

import io.github.databob.{Databob, Generator, GeneratorType}

import scala.reflect.ClassTag

class ErasureMatchingGenerator[A: ClassTag](predicate: (Class[_]) => Boolean, fn: (GeneratorType, Databob) => A) extends Generator[A]() {
  override def pf(databob: Databob) = {
    case generatorType if predicate(generatorType.erasure) => fn(generatorType, databob)
  }
}
