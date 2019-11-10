package io.github.databob.generators

import io.github.databob.{Databob, Generator}

import scala.reflect.runtime.{universe => ru}

class TypeMatchingGenerator[A: ru.TypeTag](predicate: (ru.Type, Databob) => Boolean, fn: (ru.Type, Databob) => A) extends Generator[A]() {

  override def pf(databob: Databob) = {
    case tpe if predicate(tpe, databob) => fn(tpe, databob)
  }
}
