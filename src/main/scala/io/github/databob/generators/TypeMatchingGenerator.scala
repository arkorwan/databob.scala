package io.github.databob.generators

import io.github.databob.{Databob, Generator}

import scala.reflect.runtime.{universe => ru}

class TypeMatchingGenerator[B: ru.TypeTag](predicate: (ru.Type, Databob) => Boolean, fn: (ru.Type, Databob) => B) extends Generator() {

  override def pf(databob: Databob) = {
    case tpe if tpe <:< ru.typeOf[B] && predicate(tpe, databob) => fn(tpe, databob)
  }
}
