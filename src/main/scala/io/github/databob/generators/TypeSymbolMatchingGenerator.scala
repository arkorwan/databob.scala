package io.github.databob.generators

import io.github.databob.{Databob, Generator, GeneratorType}
import org.json4s.reflect.TypeInfo

import scala.reflect.runtime.{universe => ru}

class TypeSymbolMatchingGenerator(predicate: (ru.Symbol) => Boolean, fn: (ru.Symbol, ru.Mirror, Databob) => Any) extends Generator[Any]() {

  object TypeSymbol {
    def unapply(ti: TypeInfo): Option[(ru.Symbol, ru.Mirror)] = {
      val runtimeMirror = ru.runtimeMirror(ti.clazz.getClassLoader)
      Some((runtimeMirror.classSymbol(ti.clazz).toType.typeSymbol), runtimeMirror)
    }
  }

  override def pf(databob: Databob) = {
    case GeneratorType(TypeSymbol(sym, runtimeMirror), _, _) if predicate(sym) => fn(sym, runtimeMirror, databob)
  }
}
