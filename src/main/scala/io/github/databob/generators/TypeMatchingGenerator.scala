package io.github.databob.generators

import io.github.databob.{Databob, Generator, GeneratorType}
import org.json4s.reflect.TypeInfo

import scala.reflect.ClassTag

class TypeMatchingGenerator[A: ClassTag](mk: Databob => A) extends Generator[A] {
   val Class = implicitly[ClassTag[A]].runtimeClass

   def pf(databob: Databob): PartialFunction[GeneratorType, A] = {
     case GeneratorType(TypeInfo(Class, _), _, _) => mk(databob)
   }
 }
