package io.github.databob.generators

import io.github.databob.{Databob, Generator}
import io.github.databob.Generator._

import scala.annotation.tailrec
import scala.reflect.runtime.{universe => ru}
import scala.util.Try

object EnumTypeGenerators {

  case class Sampler(value: Double) {
    def sampleFrom[A](available: List[A]) = available((value * available.length).toInt)
  }

  // extra Generators to help picking a sample from a fixed size collection
  val defaultSampler = typeIs[Sampler](_ => Sampler(0))
  val randomSampler = typeIs[Sampler](_ => Sampler(scala.util.Random.nextDouble()))

  // -------------- SUM TYPE ------------

  // need to support case objects. So just supports all objects.
  val objGenerator = new TypeMatchingGenerator((tpe, _) => tpe.typeSymbol.isModuleClass,
    (tpe, databob) => {
      databob.mirror.reflectModule(tpe.typeSymbol.asClass.module.asModule).instance
    }
  )

  val sealedAbstractGenerator = new TypeMatchingGenerator((tpe, _) => {
    val classSym = tpe.typeSymbol.asClass
    classSym.isSealed && classSym.isAbstract
  }, (tpe, databob) => {
    val concreteSubclasses = tpe.typeSymbol.asClass.knownDirectSubclasses.toList.sortBy(_.fullName).filterNot(_.isAbstract)
    val selectedClass = databob.mk[Sampler].sampleFrom(concreteSubclasses)
    databob.mk(selectedClass.asClass.toType)
  })

  // -------------- Scala Enumeration ------------

  object EnumerationModuleClass {

    def fromClassName(name: String): Option[ru.Type] = {
      Try { Class.forName(name) }.toOption.flatMap{ clazz =>
        val mirror = ru.runtimeMirror(clazz.getClassLoader)
        val encTpe = mirror.classSymbol(clazz).toType
        if(encTpe <:< ru.typeOf[Enumeration]) Some(encTpe) else None
      }
    }

    // recursively querying for the enclosing class by name, allowing for nested type
    // as long as it's not path-dependent. Package objects are also supported.
    // Note that type name for type within package object might or might not have the
    // subpackage '.package' in it, so we need to try out both options.
    def tryCreateClass(tpe: ru.Type, query: String => Option[ru.Type]): Option[ru.Type] = {

      @tailrec
      def find(reversePrefixes: Seq[String], suffix: String): Option[ru.Type] = {
        if(reversePrefixes.isEmpty){
          query(suffix)
        } else {
          val prefix = reversePrefixes.reverse.mkString(".")
          // case 1: not involving package object OR the name 'package' is already present
          lazy val attempt1 = query(prefix + "." + suffix)
          // case 2: module is defined in a package object, and the name 'package' is not present
          lazy val attempt2 = query(prefix + ".package$" + suffix)
          (attempt1, attempt2, reversePrefixes) match {
            case (v1, _, _) if v1.nonEmpty => v1
            case (_, v2, _) if v2.nonEmpty => v2
            case (_, _, Nil) => None
            case (_, _, head +: tail) => find(tail, head + "$" + suffix)
          }
        }
      }

      val typeName = tpe.toString
      val path = typeName.split('.').reverse.toList.tail // drop the "Value" class
      find(path.tail, path.head + "$")
    }

    val cache = scala.collection.mutable.Map.empty[ru.Type, Option[ru.Type]]

    // custom extractor to get enclosing enum class
    def unapply(tpe: ru.Type): Option[ru.Type] = {
      if (tpe.typeSymbol.owner.asClass.toType <:< ru.typeOf[Enumeration]) {
        // getting class by going through class name as string ... any better ways?
        cache getOrElseUpdate (tpe, tryCreateClass(tpe, fromClassName))
      } else None
    }
  }

  val scalaEnumGenerator: Generator[Any] = new Generator[Any]{
    override def pf(databob: Databob): PartialFunction[ru.Type, Any] = {
      case EnumerationModuleClass(enclosingType) =>
        val enumModule = databob.mirror.reflectModule(enclosingType.typeSymbol.asClass.module.asModule)
          .instance.asInstanceOf[Enumeration]
        val values = enumModule.values.toList.sortBy(_.id)
        databob.mk[Sampler].sampleFrom(values)
    }
  }

  private lazy val Base = (objGenerator + sealedAbstractGenerator + scalaEnumGenerator)

  lazy val Defaults = defaultSampler +: Base

  lazy val Random = randomSampler +: Defaults

}
