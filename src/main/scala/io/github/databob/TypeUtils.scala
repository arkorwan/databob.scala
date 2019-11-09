package io.github.databob

import scala.reflect.runtime.universe._
import scala.reflect.{ClassTag, ManifestFactory}

object TypeUtils {

  // https://stackoverflow.com/a/29127923/1823254
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

  // https://stackoverflow.com/a/25691045/1823254
  def typeToTypeTag[T](
                        tpe: Type,
                        mirror: reflect.api.Mirror[reflect.runtime.universe.type]
                      ): TypeTag[T] = {
    TypeTag(mirror, new reflect.api.TypeCreator {
      def apply[U <: reflect.api.Universe with Singleton](m: reflect.api.Mirror[U]) = {
        assert(m eq mirror, s"TypeTag[$tpe] defined in $mirror cannot be migrated to $m.")
        tpe.asInstanceOf[U#Type]
      }
    })
  }

  // https://stackoverflow.com/a/22972751/1823254
  def classToTypeTag[T](c: Class[T]) ={
    val mirror = runtimeMirror(c.getClassLoader)  // obtain runtime mirror
    val sym = mirror.staticClass(c.getName)  // obtain class symbol for `c`
    val tpe = sym.selfType  // obtain type object for `c`
    typeToTypeTag[T](tpe, mirror)
  }



}
