package io.github.databob.generators

import io.github.databob.TypeUtils

object SpecializedTypeGenerators {

  val objGenerator = {
    new TypeSymbolMatchingGenerator(_.isModuleClass, (sym, mirror, _) => {
      mirror.reflectModule(sym.asClass.module.asModule).instance
    })
  }

  val sealedAbstractGenerator = {
    new TypeSymbolMatchingGenerator(sym => {
      val scalaClz = sym.asClass
      scalaClz.isSealed && scalaClz.isAbstract
    }, (sym, mirror, databob) => {
      val scalaClz = sym.asClass
      val concreteSubclasses = scalaClz.knownDirectSubclasses.toSeq.filterNot(_.isAbstract)
      val pick = concreteSubclasses(scala.util.Random.nextInt(concreteSubclasses.length))
      val mf = TypeUtils.typeToTypeTag(pick.asClass.toType, mirror)
      databob.mk(mf).asInstanceOf[Any]
    })

  }

  lazy val Defaults = (objGenerator + sealedAbstractGenerator)

  lazy val Random = Defaults

}
