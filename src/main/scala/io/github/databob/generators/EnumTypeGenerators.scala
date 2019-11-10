package io.github.databob.generators

object EnumTypeGenerators {

  // need to support case objects for ADT sum type.
  val objGenerator = {
    new TypeMatchingGenerator((tpe, _) => tpe.typeSymbol.isModuleClass,
      (tpe, databob) => {
        databob.mirror.reflectModule(tpe.typeSymbol.asClass.module.asModule).instance
      }
    )
  }

  val sealedAbstractGenerator = {
    new TypeMatchingGenerator((tpe, _) => {
      val scalaClz = tpe.typeSymbol.asClass
      scalaClz.isSealed && scalaClz.isAbstract
    }, (tpe, databob) => {
      val concreteSubclasses = tpe.typeSymbol.asClass.knownDirectSubclasses.toSeq.filterNot(_.isAbstract)
      val pick = concreteSubclasses(scala.util.Random.nextInt(concreteSubclasses.length))
      databob.mk(pick.asClass.toType)
    })

  }

  lazy val Defaults = (objGenerator + sealedAbstractGenerator)

  lazy val Random = Defaults

}
