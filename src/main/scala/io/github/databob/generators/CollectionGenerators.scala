package io.github.databob.generators

import io.github.databob.Databob
import io.github.databob.Generator._
import io.github.databob.generators.CollectionSizeRange.exactly

import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

/**
 * Generators for Collection types
 */
object CollectionGenerators {

  private def range(databob: Databob) = databob.mk[CollectionSizeRange].toRandomRange

  /**
   * Generates Empty collections
   */
  lazy val Empty = CollectionSizeRange.none +
    typeMatches(tpe => tpe <:< ru.typeOf[Traversable[_]], (gt, databob) => {
      val companion = databob.mirror.reflectModule(gt.companion.typeSymbol.asClass.module.asModule)
      val instance = databob.mirror.reflect(companion.instance)
      val applyMethod = gt.companion.member(ru.TermName("apply")).asMethod
      val nestedFunc = gt.typeArgs.length match {
        case 1 => () => databob.mk(gt.typeArgs.head)
        case 2 => () => databob.mk(gt.typeArgs.head) -> databob.mk(gt.typeArgs(1))
        case _ => throw new UnsupportedOperationException()
      }
      val args = range(databob).map(i => nestedFunc.apply())
      instance.reflectMethod(applyMethod)(args)
    }) +
    typeIsWithType[Array[Int]]((_, databob) => range(databob).map(i => databob.mk[Int]).toArray) +
    typeIsWithType[Array[Long]]((_, databob) => range(databob).map(i => databob.mk[Long]).toArray) +
    typeIsWithType[Array[Short]]((_, databob) => range(databob).map(i => databob.mk[Short]).toArray) +
    typeIsWithType[Array[Byte]]((_, databob) => range(databob).map(i => databob.mk[Byte]).toArray) +
    typeIsWithType[Array[Char]]((_, databob) => range(databob).map(i => databob.mk[Char]).toArray) +
    typeIsWithType[Array[Double]]((_, databob) => range(databob).map(i => databob.mk[Double]).toArray) +
    typeIsWithType[Array[Float]]((_, databob) => range(databob).map(i => databob.mk[Float]).toArray) +
    typeIsWithType[Array[Boolean]]((_, databob) => range(databob).map(i => databob.mk[Boolean]).toArray) +
    new TypeMatchingGenerator((tpe, databob) => {
      databob.mirror.runtimeClass(tpe).isArray
    }, (tpe, databob) => {
      val items = range(databob).map(_ => databob.mk(tpe.typeArgs.head))
      val clsTag = ClassTag[Any](databob.mirror.runtimeClass(tpe.typeArgs.head))
      items.toArray(clsTag)
    }) +
    typeIsWithType[java.util.List[_]]((tpe, databob) => {
      val l = new java.util.ArrayList[Any]()
      l.addAll(range(databob).map(i => databob.mk(tpe.typeArgs.head)).toList.asJava)
      l
    }) +
    typeIsWithType[java.util.Set[_]]((tpe, databob) => {
      val s = new java.util.HashSet[Any]()
      s.addAll(range(databob).map(i => databob.mk(tpe.typeArgs.head)).toList.asJava)
      s
    }) +
    typeIsWithType[java.util.Map[_, _]]((tpe, databob) => {
      val map = Map(range(databob).map(i => databob.mk(tpe.typeArgs.head) -> databob.mk(tpe.typeArgs(1))): _*)
      val s = new java.util.HashMap[Any, Any]()
      s.putAll(map.asJava)
      s
    })

  /**
   * Generates Non-Empty collections
   */
  lazy val NonEmpty = exactly(1) +: Empty

  /**
   * Generates Random collections
   */
  lazy val Random =
    typeIs(_ => CoinToss.Even) +
      typeIs[CollectionSizeRange](_ => if (Databob.random[CoinToss].toss) CollectionSizeRange(1, 5) else CollectionSizeRange.empty) ++
      Empty
}
