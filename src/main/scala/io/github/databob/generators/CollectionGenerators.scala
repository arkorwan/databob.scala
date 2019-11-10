package io.github.databob.generators

import io.github.databob.Databob
import io.github.databob.Generator._
import io.github.databob.generators.CollectionSizeRange.exactly

import scala.collection.JavaConverters._

/**
 * Generators for Collection types
 */
object CollectionGenerators {

  private def range(databob: Databob) = databob.mk[CollectionSizeRange].toRandomRange

  /**
   * Generates Empty collections
   */
  lazy val Empty = CollectionSizeRange.none +
    typeMatches[Map[_, _]]((gt, databob) => Map(range(databob).map(i => databob.mk(gt.typeArgs.head) -> databob.mk(gt.typeArgs(1))): _*)) +
    typeMatches[Set[_]]((gt, databob) => Set(range(databob).map(i => databob.mk(gt.typeArgs.head)): _*)) +
    typeMatches[List[_]]((gt, databob) => List(range(databob).map(i => databob.mk(gt.typeArgs.head)): _*)) +
    typeMatches[Vector[_]]((gt, databob) => Vector(range(databob).map(i => databob.mk(gt.typeArgs.head)): _*)) +
    typeMatches[Seq[_]]((gt, databob) => Seq(range(databob).map(i => databob.mk(gt.typeArgs.head)): _*)) +
    new TypeMatchingGenerator((tpe, databob) => {
      databob.mirror.runtimeClass(tpe).isArray
    }, (tpe, databob) => Array(range(databob).map(i => databob.mk(tpe.typeArgs.head)): _*)) +
    typeMatches[java.util.List[_]]((tpe, databob) => {
      val l = new java.util.ArrayList[Any]()
      l.addAll(range(databob).map(i => databob.mk(tpe.typeArgs.head)).toList.asJava)
      l
    }) +
    typeMatches[java.util.Set[_]]((tpe, databob) => {
      val s = new java.util.HashSet[Any]()
      s.addAll(range(databob).map(i => databob.mk(tpe.typeArgs.head)).toList.asJava)
      s
    }) +
    typeMatches[java.util.Map[_, _]]((tpe, databob) => {
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
