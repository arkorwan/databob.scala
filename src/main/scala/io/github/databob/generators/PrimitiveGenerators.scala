package io.github.databob.generators

import java.lang.{Character => JavaCharacter}
import java.util.UUID

import io.github.databob.Databob
import io.github.databob.Generator._

/**
 * Generators for Primitive types
 */
object PrimitiveGenerators {

  /**
   * Creates Primitive values with their default values (0 for numeric, empty Strings, false)
   */
  lazy val Defaults = erasureIs[Int](databob => databob.mk[BigDecimal].toInt) +
    erasureIs[BigDecimal](databob => BigDecimal(0)) +
    erasureIs[BigInt](databob => databob.mk[BigDecimal].toBigInt()) +
    erasureIs[String](databob => "") +
    erasureIs[Long](databob => databob.mk[BigDecimal].toLong) +
    erasureIs[Double](databob => databob.mk[BigDecimal].toDouble) +
    erasureIs[Float](databob => databob.mk[BigDecimal].toFloat) +
    erasureIs[Short](databob => databob.mk[BigDecimal].toShort) +
    erasureIs[Byte](databob => databob.mk[BigDecimal].toByte) +
    erasureIs[Boolean](databob => false) +
    erasureIs[Char](databob => databob.mk[BigDecimal].toChar) +
    erasureIs[JavaCharacter](databob => databob.mk[BigDecimal].toChar) +
    erasureIs[Exception](databob => new Exception(databob.mk[String])) +
    erasureIs[RuntimeException](databob => new RuntimeException(databob.mk[String])) +
    erasureIs[UUID](databob => new UUID(databob.mk[Long], databob.mk[Long]))

  /**
   * Creates random Primitive values
   */
  lazy val Random =
    typeIs(databob => CoinToss.Even) +
      erasureIs[BigDecimal](databob => BigDecimal(scala.util.Random.nextDouble() * Integer.MAX_VALUE)) +
      erasureIs[Boolean](databob => Databob.random[CoinToss].toss) +
      erasureIs[UUID](databob => UUID.randomUUID()) +
      erasureIs[String](databob => Databob.random[UUID].toString) ++
      Defaults
}
