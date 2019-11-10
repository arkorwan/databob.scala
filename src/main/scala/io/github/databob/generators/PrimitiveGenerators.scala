package io.github.databob.generators

import java.lang.{Boolean => JavaBoolean, Byte => JavaByte, Character => JavaChar, Double => JavaDouble, Float => JavaFloat, Integer => JavaInteger, Long => JavaLong, Short => JavaShort}
import java.math.{BigDecimal => JavaBigDecimal, BigInteger => JavaBigInteger}
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
  lazy val Defaults = typeIs[Int](_.mk[BigDecimal].toInt) +
    typeIs[BigDecimal](_ => BigDecimal(0)) +
    typeIs[JavaBigDecimal](_.mk[BigDecimal].bigDecimal) +
    typeIs[BigInt](_.mk[BigDecimal].toBigInt()) +
    typeIs[JavaBigInteger](_.mk[BigInt].bigInteger) +
    typeIs[String](_ => "") +
    typeIs[Long](_.mk[BigDecimal].toLong) +
    typeIs[JavaLong](_.mk[Long]) +
    typeIs[Int](_.mk[BigDecimal].toInt) +
    typeIs[JavaInteger](_.mk[Int]) +
    typeIs[Double](_.mk[BigDecimal].toDouble) +
    typeIs[JavaDouble](_.mk[Double]) +
    typeIs[Float](_.mk[BigDecimal].toFloat) +
    typeIs[JavaFloat](_.mk[Float]) +
    typeIs[Short](_.mk[BigDecimal].toShort) +
    typeIs[JavaShort](_.mk[Short]) +
    typeIs[Byte](_.mk[BigDecimal].toByte) +
    typeIs[JavaByte](_.mk[Byte]) +
    typeIs[Boolean](_ => false) +
    typeIs[JavaBoolean](_.mk[Boolean]) +
    typeIs[Char](_.mk[BigDecimal].toChar) +
    typeIs[JavaChar](_.mk[Char]) +
    typeIs[Exception](databob => new Exception(databob.mk[String])) +
    typeIs[RuntimeException](databob => new RuntimeException(databob.mk[String])) +
    typeIs[UUID](databob => new UUID(databob.mk[Long], databob.mk[Long]))

  /**
   * Creates random Primitive values
   */
  lazy val Random =
    typeIs(_ => CoinToss.Even) +
      typeIs[BigDecimal](_ => BigDecimal(scala.util.Random.nextDouble() * Integer.MAX_VALUE)) +
      typeIs[Boolean](_ => Databob.random[CoinToss].toss) +
      typeIs[UUID](_ => UUID.randomUUID()) +
      typeIs[String](_ => Databob.random[UUID].toString) ++
      Defaults
}
