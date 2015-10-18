package org.json4s

case class Other(name: String)

case class Person(age: Int, names: Seq[Other])

object Test extends App {
  implicit val f = DefaultRandomFormats
  println(Random.random[Person](JObject()))
}
