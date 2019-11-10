package databob.unit

import java.util

import io.github.databob.Databob
import io.github.databob.generators._
import org.scalatest.{FunSpec, Matchers}

class CollectionsGeneratorsTest extends FunSpec with Matchers with GeneratorSpecs {

  describe("empty") {
    implicit val g = CollectionGenerators.Empty ++ PrimitiveGenerators.Defaults
    itSupports[List[Int]](List())
    itSupports[Map[Int, Int]](Map())
    itSupports[Set[Int]](Set())
    itSupports[Vector[Int]](Vector())
    itSupports[Seq[Int]](Seq())
    itSupports[util.List[Int]](new util.ArrayList())
    itSupports[util.Map[Int, Int]](new util.HashMap())
    itSupports[util.Set[Int]](new util.HashSet())
    itSupports[LongerList](List())
    itSupports[Array[Int]](Array())
    itSupports[Array[Long]](Array())
    itSupports[Array[Short]](Array())
    itSupports[Array[Byte]](Array())
    itSupports[Array[Char]](Array())
    itSupports[Array[Double]](Array())
    itSupports[Array[Float]](Array())
    itSupports[Array[Boolean]](Array())
    itSupports[Array[String]](Array())
    itSupports[Array[Array[Int]]](Array())
    itSupports[Array[CustomVal]](Array())
    itSupports[Array[LongerList]](Array())
  }

  describe("non-empty") {
    implicit val g = CollectionGenerators.NonEmpty ++ PrimitiveGenerators.Defaults
    itSupports[List[Int]](List(0))
    itSupports[Map[String, Int]](Map("" -> 0))
    itSupports[Set[Int]](Set(0))
    itSupports[Vector[Int]](Vector(0))
    itSupports[Seq[Int]](Seq(0))
    itSupports[LongerList](List(0))
    itSupports[Array[Int]](Array(0))
    itSupports[Array[Long]](Array(0))
    itSupports[Array[Short]](Array(0))
    itSupports[Array[Byte]](Array(0))
    itSupports[Array[Char]](Array(0))
    itSupports[Array[Double]](Array(0))
    itSupports[Array[Float]](Array(0))
    itSupports[Array[Boolean]](Array(false))
    itSupports[Array[String]](Array(""))
    itSupports[Array[Array[Int]]](Array(Array(0)))
    itSupports[Array[CustomVal]](Array(new CustomVal(0)))
    itSupports[Array[LongerList]](Array(List(0)))

    val list = new util.ArrayList[Int]()
    list.add(0)
    itSupports[util.List[Int]](list)

    val map = new util.HashMap[String, Int]()
    map.put("", 0)
    itSupports[util.Map[String, Int]](map)

    val set = new util.HashSet[Int]()
    set.add(0)
    itSupports[util.Set[Int]](set)
  }

  describe("random") {
    implicit val g = CollectionGenerators.Random ++ PrimitiveGenerators.Defaults
    itSupportsRandom[List[Int]]
    itSupportsRandom[Map[Int, Int]]
    itSupportsRandom[Set[Int]]
    itSupportsRandom[Vector[Int]]
    itSupportsRandom[Seq[Int]]
    itSupportsRandom[LongerList]
    itSupportsRandom[Array[Int]]
    itSupportsRandom[Array[Long]]
    itSupportsRandom[Array[Short]]
    itSupportsRandom[Array[Byte]]
    itSupportsRandom[Array[Char]]
    itSupportsRandom[Array[Double]]
    itSupportsRandom[Array[Float]]
    itSupportsRandom[Array[Boolean]]
    itSupportsRandom[Array[String]]
    itSupportsRandom[Array[Array[Int]]]
    itSupportsRandom[Array[CustomVal]]
    itSupportsRandom[Array[LongerList]]
    itSupportsRandom[util.List[Int]]
    itSupportsRandom[util.Map[Int, Int]]
    itSupportsRandom[util.Set[Int]]
  }

}
