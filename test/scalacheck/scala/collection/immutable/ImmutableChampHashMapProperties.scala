package scala.collection.immutable

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck._

object ImmutableChampHashMapProperties extends Properties("immutable.ChampHashMap") {

  type K = String
  type V = String
  type T = (K, V)

  //  override def overrideParameters(p: org.scalacheck.Test.Parameters) =
  //    p.withMinSuccessfulTests(1000)

  property("convertToScalaMapAndCheckSize") = forAll { (input: ChampHashMap[K, V]) =>
    convertToScalaMapAndCheckSize(input)
  }

  private def convertToScalaMapAndCheckSize(input: ChampHashMap[K, V]) =
    HashMap.from(input).size == input.size

  property("convertToScalaMapAndCheckHashCode") = forAll { (input: ChampHashMap[K, V]) =>
    convertToScalaMapAndCheckHashCode(input)
  }

  private def convertToScalaMapAndCheckHashCode(input: ChampHashMap[K, V]) =
    HashMap.from(input).hashCode == input.hashCode

  property("convertToScalaMapAndCheckEquality") = forAll { (input: ChampHashMap[K, V]) =>
    input == HashMap.from(input) && HashMap.from(input) == input
  }

  property("input.equals(duplicate)") = forAll { (inputMap: ChampHashMap[K, V]) =>
    val builder = ChampHashMap.newBuilder[K, V]
    inputMap.foreach(builder.addOne)

    val duplicateMap = builder.result
    inputMap == duplicateMap
  }

  property("checkSizeAfterInsertAll") = forAll { (inputValues: HashMap[K, V]) =>
    val testMap = ChampHashMap.empty[K, V] ++ inputValues
    inputValues.size == testMap.size
  }

  property("containsAfterInsert") = forAll { (inputValues: HashMap[K, V]) =>
    val testMap = ChampHashMap.empty[K, V] ++ inputValues
    inputValues.forall { case (key, value) => testMap.get(key).contains(value) }
  }

  property("iterator equals reverseIterator.reverse()") = forAll { (input: ChampHashMap[K, V]) =>
    val xs: List[(K, V)] = input.iterator
      .foldLeft(List.empty[(K, V)])((list: List[(K, V)], item: (K, V)) => list prepended item)

    val ys: List[(K, V)] = input.reverseIterator
      .foldLeft(List.empty[(K, V)])((list: List[(K, V)], item: (K, V)) => list appended item)

    xs == ys
  }

}
