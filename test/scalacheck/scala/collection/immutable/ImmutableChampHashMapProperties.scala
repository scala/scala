package scala.collection.immutable

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck._

object ImmutableChampHashMapProperties extends Properties("immutable.HashMap") {

  type K = String
  type V = String
  type T = (K, V)

  //  override def overrideParameters(p: org.scalacheck.Test.Parameters) =
  //    p.withMinSuccessfulTests(1000)

  property("convertToScalaMapAndCheckSize") = forAll { (input: HashMap[K, V]) =>
    convertToScalaMapAndCheckSize(input)
  }

  private def convertToScalaMapAndCheckSize(input: HashMap[K, V]) =
    HashMap.from(input).size == input.size

  property("convertToScalaMapAndCheckHashCode") = forAll { input: HashMap[K, V] =>
    convertToScalaMapAndCheckHashCode(input)
  }

  private def convertToScalaMapAndCheckHashCode(input: HashMap[K, V]) =
    HashMap.from(input).hashCode == input.hashCode

  property("convertToScalaMapAndCheckEquality") = forAll { input: HashMap[K, V] =>
    input == HashMap.from(input) && HashMap.from(input) == input
  }

  property("input.equals(duplicate)") = forAll { inputMap: HashMap[K, V] =>
    val builder = HashMap.newBuilder[K, V]
    inputMap.foreach(builder.addOne)

    val duplicateMap = builder.result
    inputMap == duplicateMap
  }

  property("checkSizeAfterInsertAll") = forAll { inputValues: HashMap[K, V] =>
    val testMap = HashMap.empty[K, V] ++ inputValues
    inputValues.size == testMap.size
  }

  property("containsAfterInsert") = forAll { inputValues: HashMap[K, V] =>
    val testMap = HashMap.empty[K, V] ++ inputValues
    inputValues.forall { case (key, value) => testMap.get(key).contains(value) }
  }

  property("iterator equals reverseIterator.reverse()") = forAll { input: HashMap[K, V] =>
    val xs: List[(K, V)] = input.iterator
      .foldLeft(List.empty[(K, V)])((list: List[(K, V)], item: (K, V)) => list prepended item)

    val ys: List[(K, V)] = input.reverseIterator
      .foldLeft(List.empty[(K, V)])((list: List[(K, V)], item: (K, V)) => list appended item)

    xs == ys
  }

  property("building element-wise is the same as in bulk") = forAll { seq: Seq[(K, V)] =>
    val xs = (HashMap.newBuilder[K, V] ++= seq).result()
    val ys = {
      val b = HashMap.newBuilder[K, V]
      seq.foreach(b += _)
      b.result()
    }
    var zs = HashMap.empty[K, V]
    seq.foreach(zs += _)

    xs == ys && xs == zs
  }

  property("adding elems twice to builder is the same as adding them once") = forAll { seq: Seq[(K, V)] =>
    val b = HashMap.newBuilder[K, V].addAll(seq)
    b.result == b.addAll(seq).result()
  }

  property("(xs ++ ys).toMap == xs.toMap ++ ys.toMap") = forAll { (xs: Seq[(K, V)],ys: Seq[(K, V)]) =>
    (xs ++ ys).toMap == xs.toMap ++ ys.toMap
  }

  property("HashMapBuilder produces the same Map as MapBuilder") = forAll { (xs: Seq[(K, V)]) =>
    Map.newBuilder[K, V].addAll(xs).result() == HashMap.newBuilder[K, V].addAll(xs).result()
  }
  property("HashMapBuilder does not mutate after releasing") = forAll { (xs: Seq[(K, V)], ys: Seq[(K, V)], single: (K, V), addSingleFirst: Boolean) =>
    val b = HashMap.newBuilder[K, V].addAll(xs)

    val hashMapA = b.result()

    val cloneOfA: Map[K, V] = hashMapA.foldLeft(Map.empty[K, V])(_ + _)

    if (addSingleFirst) {
      b.addOne(single)
      b.addAll(ys)
    } else {
      b.addAll(ys)
      b.addOne(single)
    }

    (b.result().size >= hashMapA.size) && hashMapA == cloneOfA
  }
  property("Map does not mutate after releasing") = forAll { (xs: Seq[(K, V)], ys: Seq[(K, V)], single: (K, V), addSingleFirst: Boolean) =>
    val b = Map.newBuilder[K, V].addAll(xs)

    val mapA = b.result()

    val cloneOfA: Map[K, V] = mapA.foldLeft(Map.empty[K, V])(_ + _)

    if (addSingleFirst) {
      b.addOne(single)
      b.addAll(ys)
    } else {
      b.addAll(ys)
      b.addOne(single)
    }

    (b.result().size >= mapA.size) && mapA == cloneOfA
  }

  property("calling result() twice returns the same instance") = forAll { xs: Seq[(K, V)] =>
    val mb = Map.newBuilder[K, V].addAll(xs)
    val hmb = HashMap.newBuilder[K, V].addAll(xs)
    (mb.result() eq mb.result()) && (hmb.result() eq hmb.result())
  }

  property("transform(f) == map { (k, v) => (k, f(k, v)) }") = forAll { (xs: HashMap[K, V], f: (K, V) => String) =>
    xs.transform(f) == xs.map{ case (k, v) => (k, f(k, v)) }
  }

  // Warning: This is not necessarily true if `V` is a primitive, because equality is checked
  // by `eq`, which may not return `true` for boxed primitives that otherwise would be equal.
  property("xs.transform((_, v) => v) eq xs") = forAll { xs: HashMap[K, V] =>
    xs.transform((_, v) => v) eq xs
  }

  property("left.merged(right) { select left } is the same as right concat left") =
    forAll { (left: HashMap[K, V], right: HashMap[K, V]) =>
      left.merged(right)((left, _) => left) == right.concat(left)
    }
  property("left.merged(right) { select right } is the same as left concat right") =
    forAll { (left: HashMap[K, V], right: HashMap[K, V]) =>
      left.merged(right)((_, right) => right) == left.concat(right)
    }
  property("merged passes through all key-values in either map that aren't in the other") =
    forAll { (left: HashMap[K, V], right: HashMap[K, V], mergedValue: V) =>
      val onlyInLeft = left.filter { case (k, _) => !right.contains(k) }
      val onlyInRight = right.filter { case (k, _) => !left.contains(k) }

      val merged = left.merged(right){ case (_, (k, _)) => k -> mergedValue }

      onlyInLeft.forall{ case (k, v) => merged.get(k).contains(v) } &&
      onlyInRight.forall{ case (k, v) => merged.get(k).contains(v) }
    }

  property("merged results in a merged value for all keys that appear in both maps") =
    forAll { (left: HashMap[K, V], right: HashMap[K, V], mergedValue: V) =>
      val intersected = left.filter { case (k, _) => right.contains(k) }

      val merged = left.merged(right){ case (_, (k, _)) => k -> mergedValue }

      intersected.forall { case (k, _) => merged.get(k).contains(mergedValue) }
    }
}
