package scala.collection.immutable

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.collection.Hashing

object ImmutableChampHashMapProperties extends Properties("HashMap") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters = p.withInitialSeed(42L)

  type K = Int
  type V = Int
  type T = (K, V)

  //  override def overrideParameters(p: org.scalacheck.Test.Parameters) =
  //    p.withMinSuccessfulTests(1000)

  property("convertToScalaMapAndCheckSize") = forAll { input: HashMap[K, V] =>
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

  property("xs.transform((_, v) => v) eq xs") = forAll { xs: HashMap[K, String] =>
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

  property("rootnode hashCode should be sum of key improved hashcodes") =
    forAll { (seq: Seq[(K, V)]) =>
      val distinct = seq.distinctBy(_._1)
      val expectedHash = distinct.map(_._1.hashCode).map(Hashing.improve).sum
      val b = HashMap.newBuilder[K, V]
      distinct.foreach(b.addOne)
      b.result().rootNode.cachedJavaKeySetHashCode == expectedHash
    }

  property("xs.toList.filter(p).toMap == xs.filter(p)") = forAll { (xs: HashMap[Int, Int], flipped: Boolean) =>
    val p: ((Int, Int)) => Boolean = { case (k, v) => k * v >= 0} // "key and value are the same sign"
    xs.toList.filterImpl(p, flipped).toMap == xs.filterImpl(p, flipped)
  }

  property("xs.filter(p) retains all elements that pass `p`, and only those") =
    forAll { (xs: HashMap[Int, Int], p: ((Int, Int)) => Boolean, flipped: Boolean) =>
      xs.filterImpl(p, flipped) == {
        val builder = HashMap.newBuilder[Int, Int]
        xs.foreach (kv => if (p(kv) != flipped) builder.addOne(kv))
        builder.result()
      }
  }
  property("xs.filter(p) does not perform any hashes") =
    forAll { (xs: HashMap[Int, Int], p: ((Int, Int)) => Boolean, flipped: Boolean) =>
      // container which tracks the number of times its hashCode() is called
      case class Container(inner: Int) {
        var timesHashed = 0
        override def hashCode() = {
            timesHashed += 1
            inner.hashCode()
          }
      }

      val ys: HashMap[Container, Int] = xs.map { case (k, v) => Container(k) -> v }
      val zs = ys.filterImpl( { case (k, v) => p((k.inner, v))}, flipped)

      ys.forall(_._1.timesHashed <= 1)
     }

  property("xs.removeAll(ys) == xs.filterNot(kv => ys.toSet.contains(kv._1))") = forAll { (xs: HashMap[K, V], ys: Iterable[K]) =>
    val ysSet = ys.toSet
    xs.removedAll(ys) == xs.filterNot { case (k, _) => ysSet.contains(k) }
  }
  property("concat(HashMap)") = forAll { (left: HashMap[Int, Int], right: HashMap[Int, Int]) =>
    val expected: collection.Map[Int, Int] = left concat right
    val actual: collection.Map[Int, Int] = (left.toSeq ++ right.toSeq).to(HashMap)

    actual ?= expected
  }

  property("concat(mutable.HashMap)") = forAll { (left: HashMap[Int, Int], right: HashMap[Int, Int]) =>
    val expected: collection.Map[Int, Int] = left concat right
    val actual: collection.Map[Int, Int] = left.concat(right.to(collection.mutable.HashMap.mapFactory))
    actual ?= expected
  }
  property("concat(Vector)") = forAll { (left: HashMap[Int, Int], right: Vector[(Int, Int)]) =>
    val expected: collection.Map[Int, Int] = left.mapFactory.newBuilder.addAll(left).addAll(right).result()
    val actual: collection.Map[Int, Int] = left.concat(right)
    actual ?= expected
  }
  property("removedAll(HashSet)") = forAll { (left: HashMap[Int, Int], right: HashSet[Int]) =>
    val expected: collection.Map[Int, Int] = left.view.filterKeys(!right.contains(_)).toMap
    val actual: collection.Map[Int, Int] = left -- right
    actual ?= expected
  }
  property("removedAll(mutable.HashSet)") = forAll { (left: HashMap[Int, Int], right: HashSet[Int]) =>
    val expected: collection.Map[Int, Int] = left -- right
    val actual: collection.Map[Int, Int] = left -- right.to(collection.mutable.HashSet)
    actual ?= expected
  }
  property("removedAll(Vector)") = forAll { (left: HashMap[Int, Int], right: Vector[Int]) =>
    val expected: collection.Map[Int, Int] = left.view.filterKeys(!right.contains(_)).toMap
    val actual: collection.Map[Int, Int] = left -- right
    actual ?= expected
  }
}
