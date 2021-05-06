package scala.collection.immutable

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.collection.Hashing

object ImmutableChampHashMapProperties extends Properties("HashMap") {

  type K = Int
  type V = Int
  type T = (K, V)

  property("convertToScalaMapAndCheckSize") = forAll { (input: HashMap[K, V]) =>
    convertToScalaMapAndCheckSize(input)
  }

  private def convertToScalaMapAndCheckSize(input: HashMap[K, V]) = HashMap.from(input).size == input.size

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

    val duplicateMap = builder.result()
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
    b.result() == b.addAll(seq).result()
  }

  property("(xs ++ ys).toMap == xs.toMap ++ ys.toMap") = forAll { (xs: Seq[(K, V)],ys: Seq[(K, V)]) =>
    (xs ++ ys).toMap ?= xs.toMap ++ ys.toMap
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
      left.merged(right)((left, _) => left) ?= right.concat(left)
    }
  property("left.merged(right) { select right } is the same as left concat right") =
    forAll { (left: HashMap[K, V], right: HashMap[K, V]) =>
      left.merged(right)((_, right) => right) ?= left.concat(right)
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

  property("merged with a null merge function is equal to concatenation") =
    forAll { (left: HashMap[K, V], right: HashMap[K, V]) =>
      left.merged(right)(null) == right.concat(left)
    }

  property("merged specification") =
    forAll { (left: HashMap[K, V], right: HashMap[K, V], mergef: ((K, V), (K, V))=> (K, V)) =>
      // A key-by-key specification of the behaviour of merged, for case where mergef != null
      val keysOnlyInLeft: Set[K] = left.keySet -- right.keySet
      val keysOnlyInRight: Set[K] = right.keySet -- left.keySet
      val keysInBoth: Set[K] = left.keySet intersect right.keySet

      val resultsOfMergeFunctionByOriginalKey: Map[K, (K, V)] = {
        val rightKVsByKey: Map[K, (K, V)] = right.iterator.map { case kv@(k, _) => k -> kv }.toMap
        keysInBoth.iterator.map(k => k -> mergef((k, left(k)), rightKVsByKey(k))).toMap
      }

      val resultsOfMergeFunctionByNewKey: Map[K, List[V]] = resultsOfMergeFunctionByOriginalKey.valuesIterator.toList.groupMap(_._1)(_._2)

      val allKeys = keysOnlyInLeft.concat(keysOnlyInRight.iterator.concat(resultsOfMergeFunctionByNewKey.keys))

      val merged = left.merged(right)(mergef)

      keysOnlyInLeft.iterator.map { leftKey =>
        Prop(merged.get(leftKey).contains(left(leftKey)) || resultsOfMergeFunctionByNewKey.contains(leftKey))
          .label(s"Key $leftKey from left(and not right) must be found in merged, or overwritten by the merge function")
      }.concat(
        keysOnlyInRight.iterator.map { rightKey =>
          Prop(merged.get(rightKey).contains(right(rightKey)) || resultsOfMergeFunctionByNewKey.contains(rightKey))
            .label(s"Key $right from right(and not left) must be found in merged, or overwritten by the merge function")
        }
      ).concat(
        keysInBoth.iterator.map { key =>

          val (newKey, _) = resultsOfMergeFunctionByOriginalKey(key)
          Prop(merged.get(newKey).exists { mergedValue =>
            resultsOfMergeFunctionByNewKey(newKey).contains(mergedValue) || // the resulting value could have come from the merge function
              left.get(newKey).contains(mergedValue) || // or it could have been from left
              right.get(newKey).contains(mergedValue) //                             ... or right
          })
            .label(
              s"""Key $key found in left and right, so its merged form ($newKey) must exist in the resulting map, with the value derived from either:
                 |1) this or an other invocation of mergef
                 |2) a value existing in left
                 |3) a value existing in right""".stripMargin)
        }
      ).concat(
        merged.keysIterator.map ( k => Prop(allKeys.contains(k)).label(s"Key $k in merged should not be present"))
      )
      .foldLeft(
        Prop(merged.size <= keysInBoth.size + keysOnlyInLeft.size + keysOnlyInRight.size)
          .label("A merged hashmap mustn't have greater size than leftOnlyKeys + rightOnlyKeys + collidingKeys"))(_ && _)
    }

  property("rootnode hashCode should be sum of key improved hashcodes") =
    forAll { seq: Seq[(K, V)] =>
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

  property("hm.removedAll(list) == list.foldLeft(hm)(_ - _)") = forAll { (hm: HashMap[K, V], l: List[K]) =>
    hm.removedAll(l) ?= l.foldLeft(hm)(_ - _)
  }

  property("hm.removedAll(list) does not mutate hm") = forAll { (hm: HashMap[K, V], l: List[K]) =>
    val clone = hm.to(List).to(HashMap)
    hm.removedAll(l)
    hm ?= clone
  }
  property("hm.concat(list) == list.foldLeft(hm)(_ + _)") = forAll { (hm: HashMap[K, V], l: List[(K, V)]) =>
    hm.concat(l) ?= l.foldLeft(hm)((m, tuple) => m + tuple)
  }

  property("hm.concat(list) does not mutate hm") = forAll { (hm: HashMap[K, V], l: List[(K, V)]) =>
    val clone = hm.to(List).to(HashMap)
    hm.concat(l)
    hm ?= clone
  }

  property("hm.concat(mutable.HashMap) == list.foldLeft(hm)(_ + _)") = forAll { (hm: HashMap[K, V], l: List[(K, V)]) =>
    val mhm = l.to(collection.mutable.HashMap)
    hm.concat(mhm) ?= mhm.foldLeft(hm)((m, tuple) => m + tuple)
  }

  property("hm.concat(mutable.HashMap) does not mutate hm or the mutable.HashMap") = forAll { (hm: HashMap[K, V], l: List[(K, V)]) =>
    val clone = hm.to(List).to(HashMap)
    val mhm = l.to(collection.mutable.HashMap)
    hm.concat(mhm)
    (hm ?= clone).label("hm is unmutated") &&
      ((mhm : collection.Map[K, V]) ?= l.to(HashMap)).label("mhm is unmutated")
  }

  property("hm.removedAll(hashSet) == hashSet.foldLeft(this)(_ - _)") = forAll { (hm: HashMap[K, V], hs: HashSet[K]) =>
    hm.removedAll(hs) ?= hs.foldLeft(hm)(_ - _)
  }
  property("hm.removedAll(hashSet) does not mutate hm") = forAll { (hm: HashMap[K, V], hs: HashSet[K]) =>
    val clone = hm.to(List).to(HashMap)
    hm.removedAll(hs)
    hm ?= clone
  }
  property("hm.removedAll(mutable.hashSet) == hashSet.foldLeft(this)(_ - _)") = forAll { (hm: HashMap[K, V], hs: HashSet[K]) =>
    val mhs = hs.to(collection.mutable.HashSet)
    hm.removedAll(mhs) ?= mhs.foldLeft(hm)(_ - _)
  }
  property("hm.removedAll(mutable.hashSet) does not mutate hm or the mutable.hashSet") = forAll { (hm: HashMap[K, V], hs: HashSet[K]) =>
    val clone = hm.to(List).to(HashMap)
    val mhs = hs.to(collection.mutable.HashSet)
    hm.removedAll(mhs)
    (hm ?= clone).label("hm is unmutated") &&
      ((mhs : collection.Set[K]) ?= hs).label("mhs is unmutated")
  }

  property("xs.keySet + k == xs.map(_._1).to(Set) + k") = forAll { (hm: HashMap[K, V], k: K) =>
    (hm.keySet + k) ?= (hm.map((kv: (K, V)) => kv._1).to(Set) + k)
  }
  property("xs.keySet - k == xs.map(_._1).to(Set) - k") = forAll { (hm: HashMap[K, V], k: K) =>
    (hm.keySet - k) ?= (hm.map((kv: (K, V)) => kv._1).to(Set) - k)
  }
  property("xs.keySet removedAll ys == xs.map(_._1).to(Set) removedAll ys") = forAll { (hm: HashMap[K, V], ys: List[K]) =>
    (hm.keySet.removedAll(ys) ?= hm.map((kv: (K, V)) => kv._1).to(Set).removedAll(ys)).label("RemovedAll(List)") &&
      (hm.keySet.removedAll(ys.toSet) ?= hm.map((kv: (K, V)) => kv._1).to(Set).removedAll(ys)).label("RemovedAll(Set)")
  }
  property("xs.keySet filter p == xs.map(_._1).to(Set) filter p") = forAll { (hm: HashMap[K, V], p: (K => Boolean)) =>
    hm.keySet.filter(p) ?= hm.to(List).map(_._1).to(Set).filter(p)
  }

  property("xs.keySet filterNot p == xs.map(_._1).to(Set) filterNot p") = forAll { (hm: HashMap[K, V], p: (K => Boolean)) =>
    hm.keySet.filterNot(p) ?= hm.to(List).map(_._1).to(Set).filterNot(p)
  }
  property("xs.keySet ks == xs.map(_._1).to(Set) concat ks") = forAll { (hm: HashMap[K, V], ks: List[K]) =>
    hm.keySet.concat(ks) ?= hm.to(List).map(_._1).to(Set).concat(ks)
  }
  property("xs.keySet diff ks == xs.map(_._1).to(Set) diff ks") = forAll { (hm: HashMap[K, V], ks: Set[K]) =>
    hm.keySet.diff(ks) ?= hm.to(List).map(_._1).to(Set).diff(ks)
  }

  property("xs.removed(key) eq xs when !xs.contains(key)") = forAll { (hm: HashMap[K, V], key: K) =>
    Prop(!hm.contains(key)).label("HashMap does not contain key") ==>
      Prop(hm.removed(key) eq hm).label("hashMap.removed(key) eq hashMap")
  }

}
