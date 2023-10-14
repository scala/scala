package scala.collection.immutable

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

object ImmutableChampHashSetProperties extends Properties("immutable.HashSet") {

  type K = Int

  private def doSubtract(one: HashSet[K], two: HashSet[K]) = {
    one.foldLeft(HashSet.empty[K])((result, elem) => if (two contains elem) result else result + elem)
  }

  private def doIntersect(one: HashSet[K], two: HashSet[K]) = {
    one.foldLeft(HashSet.empty[K])((result, elem) => if (two contains elem) result + elem else result)
  }

  property("convertToScalaSetAndCheckSize") = forAll { (input: HashSet[K]) =>
    convertToScalaSetAndCheckSize(input)
  }

  private def convertToScalaSetAndCheckSize(input: HashSet[K]) =
    HashSet.from(input).size == input.size

  property("convertToScalaSetAndCheckHashCode") = forAll { (input: HashSet[K]) =>
    convertToScalaSetAndCheckHashCode(input)
  }

  private def convertToScalaSetAndCheckHashCode(input: HashSet[K]) =
    HashSet.from(input).hashCode == input.hashCode

  property("convertToScalaSetAndCheckEquality") = forAll { (input: HashSet[K]) =>
    input == HashSet.from(input) && HashSet.from(input) == input
  }

  property("input.equals(duplicate)") = forAll { (inputSet: HashSet[K]) =>
    val builder = HashSet.newBuilder[K]
    inputSet.foreach(builder.addOne)

    val duplicateSet = builder.result()
    inputSet == duplicateSet
  }

  property("checkSizeAfterInsertAll") = forAll { (inputValues: HashSet[K]) =>
    val testSet = HashSet.empty[K] ++ inputValues
    inputValues.size == testSet.size
  }

  property("containsAfterInsert") = forAll { (inputValues: HashSet[K]) =>
    val testSet = HashSet.empty[K] ++ inputValues
    inputValues.forall(testSet.contains)
  }

  property("notContainedAfterInsertRemove") = forAll { (input: HashSet[K], item: K) =>
    !(input + item - item).contains(item)
  }

  property("intersectIdentityReference") = forAll { (inputShared: HashSet[K]) =>
    inputShared == inputShared.intersect(inputShared)
  }

  property("intersectIdentityStructural") = forAll { (inputShared: HashSet[K]) =>
    val builder = HashSet.newBuilder[K]
    inputShared.foreach(builder.addOne)

    val duplicateSet = builder.result()
    inputShared == inputShared.intersect(duplicateSet)
  }

  property("intersect") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K], inputShared: HashSet[K]) =>
    val oneWithoutShared = doSubtract(doSubtract(inputOne, inputShared), inputTwo)
    val twoWithoutShared = doSubtract(doSubtract(inputTwo, inputShared), inputOne)

    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared
    val intersectedWithShared = oneWithShared.intersect(twoWithShared)

    val predicate1 = inputShared.forall(intersectedWithShared.contains)
    val predicate2 = !intersectedWithShared.exists(oneWithoutShared.contains)
    val predicate3 = !intersectedWithShared.exists(twoWithoutShared.contains)

    predicate1 && predicate2 && predicate3
  }

  property("intersectMaintainsSizeAndHashCode") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K], inputShared: HashSet[K]) =>
    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared
    val intersectedWithShared = oneWithShared.intersect(twoWithShared)

    convertToScalaSetAndCheckSize(intersectedWithShared) && convertToScalaSetAndCheckHashCode(intersectedWithShared)
  }

  property("intersectEqualToDefaultImplementation") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K], inputShared: HashSet[K]) =>
    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared

    val intersectNative = oneWithShared.intersect(twoWithShared)
    val intersectDefault = doIntersect(oneWithShared, twoWithShared)

    intersectDefault =? intersectNative
  }

  property("intersectIdentityMostlyReference") = forAll { (input: HashSet[K], key: K) =>
    val inputCopy = if (input.contains(key)) input - key + key else input + key - key

    input == input.intersect(inputCopy) && inputCopy.intersect(input) == input
  }

  property("unionIdentityReference") = forAll { (inputShared: HashSet[K]) =>
    inputShared == inputShared.union(inputShared)
  }

  property("unionIdentityMostlyReference") = forAll { (input: HashSet[K], key: K) =>
    val inputCopy = if (input.contains(key)) input - key + key else input + key - key

    input == input.union(inputCopy) && inputCopy.union(input) == input
  }

  property("unionIdentityStructural") = forAll { (inputShared: HashSet[K]) =>
    val builder = HashSet.newBuilder[K]
    inputShared.foreach(builder.addOne)

    val duplicateSet = builder.result()
    inputShared == inputShared.union(duplicateSet)
  }

  property("union") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K]) =>
    val unioned = inputOne.intersect(inputTwo)

    unioned.forall(inputOne.contains) && unioned.forall(inputTwo.contains)
  }

  property("unionMaintainsSizeAndHashCode") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K]) =>
    val unioned = inputOne.union(inputTwo)

    convertToScalaSetAndCheckSize(unioned) && convertToScalaSetAndCheckHashCode(unioned)
  }

  property("unionEqualToDefaultImplementation") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K]) =>
    inputOne ++ inputTwo == inputOne.union(inputTwo)
  }

  property("subtract") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K]) =>
    val subtracted = inputOne.diff(inputTwo)

    subtracted.forall(inputOne.contains) && !subtracted.exists(inputTwo.contains)
  }

  property("subtractMaintainsSizeAndHashCode") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K]) =>
    val subtracted = inputOne.diff(inputTwo)
    convertToScalaSetAndCheckSize(subtracted) && convertToScalaSetAndCheckHashCode(subtracted)
  }

  property("subtractIdentityReference") = forAll { inputShared: HashSet[K] =>
    HashSet.empty[K] == inputShared.diff(inputShared)
  }

  property("subtractIdentityMostlyReference") = forAll { (input: HashSet[K], key: K) =>
    val inputCopy = if (input.contains(key)) input - key + key else input + key - key

    input.diff(inputCopy).isEmpty && inputCopy.diff(input).isEmpty
  }

  property("subtractIdentityStructural") = forAll { inputShared: HashSet[K] =>
    val builder = HashSet.newBuilder[K]
    inputShared.foreach(builder.addOne)

    val duplicateSet = builder.result()
    HashSet.empty[K] == inputShared.diff(duplicateSet)
  }

  property("subtract") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K], inputShared: HashSet[K]) =>
    val oneWithoutShared = inputOne diff inputShared diff inputTwo
    val twoWithoutShared = inputTwo diff inputShared diff inputOne

    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared
    val subtractedWithShared = oneWithShared diff twoWithShared

    val predicate1 = !inputShared.exists(subtractedWithShared.contains)
    val predicate2 = subtractedWithShared.forall(oneWithoutShared.contains)
    val predicate3 = !subtractedWithShared.exists(twoWithoutShared.contains)

    predicate1 && predicate2 && predicate3
  }

  property("subtractMaintainsSizeAndHashCode") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K], inputShared: HashSet[K]) =>
    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared
    val subtractedWithShared = oneWithShared diff twoWithShared

    convertToScalaSetAndCheckSize(subtractedWithShared) && convertToScalaSetAndCheckHashCode(subtractedWithShared)
  }

  property("subtractEqualToDefaultImplementation") = forAll { (inputOne: HashSet[K], inputTwo: HashSet[K], inputShared: HashSet[K]) =>
    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared

    val subtractNative = oneWithShared diff twoWithShared
    val subtractDefault = doSubtract(oneWithShared, twoWithShared)

    subtractDefault == subtractNative
  }

  property("iterator equals reverseIterator.reverse()") = forAll { input: HashSet[K] =>
    val xs: List[K] = input.iterator
      .foldLeft(List.empty[K])((list: List[K], item: K) => list prepended item)

    val ys: List[K] = input.reverseIterator
      .foldLeft(List.empty[K])((list: List[K], item: K) => list appended item)

    xs == ys
  }

  property("smaller subsetOf larger") = forAll { inputSet: HashSet[K] =>
    if (inputSet.isEmpty) {
      true
    } else {
      val randomSamples = scala.util.Random.nextInt(inputSet.size)
      val randomIndices = ArraySeq.fill(randomSamples)(scala.util.Random.nextInt(inputSet.size))

      val inputArray = inputSet.toArray
      val smallerSet = HashSet.from(randomIndices.map(inputArray).toSet)

      smallerSet.subsetOf(inputSet)
    }
  }

  property("building element-wise is the same as in bulk") = forAll { seq: Seq[K] =>
    val xs = (HashSet.newBuilder[K] ++= seq).result()
    val ys = {
      val b = HashSet.newBuilder[K]
      seq.foreach(b += _)
      b.result()
    }
    var zs = HashSet.empty[K]
    seq.foreach(zs += _)
    xs == ys && xs == zs
  }
  property("adding elems twice to builder is the same as adding them once") = forAll { seq: Seq[K] =>
    val b = HashSet.newBuilder[K].addAll(seq)
    b.result() == b.addAll(seq).result()
  }
  property("(xs ++ ys).toSet == xs.toSet ++ ys.toSet") = forAll { (xs: Seq[K],ys: Seq[K]) =>
    (xs ++ ys).toSet =? xs.toSet ++ ys.toSet
  }
  property("HashSetBuilder produces the same Set as SetBuilder") = forAll { (xs: Seq[K]) =>
    HashSet.newBuilder[K].addAll(xs).result() =? HashSet.newBuilder[K].addAll(xs).result()
  }
  property("HashSetBuilder does not mutate after releasing") = forAll { (xs: Seq[K], ys: Seq[K], single: K, addSingleFirst: Boolean) =>
    val b = HashSet.newBuilder[K].addAll(xs)
    val hashSetA = b.result()
    val cloneOfA: Set[K] = hashSetA.foldLeft(Set.empty[K])(_ + _)
    if (addSingleFirst) {
      b.addOne(single)
      b.addAll(ys)
    } else {
      b.addAll(ys)
      b.addOne(single)
    }
    Prop(b.result().size >= hashSetA.size) && ((hashSetA: Set[K]) ?= (cloneOfA: Set[K]))
  }
  property("Set does not mutate after releasing") = forAll { (xs: Seq[K], ys: Seq[K], single: K, addSingleFirst: Boolean) =>
    val b = Set.newBuilder[K].addAll(xs)
    val setA = b.result()
    val cloneOfA: Set[K] = setA.foldLeft(Set.empty[K])(_ + _)
    if (addSingleFirst) {
      b.addOne(single)
      b.addAll(ys)
    } else {
      b.addAll(ys)
      b.addOne(single)
    }
    (b.result().size >= setA.size) && setA == cloneOfA
  }
  property("calling result() twice returns the same instance") = forAll { xs: Seq[K] =>
    val mb = Set.newBuilder[K].addAll(xs)
    val hmb = Set.newBuilder[K].addAll(xs)
    (mb.result() eq mb.result()) && (hmb.result() eq hmb.result())
  }

  property("xs.toList.toSet == xs") = forAll { xs: Set[K] =>
    xs.toList.toSet == xs
  }

  property("(xs - elem) == xs.toList.filterNot(_ == elem).toSet") = forAll { (xs: Set[K], elem: K) =>
    (xs - elem) == xs.toList.filterNot(_ == elem).toSet
  }

  property("(xs + elem) == (xs.toList :+ elem).toSet") = forAll { (xs: Set[K], elem: K) =>
    (xs + elem) == (xs.toList :+ elem).toSet
  }
  property("xs -- range == xs -- range.toSet") = forAll { (xs: Set[Int], rangeMax: Int) =>
    val range = 1 to (rangeMax % 10000)
    xs -- range == xs -- range.toSet
  }

  property("concat(mutable.HashSet)") = forAll { (left: HashSet[Int], right: HashSet[Int]) =>
    val expected: collection.Set[Int] = left concat right
    val actual: collection.Set[Int] = left.concat(right.to(collection.mutable.HashSet))
    actual ?= expected
  }
  property("concat(Vector)") = forAll { (left: HashSet[Int], right: Vector[Int]) =>
    val expected: collection.Set[Int] = left.iterableFactory.newBuilder[Int].addAll(left).addAll(right).result()
    val actual: collection.Set[Int] = left.concat(right)
    actual ?= expected
  }
  property("removedAll(HashSet)") = forAll { (left: HashSet[Int], right: HashSet[Int]) =>
    val expected: collection.Set[Int] = left.filter(!right.contains(_))
    val actual: collection.Set[Int] = left -- right
    actual ?= expected
  }
  property("removedAll(mutable.HashSet)") = forAll { (left: HashSet[Int], right: HashSet[Int]) =>
    val expected: collection.Set[Int] = left -- right
    val actual: collection.Set[Int] = left -- right.to(collection.mutable.HashSet)
    actual ?= expected
  }

  property("removedAll(Vector)") = forAll { (left: HashSet[Int], right: Vector[Int]) =>
    val expected: collection.Set[Int] = left.filter(!right.contains(_))
    val actual: collection.Set[Int] = left -- right
    actual ?= expected
  }
  property("(xs.filter(p) == xs.toList.filter(p).toSet") = forAll { xs: HashSet[Int] =>
    val isEven = (i: Int) => i % 2 == 0
    xs.toList.filter(isEven).toSet =? xs.filter(isEven)
  }
  property("(xs.filterNot(p) == xs.toList.filterNot(p).toSet") = forAll { xs: HashSet[Int] =>
    val isEven = (i: Int) => i % 2 == 0
    xs.toList.filterNot(isEven).toSet =? xs.filterNot(isEven)
  }

  property("xs ++ list == list.foldLeft(xs)(_ + _)") = forAll { (xs: HashSet[Int], list: List[Int]) =>
    xs.concat(list) ?= list.foldLeft(xs)(_ + _)
  }


  property("hs.concat(list) does not mutate hs") = forAll { (hs: HashSet[K], l: List[K]) =>
    val asList = hs.toList
    val clone = hs.to(List).to(HashSet)
    hs.concat(l)
    hs ?= clone
  }
  property("hs.concat(mutable.HashSet) does not mutate hs or the mutable.HashSet") = forAll { (hs: HashSet[K], l: List[K]) =>
    val mhs: collection.Set[K] = l.to(collection.mutable.HashSet)
    val clone = hs.to(List).to(HashSet)
    hs.concat(mhs)
    hs ?= clone
  }
  property("hs.concat(mutable.HashSet)") = forAll { (hs: HashSet[K], l: List[K]) =>
    val mhs: collection.Set[K] = l.to(collection.mutable.HashSet)
    hs.concat(mhs) ?= mhs.foldLeft(hs)(_ + _)
  }

  property("hs.union(hashSet) does not mutate hs") = forAll { (hs: HashSet[K], hs2: HashSet[K]) =>
    val clone = hs.to(List).to(HashSet)
    hs.union(hs2)
    hs ?= clone
  }
  property("hs.removedAll(list) does not mutate hs") = forAll { (hs: HashSet[K], l: List[K]) =>
    val clone = hs.to(List).to(HashSet)
    hs.removedAll(l)
    hs ?= clone
  }
  property("hs.diff(hashSet) does not mutate hs") = forAll { (hs: HashSet[K], hs2: HashSet[K]) =>
    val clone = hs.to(List).to(HashSet)
    hs.diff(hs2)
    hs ?= clone
  }
  property("hs.removedAll(hashSet) does not mutate hs") = forAll { (hs: HashSet[K], hs2: HashSet[K]) =>
    val clone = hs.to(List).to(HashSet)
    hs.removedAll(hs2)
    hs ?= clone
  }
  property("t11551") = forAll { (x: HashSet[AnyVal], y: HashSet[AnyVal]) =>
    val z = x ++ y
    z.size ?= z.toList.toSet.size
  }
}
