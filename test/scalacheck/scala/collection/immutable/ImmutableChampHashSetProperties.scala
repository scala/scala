package scala.collection.immutable

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck._

object ImmutableChampHashSetProperties extends Properties("immutable.ChampHashSet") {

  type K = String

//  override def overrideParameters(p: org.scalacheck.Test.Parameters) =
//    p.withMinSuccessfulTests(1000)

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

    val duplicateSet = builder.result
    inputSet == duplicateSet
  }

  property("checkSizeAfterInsertAll") = forAll { (inputValues: HashSet[K]) =>
    val testSet = HashSet.empty[K] ++ inputValues
    inputValues.size == testSet.size
  }

  property("containsAfterInsert") = forAll { (inputValues: HashSet[K]) =>
    var testSet = HashSet.empty[K] ++ inputValues
    inputValues.forall(testSet.contains)
  }

  property("notContainedAfterInsertRemove") = forAll { (input: HashSet[K], item: K) =>
    (input + item - item).contains(item) == false
  }

  property("intersectIdentityReference") = forAll { (inputShared: HashSet[K]) =>
    inputShared == inputShared.intersect(inputShared)
  }

  property("intersectIdentityStructural") = forAll { (inputShared: HashSet[K]) =>
    val builder = HashSet.newBuilder[K]
    inputShared.foreach(builder.addOne)

    val duplicateSet = builder.result
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

    intersectDefault == intersectNative
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

    val duplicateSet = builder.result
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

  property("subtractIdentityReference") = forAll { (inputShared: HashSet[K]) =>
    HashSet.empty[K] == inputShared.diff(inputShared)
  }

  property("subtractIdentityMostlyReference") = forAll { (input: HashSet[K], key: K) =>
    val inputCopy = if (input.contains(key)) input - key + key else input + key - key

    input.diff(inputCopy).isEmpty && inputCopy.diff(input).isEmpty
  }

  property("subtractIdentityStructural") = forAll { (inputShared: HashSet[K]) =>
    val builder = HashSet.newBuilder[K]
    inputShared.foreach(builder.addOne)

    val duplicateSet = builder.result
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

  property("iterator equals reverseIterator.reverse()") = forAll { (input: HashSet[K]) =>
    val xs: List[K] = input.iterator
      .foldLeft(List.empty[K])((list: List[K], item: K) => list prepended item)

    val ys: List[K] = input.reverseIterator
      .foldLeft(List.empty[K])((list: List[K], item: K) => list appended item)

    xs == ys
  }

  property("smaller subsetOf larger") = forAll { (inputSet: HashSet[K]) =>
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

}
