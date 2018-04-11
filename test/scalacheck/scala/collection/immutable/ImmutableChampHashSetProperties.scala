package scala.collection.immutable

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.scalacheck._

object ImmutableChampHashSetProperties extends Properties("immutable.ChampHashSet") {

  type K = String

//  override def overrideParameters(p: org.scalacheck.Test.Parameters) =
//    p.withMinSuccessfulTests(1000)

  private def doSubtract(one: ChampHashSet[K], two: ChampHashSet[K]) = {
    one.foldLeft(ChampHashSet.empty[K])((result, elem) => if (two contains elem) result else result + elem)
  }

  private def doIntersect(one: ChampHashSet[K], two: ChampHashSet[K]) = {
    one.foldLeft(ChampHashSet.empty[K])((result, elem) => if (two contains elem) result + elem else result)
  }

  property("convertToScalaSetAndCheckSize") = forAll { (input: ChampHashSet[K]) =>
    convertToScalaSetAndCheckSize(input)
  }

  private def convertToScalaSetAndCheckSize(input: ChampHashSet[K]) =
    HashSet.from(input).size == input.size

  property("convertToScalaSetAndCheckHashCode") = forAll { (input: ChampHashSet[K]) =>
    convertToScalaSetAndCheckHashCode(input)
  }

  private def convertToScalaSetAndCheckHashCode(input: ChampHashSet[K]) =
    HashSet.from(input).hashCode == input.hashCode

  property("convertToScalaSetAndCheckEquality") = forAll { (input: ChampHashSet[K]) =>
    input == HashSet.from(input) && HashSet.from(input) == input
  }

  property("input.equals(duplicate)") = forAll { (inputSet: ChampHashSet[K]) =>
    val builder = ChampHashSet.newBuilder[K]()
    inputSet.foreach(builder.addOne)

    val duplicateSet = builder.result
    inputSet == duplicateSet
  }

  property("checkSizeAfterInsertAll") = forAll { (inputValues: HashSet[K]) =>
    val testSet = ChampHashSet.empty[K] ++ inputValues
    inputValues.size == testSet.size
  }

  property("containsAfterInsert") = forAll { (inputValues: HashSet[K]) =>
    var testSet = ChampHashSet.empty[K] ++ inputValues
    inputValues.forall(testSet.contains)
  }

  property("notContainedAfterInsertRemove") = forAll { (input: ChampHashSet[K], item: K) =>
    (input + item - item).contains(item) == false
  }

  property("intersectIdentityReference") = forAll { (inputShared: ChampHashSet[K]) =>
    inputShared == inputShared.intersect(inputShared)
  }

  property("intersectIdentityStructural") = forAll { (inputShared: ChampHashSet[K]) =>
    val builder = ChampHashSet.newBuilder[K]()
    inputShared.foreach(builder.addOne)

    val duplicateSet = builder.result
    inputShared == inputShared.intersect(duplicateSet)
  }

  property("intersect") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K], inputShared: ChampHashSet[K]) =>
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

  property("intersectMaintainsSizeAndHashCode") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K], inputShared: ChampHashSet[K]) =>
    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared
    val intersectedWithShared = oneWithShared.intersect(twoWithShared)

    convertToScalaSetAndCheckSize(intersectedWithShared) && convertToScalaSetAndCheckHashCode(intersectedWithShared)
  }

  property("intersectEqualToDefaultImplementation") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K], inputShared: ChampHashSet[K]) =>
    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared

    val intersectNative = oneWithShared.intersect(twoWithShared)
    val intersectDefault = doIntersect(oneWithShared, twoWithShared)

    intersectDefault == intersectNative
  }

  property("intersectIdentityMostlyReference") = forAll { (input: ChampHashSet[K], key: K) =>
    val inputCopy = if (input.contains(key)) input - key + key else input + key - key

    input == input.intersect(inputCopy) && inputCopy.intersect(input) == input
  }

  property("unionIdentityReference") = forAll { (inputShared: ChampHashSet[K]) =>
    inputShared == inputShared.union(inputShared)
  }

  property("unionIdentityMostlyReference") = forAll { (input: ChampHashSet[K], key: K) =>
    val inputCopy = if (input.contains(key)) input - key + key else input + key - key

    input == input.union(inputCopy) && inputCopy.union(input) == input
  }

  property("unionIdentityStructural") = forAll { (inputShared: ChampHashSet[K]) =>
    val builder = ChampHashSet.newBuilder[K]()
    inputShared.foreach(builder.addOne)

    val duplicateSet = builder.result
    inputShared == inputShared.union(duplicateSet)
  }

  property("union") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K]) =>
    val unioned = inputOne.intersect(inputTwo)

    unioned.forall(inputOne.contains) && unioned.forall(inputTwo.contains)
  }

  property("unionMaintainsSizeAndHashCode") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K]) =>
    val unioned = inputOne.union(inputTwo)

    convertToScalaSetAndCheckSize(unioned) && convertToScalaSetAndCheckHashCode(unioned)
  }

  property("unionEqualToDefaultImplementation") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K]) =>
    inputOne ++ inputTwo == inputOne.union(inputTwo)
  }

  property("subtract") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K]) =>
    val subtracted = inputOne.diff(inputTwo)

    subtracted.forall(inputOne.contains) && !subtracted.exists(inputTwo.contains)
  }

  property("subtractMaintainsSizeAndHashCode") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K]) =>
    val subtracted = inputOne.diff(inputTwo)
    convertToScalaSetAndCheckSize(subtracted) && convertToScalaSetAndCheckHashCode(subtracted)
  }

  property("subtractIdentityReference") = forAll { (inputShared: ChampHashSet[K]) =>
    ChampHashSet.empty[K] == inputShared.diff(inputShared)
  }

  property("subtractIdentityMostlyReference") = forAll { (input: ChampHashSet[K], key: K) =>
    val inputCopy = if (input.contains(key)) input - key + key else input + key - key

    input.diff(inputCopy).isEmpty && inputCopy.diff(input).isEmpty
  }

  property("subtractIdentityStructural") = forAll { (inputShared: ChampHashSet[K]) =>
    val builder = ChampHashSet.newBuilder[K]()
    inputShared.foreach(builder.addOne)

    val duplicateSet = builder.result
    ChampHashSet.empty[K] == inputShared.diff(duplicateSet)
  }

  property("subtract") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K], inputShared: ChampHashSet[K]) =>
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

  property("subtractMaintainsSizeAndHashCode") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K], inputShared: ChampHashSet[K]) =>
    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared
    val subtractedWithShared = oneWithShared diff twoWithShared

    convertToScalaSetAndCheckSize(subtractedWithShared) && convertToScalaSetAndCheckHashCode(subtractedWithShared)
  }

  property("subtractEqualToDefaultImplementation") = forAll { (inputOne: ChampHashSet[K], inputTwo: ChampHashSet[K], inputShared: ChampHashSet[K]) =>
    val oneWithShared = inputOne ++ inputShared
    val twoWithShared = inputTwo ++ inputShared

    val subtractNative = oneWithShared diff twoWithShared
    val subtractDefault = doSubtract(oneWithShared, twoWithShared)

    subtractDefault == subtractNative
  }

  property("iterator() equals reverseIterator().reverse()") = forAll { (input: ChampHashSet[K]) =>
    val xs: List[K] = input.iterator()
      .foldLeft(List.empty[K])((list: List[K], item: K) => list prepended item)

    val ys: List[K] = input.reverseIterator()
      .foldLeft(List.empty[K])((list: List[K], item: K) => list appended item)

    xs == ys
  }

  property("smaller subsetOf larger") = forAll { (inputSet: ChampHashSet[K]) =>
    if (inputSet.isEmpty) {
      true
    } else {
      val randomSamples = scala.util.Random.nextInt(inputSet.size)
      val randomIndices = ImmutableArray.fill(randomSamples)(scala.util.Random.nextInt(inputSet.size))

      val inputArray = inputSet.toArray
      val smallerSet = ChampHashSet.from(randomIndices.map(inputArray).toSet)

      smallerSet.subsetOf(inputSet)
    }
  }

}
