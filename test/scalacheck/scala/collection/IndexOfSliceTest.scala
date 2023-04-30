package scala.collection

import scala.collection.immutable.{LazyList, Nil}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop._

object IndexOfSliceTest extends Properties("indexOfSlice") {

  // The default arbitrary[Seq[Int]] picks only one Seq implementation.
  // Here we explicitly list all the implementations we want to test
  @annotation.nowarn("msg=type WrappedArray")
  val genDifferentSeqs =
    Gen.oneOf[Seq[Int]](
      Arbitrary.arbitrary[collection.immutable.List[Int]],
      Arbitrary.arbitrary[collection.immutable.Vector[Int]],
      Arbitrary.arbitrary[collection.immutable.ArraySeq[Int]],
      Arbitrary.arbitrary[collection.mutable.ListBuffer[Int]],
      Arbitrary.arbitrary[collection.mutable.ArrayBuffer[Int]],
      Arbitrary.arbitrary[collection.mutable.WrappedArray[Int]],
    )
  val genDifferentEmpties =
    Gen.oneOf[Seq[Int]](
      List.empty[Int],
      Vector.empty[Int],
      LazyList.empty[Int],
    )
  val genPositive = Gen.chooseNum(1, Int.MaxValue)

  property("indexOfSlice(empty) == 0") =
    forAll(genDifferentSeqs, genDifferentEmpties) { (seq: Seq[Int], empty: Seq[Int]) =>
      seq.indexOfSlice(empty) == 0
    }

  property("indexOfSlice(Nil, from = size) == size") =
    forAll(genDifferentSeqs, genDifferentEmpties) { (seq: Seq[Int], empty: Seq[Int]) =>
      seq.indexOfSlice(empty, from = seq.size) == seq.size
    }

  property("indexOfSlice(Nil, from = size + N) == -1") =
    forAll(genDifferentSeqs, genDifferentEmpties, genPositive) { (seq: Seq[Int], empty: Seq[Int], excess: Int) =>
      val from = math.min(Int.MaxValue.toLong, seq.size.toLong + excess)
      seq.indexOfSlice(empty, from = from.toInt) == -1
    }

}
