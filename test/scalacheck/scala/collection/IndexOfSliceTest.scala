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
      Arbitrary.arbitrary[collection.mutable.WrappedArray[Int]]
    )

  property("indexOfSlice(Nil) == 0") =
    forAll(genDifferentSeqs) { seq: Seq[Int] =>
      seq.indexOfSlice(Nil) == 0 && seq.indexOfSlice(LazyList.empty) == 0
    }

  property("indexOfSlice(Nil, from = size + 1) == -1") =
    forAll(genDifferentSeqs) { seq: Seq[Int] =>
      seq.indexOfSlice(Nil, from = seq.size + 1) == -1
    }

}
