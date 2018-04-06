import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._
import util._
import Buildable._
import scala.collection.mutable.ArraySeq

import scala.Predef.{ refArrayOps => _, genericArrayOps => _, genericWrapArray => _, wrapRefArray => _, _ }
import strawman.collection.arrayToArrayOps
import strawman.collection.IterableOnce

object ArrayOldTest extends Properties("Array") {
  /** At this moment the authentic scalacheck Array Builder/Arb bits are commented out.
   */
  implicit def arbArray[T](implicit a: Arbitrary[T], m: Manifest[T]): Arbitrary[Array[T]] =
    Arbitrary(containerOf[List,T](arbitrary[T]) map (_.toArray))

  val arrGen: Gen[Array[_]] = oneOf(
    arbitrary[Array[Int]],
    arbitrary[Array[Array[Int]]],
    arbitrary[Array[List[String]]],
    arbitrary[Array[String]],
    arbitrary[Array[Boolean]],
    arbitrary[Array[AnyVal]]
  )

  // inspired by #1857 and #2352
  property("eq/ne") = forAll(arrGen, arrGen) { (c1, c2) =>
    (c1 eq c2) || (c1 ne c2)
  }

  // inspired by #2299
  def smallInt = choose(1, 10)
  property("ofDim") = forAll(smallInt, smallInt, smallInt) { (i1, i2, i3) =>
    val arr = Array.ofDim[String](i1, i2, i3)
    val flattened = arr flatMap (x => (x: IterableOnce[Array[String]])) flatMap (x => x: IterableOnce[String])
    flattened.length == i1 * i2 * i3
  }
}

