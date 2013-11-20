import scala.reflect.ClassTag // new style: use ClassTag
import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._
import util._
import Buildable._
import scala.collection.mutable.ArraySeq

object Test extends Properties("Array") {
  /** At this moment the authentic scalacheck Array Builder/Arb bits are commented out.
   */
  implicit def arbArray[T](implicit a: Arbitrary[T], m: ClassTag[T]): Arbitrary[Array[T]] =
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
    val flattened = arr flatMap (x => x) flatMap (x => x)
    flattened.length == i1 * i2 * i3
  }
}