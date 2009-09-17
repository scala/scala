import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._
import util._
import Buildable._

object Test extends Properties("Array") {
  val myGens: Seq[Gen[Array[_]]] = List(
    arbArray[Int],
    arbArray[Array[Int]],
    arbArray[List[String]],
    arbArray[String],
    arbArray[Boolean],
    arbArray[AnyVal](arbAnyVal)
  ) map (_.arbitrary)

  // inspired by #1857 and #2352
  property("eq/ne") =
    forAll(oneOf(myGens: _*)) { c1 =>
      forAll(oneOf(myGens: _*)) { c2 =>
        (c1 eq c2) || (c1 ne c2)
      }
    }

  def smallInt = choose(1, 10)
  // inspired by #2299
  property("ofDim") = forAll(smallInt) { i1 =>
    forAll(smallInt) { i2 =>
      forAll(smallInt) { i3 =>
        val arr = Array.ofDim[String](i1, i2, i3)
        val flattened = arr flatMap (x => x) flatMap (x => x)

        flattened.length == i1 * i2 * i3
      }
    }
  }
}

