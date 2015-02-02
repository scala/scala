import org.scalacheck._
import Gen._
import Prop._

object Test extends Properties("NaN-Ordering") {

  val specFloats: Gen[Float] = oneOf(
    Float.MaxValue,
    Float.MinPositiveValue,
    Float.MinValue,
    Float.NaN,
    Float.NegativeInfinity,
    Float.PositiveInfinity,
    -0.0f,
    +0.0f
  )

  property("Float min") = forAll(specFloats, specFloats) { (d1, d2) => {
      val mathmin = math.min(d1, d2)
      val numericmin = d1 min d2
      mathmin == numericmin || mathmin.isNaN && numericmin.isNaN
    }
  }

  property("Float max") = forAll(specFloats, specFloats) { (d1, d2) => {
      val mathmax = math.max(d1, d2)
      val numericmax = d1 max d2
      mathmax == numericmax || mathmax.isNaN && numericmax.isNaN
    }
  }

  val numFloat = implicitly[Numeric[Float]]

  property("Float lt") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.lt(d1, d2) == d1 < d2 }

  property("Float lteq") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.lteq(d1, d2) == d1 <= d2 }

  property("Float gt") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.gt(d1, d2) == d1 > d2 }

  property("Float gteq") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.gteq(d1, d2) == d1 >= d2 }

  property("Float equiv") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.equiv(d1, d2) == (d1 == d2) }

  property("Float reverse.min") = forAll(specFloats, specFloats) { (d1, d2) => {
      val mathmax = math.max(d1, d2)
      val numericmin = numFloat.reverse.min(d1, d2)
      mathmax == numericmin || mathmax.isNaN && numericmin.isNaN
    }
  }

  property("Float reverse.max") = forAll(specFloats, specFloats) { (d1, d2) => {
      val mathmin = math.min(d1, d2)
      val numericmax = numFloat.reverse.max(d1, d2)
      mathmin == numericmax || mathmin.isNaN && numericmax.isNaN
    }
  }

  property("Float reverse.lt") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.reverse.lt(d1, d2) == d2 < d1 }

  property("Float reverse.lteq") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.reverse.lteq(d1, d2) == d2 <= d1 }

  property("Float reverse.gt") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.reverse.gt(d1, d2) == d2 > d1 }

  property("Float reverse.gteq") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.reverse.gteq(d1, d2) == d2 >= d1 }

  property("Float reverse.equiv") = forAll(specFloats, specFloats) { (d1, d2) => numFloat.reverse.equiv(d1, d2) == (d1 == d2) }


  val specDoubles: Gen[Double] = oneOf(
    Double.MaxValue,
    Double.MinPositiveValue,
    Double.MinValue,
    Double.NaN,
    Double.NegativeInfinity,
    Double.PositiveInfinity,
    -0.0,
    +0.0
  )

  // ticket #5104
  property("Double min") = forAll(specDoubles, specDoubles) { (d1, d2) => {
    val mathmin = math.min(d1, d2)
    val numericmin = d1 min d2
    mathmin == numericmin || mathmin.isNaN && numericmin.isNaN
  }
  }

  property("Double max") = forAll(specDoubles, specDoubles) { (d1, d2) => {
    val mathmax = math.max(d1, d2)
    val numericmax = d1 max d2
    mathmax == numericmax || mathmax.isNaN && numericmax.isNaN
  }
  }

  val numDouble = implicitly[Numeric[Double]]

  property("Double lt") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.lt(d1, d2) == d1 < d2 }

  property("Double lteq") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.lteq(d1, d2) == d1 <= d2 }

  property("Double gt") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.gt(d1, d2) == d1 > d2 }

  property("Double gteq") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.gteq(d1, d2) == d1 >= d2 }

  property("Double equiv") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.equiv(d1, d2) == (d1 == d2) }

  property("Double reverse.min") = forAll(specDoubles, specDoubles) { (d1, d2) => {
    val mathmax = math.max(d1, d2)
    val numericmin = numDouble.reverse.min(d1, d2)
    mathmax == numericmin || mathmax.isNaN && numericmin.isNaN
  }
  }

  property("Double reverse.max") = forAll(specDoubles, specDoubles) { (d1, d2) => {
    val mathmin = math.min(d1, d2)
    val numericmax = numDouble.reverse.max(d1, d2)
    mathmin == numericmax || mathmin.isNaN && numericmax.isNaN
  }
  }

  property("Double reverse.lt") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.reverse.lt(d1, d2) == d2 < d1 }

  property("Double reverse.lteq") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.reverse.lteq(d1, d2) == d2 <= d1 }

  property("Double reverse.gt") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.reverse.gt(d1, d2) == d2 > d1 }

  property("Double reverse.gteq") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.reverse.gteq(d1, d2) == d2 >= d1 }

  property("Double reverse.equiv") = forAll(specDoubles, specDoubles) { (d1, d2) => numDouble.reverse.equiv(d1, d2) == (d1 == d2) }
}
