import org.scalacheck._
import Prop._
import Gen._

object Test extends Properties("==") {
  def equalObjectsEqualHashcodes(x: Any, y: Any) = (x != y) || (x == y && x.## == y.##)

  // ticket #2087
  property("short/char") = forAll { (x: Short) => {
      val ch: Char = x.toChar
      (x == ch) == (ch == x)
    }
  }

  property("symmetry") = forAll { (x: AnyVal, y: AnyVal) => (x == y) == (y == x) }
  property("transitivity") = forAll { (x: AnyVal, y: AnyVal, z: AnyVal) => x != y || y != z || x == z }

  property("##") = forAll {
    (x: Short) => {
      val anyvals = List(x.toByte, x.toChar, x, x.toInt, x.toLong, x.toFloat, x.toDouble, BigInt(x), BigDecimal(x))
      val shortAndLarger = anyvals drop 2

      val result = (
        ((anyvals, anyvals).zipped forall equalObjectsEqualHashcodes) &&
        ((shortAndLarger, shortAndLarger).zipped forall (_ == _)) &&
        ((shortAndLarger, shortAndLarger).zipped forall ((x, y) => (x: Any) == (y: Any)))
      )
      result
    }
  }
  property("## 2") = forAll {
    (dv: Double) => {
      val fv = dv.toFloat
      (fv != dv) || (fv.## == dv.##)
    }
  }
}
