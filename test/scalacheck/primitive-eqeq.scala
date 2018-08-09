import org.scalacheck._
import Prop._
import Gen._

object PrimitiveEqEqTest extends Properties("==") {
  def equalObjectsEqualHashcodes(x: Any, y: Any) = (x != y) || (x == y && x.## == y.##)

  // ticket #2087
  property("short/char") = forAll { x: Short =>
    val ch: Char = x.toChar
    (x == ch) == (ch == x)
  }

  property("symmetry") = forAll { (x: AnyVal, y: AnyVal) => (x == y) == (y == x) }
  property("transitivity") = forAll { (x: AnyVal, y: AnyVal, z: AnyVal) => x != y || y != z || x == z }

  property("##") = forAll { x: Short =>
    val anyvals = List(x.toByte, x.toChar, x, x.toInt, x.toLong, x.toFloat, x.toDouble, BigInt(x), BigDecimal(x))
    val shortAndLarger = anyvals drop 2

    (anyvals.lazyZip(anyvals) forall equalObjectsEqualHashcodes) &&
    (shortAndLarger.lazyZip(shortAndLarger) forall (_ == _)) &&
    (shortAndLarger.lazyZip(shortAndLarger) forall ((x, y) => (x: Any) == (y: Any)))
  }
  property("## 2") = forAll { dv: Double =>
    val fv = dv.toFloat
    (fv != dv) || (fv.## == dv.##)
  }
}
