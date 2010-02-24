import org.scalacheck._
import Prop._
import Gen._

object Test extends Properties("==") {
  property("reflexive") = forAll { (x: AnyVal, y: AnyVal) => (x == y) == (y == x) }
  // property("hashCode") = forAll { (x

  property("short/char") = forAll { (x: Short) => {
      val ch: Char = x.toChar
      (x == ch) == (ch == x) ||
      // that's the whole test once it works, but since it doesn't yet:
      x < 0
    }
  }
}

