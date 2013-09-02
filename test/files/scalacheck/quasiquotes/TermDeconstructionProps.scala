import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object TermDeconstructionProps extends QuasiquoteProperties("term deconstruction") {
  property("f(..x) = f") = test {
    assertThrows[MatchError] {
      val q"f(..$argss)" = q"f"
    }
  }

  property("f(x)") = forAll { (x: Tree) =>
    val q"f($x1)" = q"f($x)"
    x1 ≈ x
  }

  property("f(..xs)") = forAll { (x1: Tree, x2: Tree) =>
    val q"f(..$xs)" = q"f($x1, $x2)"
    xs ≈ List(x1, x2)
  }

  property("f(y, ..ys)") = forAll { (x1: Tree, x2: Tree, x3: Tree) =>
    val q"f($y, ..$ys)" = q"f($x1, $x2, $x3)"
    y ≈ x1 && ys ≈ List(x2, x3)
  }

  property("f(y1, y2, ..ys)") = forAll { (x1: Tree, x2: Tree, x3: Tree) =>
    val q"f($y1, $y2, ..$ys)" = q"f($x1, $x2, $x3)"
    y1 ≈ x1 && y2 ≈ x2 && ys ≈ List(x3)
  }

  property("f(...xss)") = forAll { (x1: Tree, x2: Tree) =>
    val q"f(...$argss)" = q"f($x1)($x2)"
    argss ≈ List(List(x1), List(x2))
  }

  property("f(...xss) = f") = forAll { (x1: Tree, x2: Tree) =>
    val q"f(...$argss)" = q"f"
    argss ≈ List()
  }

  property("deconstruct unit as tuple") = test {
    val q"(..$xs)" = q"()"
    assert(xs.isEmpty)
  }

  property("deconstruct tuple") = test {
    val q"(..$xs)" = q"(a, b)"
    assert(xs ≈ List(q"a", q"b"))
  }

  property("deconstruct tuple mixed") = test {
    val q"($first, ..$rest)" = q"(a, b, c)"
    assert(first ≈ q"a" && rest ≈ List(q"b", q"c"))
  }

  property("deconstruct cases") = test {
    val q"$x match { case ..$cases }" = q"x match { case 1 => case 2 => }"
    x ≈ q"x" && cases ≈ List(cq"1 =>", cq"2 =>")
  }
}