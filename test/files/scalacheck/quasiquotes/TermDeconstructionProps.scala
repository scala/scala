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

  property("deconstruct block") = test {
    val q"{ ..$xs }" = q"{ x1; x2; x3 }"
    assert(xs ≈ List(q"x1", q"x2", q"x3"))
  }

  property("exhaustive function matcher") = test {
    def matches(line: String) { val q"(..$args) => $body" = parse(line) }
    matches("() => bippy")
    matches("(y: Y) => y oh y")
    matches("(x: X, y: Y) => x and y")
  }

  property("exhaustive new pattern") = test {
    def matches(line: String) {
      val q"new { ..$early } with $name[..$targs](...$vargss) with ..$mixin { $self => ..$body }" = parse(line)
    }
    matches("new foo")
    matches("new foo { body }")
    matches("new foo[t]")
    matches("new foo(x)")
    matches("new foo[t](x)")
    matches("new foo[t](x) { body }")
    matches("new foo with bar")
    matches("new foo with bar { body }")
    matches("new { anonymous }")
    matches("new { val early = 1} with Parent[Int] { body }")
    matches("new Foo { selfie => }")
  }

  property("exhaustive assign pattern") = test {
    def matches(tree: Tree) { val q"$rhs = $lhs" = tree }
    matches(parse("left = right"))
    matches(parse("arr(1) = 2"))
    matches(AssignOrNamedArg(EmptyTree, EmptyTree))
  }

  property("deconstruct update 1") = test {
    val q"$obj(..$args) = $value" = q"foo(bar) = baz"
    assert(obj ≈ q"foo")
    assert(args ≈ List(q"bar"))
    assert(value ≈ q"baz")
  }

  property("deconstruct update 2") = test {
    val q"$left = $value" = q"foo(bar) = baz"
    assert(left ≈ q"foo(bar)")
    assert(value ≈ q"baz")
  }
}
