import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object TermDeconstructionProps extends QuasiquoteProperties("term deconstruction") {
  property("f(..x) = f") = test {
    // see SI-8008
    assertThrows[MatchError] {
      val q"f(..$args)" = q"f"
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

  property("f(y1, ..ys, yn)") = forAll { (x1: Tree, x2: Tree, x3: Tree, x4: Tree) =>
    val q"f($y1, ..$ys, $yn)" = q"f($x1, $x2, $x3, $x4)"
    y1 ≈ x1 && ys ≈ List(x2, x3) && yn ≈ x4
  }

  property("f(..ys, y_{n-1}, y_n)") = forAll { (x1: Tree, x2: Tree, x3: Tree, x4: Tree) =>
    val q"f(..$ys, $yn1, $yn)" = q"f($x1, $x2, $x3, $x4)"
    ys ≈ List(x1, x2) && yn1 ≈ x3 && yn ≈ x4
  }

  property("f(...xss)") = forAll { (x1: Tree, x2: Tree) =>
    val q"f(...$xss)" = q"f($x1)($x2)"
    xss ≈ List(List(x1), List(x2))
  }

  property("f(...$xss)(..$last)") = forAll { (x1: Tree, x2: Tree, x3: Tree) =>
    val q"f(...$xss)(..$last)" = q"f($x1)($x2)($x3)"
    xss ≈ List(List(x1), List(x2)) && last ≈ List(x3)
  }

  property("f(...$xss)(..$lastinit, $lastlast)") = forAll { (x1: Tree, x2: Tree, x3: Tree, x4: Tree) =>
    val q"f(...$xss)(..$lastinit, $lastlast)" = q"f($x1)($x2, $x3, $x4)"
    xss ≈ List(List(x1)) && lastinit ≈ List(x2, x3) && lastlast ≈ x4
  }

  property("f(...xss) = f") = forAll { (x1: Tree, x2: Tree) =>
    val q"f(...$xss)" = q"f"
    xss ≈ List()
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
    assert(first ≈ q"a")
    assert(rest ≈ List(q"b", q"c"))
  }

  property("deconstruct tuple last element") = test {
    val q"($first, ..$rest, $last)" = q"(a, b, c, d)"
    assert(first ≈ q"a")
    assert(rest ≈ List(q"b", q"c"))
    assert(last ≈ q"d")
  }

  property("deconstruct expr as tuple") = test {
    val q"(..$elems)" = q"foo"
    assert(elems ≈ List(q"foo"))
  }

  property("deconstruct cases") = test {
    val q"$x match { case ..$cases }" = q"x match { case 1 => case 2 => }"
    assert(x ≈ q"x")
    assert(cases ≈ List(cq"1 =>", cq"2 =>"))
  }

  property("deconstruct splitting last case") = test {
    val q"$_ match { case ..$cases case $last }" = q"x match { case 1 => case 2 => case 3 => }"
    assert(cases ≈ List(cq"1 =>", cq"2 =>"))
    assert(last ≈ cq"3 =>")
  }

  property("deconstruct block") = test {
    val q"{ ..$xs }" = q"{ x1; x2; x3 }"
    assert(xs ≈ List(q"x1", q"x2", q"x3"))
  }

  property("deconstruct last element of a block") = test {
    val q"{ ..$xs; $x }" = q"x1; x2; x3; x4"
    assert(xs ≈ List(q"x1", q"x2", q"x3"))
    assert(x ≈ q"x4")
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
    matches("new { val early = 1 } with Parent[Int] { body }")
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

  property("deconstruct while loop") = test {
    val q"while($cond) $body" = parse("while(cond) body")
    assert(cond ≈ q"cond")
    assert(body ≈ q"body")
  }

  property("deconstruct do while loop") = test {
    val q"do $body while($cond)" = parse("do body while(cond)")
    assert(cond ≈ q"cond")
    assert(body ≈ q"body")
  }

  property("deconstruct anonymous function with placeholders") = test {
    val q"{ $f(_) }" = q"{ foo(_) }"
    assert(f ≈ q"foo")
    val q"{ _.$member }" = q"{ _.foo }"
    assert(member ≈ TermName("foo"))
    val q"{ _ + $x }" = q"{ _ + x }"
    assert(x ≈ q"x")
    val q"{ _ * _ }" = q"{ _ * _ }"
  }

  property("si-8275 a") = test {
    val cq"_ => ..$stats" = cq"_ => foo; bar"
    assert(stats ≈ List(q"foo", q"bar"))
  }

  property("si-8275 b") = test {
    val cq"_ => ..$init; $last" = cq"_ => a; b; c"
    assert(init ≈ List(q"a", q"b"))
    assert(last ≈ q"c")
  }

  property("si-8275 c") = test {
    val cq"_ => ..$stats" = cq"_ =>"
    assert(stats.isEmpty)
    assertEqAst(q"{ case _ => ..$stats }", "{ case _ => }")
  }

  property("can't flatten type into block") = test {
    assertThrows[IllegalArgumentException] {
      val tpt = tq"List[Int]"
      q"..$tpt; ()"
    }
  }

  property("term select doesn't match type select") = test {
    assertThrows[MatchError] {
      val q"$qual.$name" = tq"foo.bar"
    }
  }

  property("type application doesn't match applied type") = test {
    assertThrows[MatchError] {
      val q"$f[..$targs]" = tq"foo[bar]"
    }
  }

  property("match doesn't match partial function") = test {
    assertThrows[MatchError] {
      val q"$_ match { case ..$_ }" = q"{ case _ => }"
    }
  }

  property("deconstruct partial function") = test {
    val q"{ case ..$cases }" = q"{ case a => b case c => d }"
    val List(cq"a => b", cq"c => d") = cases
  }

  property("SI-8350 `new C` and `new C()` are equivalent") = test {
    val q"new C" = q"new C()"
    val q"new C()" = q"new C"
  }

  property("SI-8350 new applications extracted only for non-empty ctor calls") = test{
    val q"new $c1" = q"new C()"
    assert(c1 ≈ tq"C")
    val q"new $c2" = q"new C(x)"
    assert(c2 ≈ q"${tq"C"}(x)")
  }

  property("SI-8350 original test case") = test {
    val q"new ..$parents" = q"new Foo with Bar"
    assert(parents ≈ List(tq"Foo", tq"Bar"))
  }

  property("SI-8387 new is not an application") = test {
    val `new` = q"new F(x)"
    val q"$f(...$argss)" = `new`
    assert(f ≈ `new`)
    assert(argss.isEmpty)
  }

  property("SI-8703 extract block with single expression") = test {
    val q"{ $a }" = Block(Nil, q"1")
    val Literal(Constant(1)) = a
    val q"{ $b }" = q"2"
    val Literal(Constant(2)) = b
  }
}
