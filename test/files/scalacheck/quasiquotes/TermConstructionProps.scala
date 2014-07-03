import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object TermConstructionProps extends QuasiquoteProperties("term construction") {
  property("unquote single tree return tree itself") = forAll { (t: Tree) =>
    q"$t" ≈ t
  }

  property("unquote trees into if expression") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    q"if($t1) $t2 else $t3" ≈ If(t1, t2, t3)
  }

  property("unquote trees into ascriptiopn") = forAll { (t1: Tree, t2: Tree) =>
    q"$t1 : $t2" ≈ Typed(t1, t2)
  }

  property("unquote trees into apply") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    q"$t1($t2, $t3)" ≈ Apply(t1, List(t2, t3))
  }

  property("unquote trees with .. rank into apply") = forAll { (ts: List[Tree]) =>
    q"f(..$ts)" ≈ Apply(q"f", ts)
  }

  property("unquote iterable into apply") = forAll { (trees: List[Tree]) =>
    val itrees: Iterable[Tree] = trees
    q"f(..$itrees)" ≈ Apply(q"f", trees)
  }

  property("unquote trees with ... rank into apply") = forAll { (ts1: List[Tree], ts2: List[Tree]) =>
    val argss = List(ts1, ts2)
    q"f(...$argss)" ≈ Apply(Apply(q"f", ts1), ts2)
  }

  property("unquote term name into assign") = forAll { (name: TermName, t: Tree) =>
    q"$name = $t" ≈ Assign(Ident(name), t)
  }

  property("unquote trees into block") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    blockInvariant(q"""{
      $t1
      $t2
      $t3
    }""", List(t1, t2, t3))
  }


  property("unquote tree into new") = forAll { (tree: Tree) =>
    q"new $tree" ≈ Apply(Select(New(tree), termNames.CONSTRUCTOR), List())
  }

  property("unquote tree into return") = forAll { (tree: Tree) =>
    q"return $tree" ≈ Return(tree)
  }

  property("unquote a list of arguments") = forAll { (fun: Tree, args: List[Tree]) =>
    q"$fun(..$args)" ≈ Apply(fun, args)
  }

  property("unquote list and non-list fun arguments") = forAll { (fun: Tree, arg1: Tree, arg2: Tree, args: List[Tree]) =>
    q"$fun(..$args, $arg1, $arg2)" ≈ Apply(fun, args ++ List(arg1) ++ List(arg2)) &&
    q"$fun($arg1, ..$args, $arg2)" ≈ Apply(fun, List(arg1) ++ args ++ List(arg2)) &&
    q"$fun($arg1, $arg2, ..$args)" ≈ Apply(fun, List(arg1) ++ List(arg2) ++ args)
  }

  property("unquote into new") = forAll { (name: TypeName, body: List[Tree]) =>
    q"new $name { ..$body }" ≈
      q"""{
        final class $$anon extends $name {
          ..$body
        }
        new $$anon
      }"""
  }

  property("unquote type name into this") = forAll { (T: TypeName) =>
    q"$T.this" ≈ This(T)
  }

  property("unquote tree into throw") = forAll { (t: Tree) =>
    q"throw $t" ≈ Throw(t)
  }

  property("unquote trees into type apply") = forAll { (fun: TreeIsTerm, types: List[Tree]) =>
    q"$fun[..$types]" ≈ (if (types.nonEmpty) TypeApply(fun, types) else fun)
  }

  property("unquote trees into while loop") = forAll { (cond: Tree, body: Tree) =>
    val LabelDef(_, List(), If(cond1, Block(List(body1), Apply(_, List())), Literal(Constant(())))) = q"while($cond) $body"
    body1 ≈ body && cond1 ≈ cond
  }

  property("unquote trees into do while loop") = forAll { (cond: Tree, body: Tree) =>
    val LabelDef(_, List(), Block(List(body1), If(cond1, Apply(_, List()), Literal(Constant(()))))) = q"do $body while($cond)"
    body1 ≈ body && cond1 ≈ cond
  }

  def blockInvariant(quote: Tree, trees: List[Tree]) =
    quote ≈ (trees match {
      case Nil => q"{}"
      case _ :+ last if !last.isTerm => Block(trees, q"()")
      case head :: Nil => head
      case init :+ last => Block(init, last)
    })

  property("unquote list of trees into block (1)") = forAll { (trees: List[Tree]) =>
    blockInvariant(q"{ ..$trees }", trees)
  }

  property("unquote list of trees into block (2)") = forAll { (trees1: List[Tree], trees2: List[Tree]) =>
    blockInvariant(q"{ ..$trees1 ; ..$trees2 }", trees1 ++ trees2)
  }

  property("unquote list of trees into block (3)") = forAll { (trees: List[Tree], tree: Tree) =>
    blockInvariant(q"{ ..$trees; $tree }", trees :+ tree)
  }

  property("unquote term into brackets") = test {
    val a = q"a"
    assert(q"($a)" ≈ a)
  }

  property("unquote terms into tuple") = test {
    val a1 = q"a1"
    val a2 = q"a2"
    val as = List(a1, a2)
    assert(q"(..$as)" ≈ q"scala.Tuple2($a1, $a2)")
    assert(q"(a0, ..$as)" ≈ q"scala.Tuple3(a0, $a1, $a2)")
  }

  property("unquote empty list into tuple") = test {
    val empty = List[Tree]()
    assert(q"(..$empty)" ≈ q"()")
  }

  property("unquote single element list into tuple") = test {
    val xs = q"x" :: Nil
    assert(q"(..$xs)" ≈ xs.head)
  }

  property("function param flags are the same") = test {
    val xy = q"val x: A" :: q"val y: B" :: Nil
    assertEqAst(q"(..$xy) => x + y", "(x: A, y: B) => x + y")
  }

  property("anonymous functions don't support default values") = test {
    val x = q"val x: Int = 1"
    assertThrows[IllegalArgumentException] { q"($x) => x" }
  }

  property("assign variable") = test {
    val v = q"v"
    val value = q"foo"
    assertEqAst(q"$v = $value", "v = foo")
  }

  property("assign update 1") = test {
    val v = q"v"
    val args = q"1" :: q"2" :: Nil
    val value = q"foo"
    assertEqAst(q"$v(..$args) = $value", "v(1, 2) = foo")
  }

  property("assign update 2") = test {
    val a = q"v(0)"
    val value = q"foo"
    assertEqAst(q"$a = $value", "v(0) = foo")
  }

  property("assign or named arg") = test {
    val assignx = q"x = 1"
    assertEqAst(q"f($assignx)", "f(x = 1)")
  }

  property("fresh names are regenerated at each evaluation") = test {
    def plusOne = q"{ _ + 1 }"
    assert(!plusOne.equalsStructure(plusOne))
    def whileTrue = q"while(true) false"
    assert(!whileTrue.equalsStructure(whileTrue))
    def withEvidence = q"def foo[T: X]"
    assert(!withEvidence.equalsStructure(withEvidence))
  }

  property("make sure inference doesn't infer any") = test {
    val l1 = List(q"foo")
    val l2 = List(q"bar")
    val baz = q"baz"
    assert(q"f(..${l1 ++ l2})" ≈ q"f(foo, bar)")
    assert(q"f(..${l1 ++ l2}, $baz)" ≈ q"f(foo, bar, baz)")
    assert(q"f(${if (true) q"a" else q"b"})" ≈ q"f(a)")
  }

  property("unquote iterable of non-parametric type") = test {
    object O extends Iterable[Tree] { def iterator = List(q"foo").iterator }
    q"f(..$O)"
  }

  property("SI-8016") = test {
    val xs = q"1" :: q"2" :: Nil
    assertEqAst(q"..$xs", "{1; 2}")
    assertEqAst(q"{..$xs}", "{1; 2}")
  }

  property("SI-6842") = test {
    val cases: List[Tree] = cq"a => b" :: cq"_ => c" :: Nil
    assertEqAst(q"1 match { case ..$cases }", "1 match { case a => b case _ => c }")
    assertEqAst(q"try 1 catch { case ..$cases }", "try 1 catch { case a => b case _ => c }")
  }

  property("SI-8009") = test {
    q"`foo`".asInstanceOf[reflect.internal.SymbolTable#Ident].isBackquoted
  }

  property("SI-8148") = test {
    val q"($a, $b) => $_" = q"_ + _"
    assert(a.name != b.name)
  }

  property("SI-7275 a") = test {
    val t = q"stat1; stat2"
    assertEqAst(q"..$t", "{stat1; stat2}")
  }

  property("SI-7275 b") = test {
    def f(t: Tree) = q"..$t"
    assertEqAst(f(q"stat1; stat2"), "{stat1; stat2}")
  }

  property("SI-7275 c1") = test {
    object O
    implicit val liftO = Liftable[O.type] { _ => q"foo; bar" }
    assertEqAst(q"f(..$O)", "f(foo, bar)")
  }

  property("SI-7275 c2") = test {
    object O
    implicit val liftO = Liftable[O.type] { _ => q"{ foo; bar }; { baz; bax }" }
    assertEqAst(q"f(...$O)", "f(foo, bar)(baz, bax)")
  }

  property("SI-7275 d") = test {
    val l = q"a; b" :: q"c; d" :: Nil
    assertEqAst(q"f(...$l)", "f(a, b)(c, d)")
    val l2: Iterable[Tree] = l
    assertEqAst(q"f(...$l2)", "f(a, b)(c, d)")
  }

  property("SI-7275 e") = test {
    val t = q"{ a; b }; { c; d }"
    assertEqAst(q"f(...$t)", "f(a, b)(c, d)")
  }

  property("SI-7275 e2") = test {
    val t = q"{ a; b }; c; d"
    assertEqAst(q"f(...$t)", "f(a, b)(c)(d)")
  }

  property("remove synthetic unit") = test {
    val q"{ ..$stats1 }" = q"{ def x = 2 }"
    assert(stats1 ≈ List(q"def x = 2"))
    val q"{ ..$stats2 }" = q"{ class X }"
    assert(stats2 ≈ List(q"class X"))
    val q"{ ..$stats3 }" = q"{ type X = Int }"
    assert(stats3 ≈ List(q"type X = Int"))
    val q"{ ..$stats4 }" = q"{ val x = 2 }"
    assert(stats4 ≈ List(q"val x = 2"))
  }

  property("don't remove user-defined unit") = test {
    val q"{ ..$stats }" = q"{ def x = 2; () }"
    assert(stats ≈ List(q"def x = 2", q"()"))
  }

  property("empty-tree is not a block") = test {
    assertThrows[MatchError] {
      val q"{ ..$stats1 }" = q" "
    }
  }

  property("empty block is synthetic unit") = test {
    val q"()" = q"{}"
    val q"{..$stats}" = q"{}"
    assert(stats.isEmpty)
    assertEqAst(q"{..$stats}", "{}")
    assertEqAst(q"{..$stats}", "()")
  }

  property("consistent variable order") = test {
    val q"$a = $b = $c = $d = $e = $f = $g = $h = $k = $l" = q"a = b = c = d = e = f = g = h = k = l"
    assert(a ≈ q"a" && b ≈ q"b" && c ≈ q"c" && d ≈ q"d" && e ≈ q"e" && g ≈ q"g" && h ≈ q"h" && k ≈ q"k" && l ≈ q"l")
  }

  property("SI-8385 a") = test {
    assertEqAst(q"(foo.x = 1)(2)", "(foo.x = 1)(2)")
  }

  property("SI-8385 b") = test {
    assertEqAst(q"(() => ())()", "(() => ())()")
  }

  property("match scrutinee may not be empty") = test {
    assertThrows[IllegalArgumentException] {
      val scrutinee = q""
      val cases = List(cq"_ =>")
      q"$scrutinee match { case ..$cases }"
    }
  }

  property("construct partial function") = test {
    val cases = List(cq"a => b", cq"c => d")
    assertEqAst(q"{ case ..$cases }", "{ case a => b case c => d }")
  }

  property("SI-8609 a") = test {
    val q1 = q"val x = 1"
    val q2 = q"..$q1; val y = 2"
    assert(q2 ≈ q"{ val x = 1; val y = 2 }")
  }

  property("SI-8609 b") = test {
    val q1 = q"import foo.bar"
    val q2 = q"..$q1; val y = 2"
    assert(q2 ≈ q"{ import foo.bar; val y = 2 }")
  }
}
