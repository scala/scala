import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._, internal.reificationSupport.ScalaDot

object DefinitionConstructionProps
    extends QuasiquoteProperties("definition construction")
    with ClassConstruction
    with TraitConstruction
    with TypeDefConstruction
    with ValDefConstruction
    with PatDefConstruction
    with DefConstruction
    with PackageConstruction
    with ImportConstruction {

  val x: Tree = q"val x: Int"
  property("SI-6842 a1") = test { assertEqAst(q"def f($x) = 0", "def f(x: Int) = 0") }
  property("SI-6842 a2") = test { assertEqAst(q"class C($x)", "class C(val x: Int)") }
  property("SI-6842 a3") = test { assertEqAst(q"class C { $x => }", "class C { x: Int => }") }
  property("SI-6842 a4") = test { assertEqAst(q"trait B { $x => }", "trait B { x: Int => }") }
  property("SI-6842 a5") = test { assertEqAst(q"object A { $x => }", "object A { x: Int => }") }

  val t: Tree = q"type T"
  property("SI-6842 b1") = test { assertEqAst(q"def f[$t] = 0", "def f[T] = 0") }
  property("SI-6842 b2") = test { assertEqAst(q"class C[$t]", "class C[T]") }
  property("SI-6842 b3") = test { assertEqAst(q"trait B[$t]", "trait B[T]") }
}

trait ClassConstruction { self: QuasiquoteProperties =>
  val anyRef = ScalaDot(TypeName("AnyRef"))
  val emtpyConstructor =
    DefDef(Modifiers(), termNames.CONSTRUCTOR, List(),
      List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
  def classWith(name: TypeName, parents: List[Tree] = List(anyRef), body: List[DefDef] = Nil) =
    ClassDef(
      Modifiers(), name, List(),
      Template(parents, emptyValDef, emtpyConstructor :: body))

  property("construct case class") = test {
    val params = q"val x: Int" :: q"val y: Int" :: Nil
    val name = TypeName("Point")
    assertEqAst(q"$CASE class $name(..$params)", "case class Point(x: Int, y: Int)")
  }

  property("case class bare param") = test {
    assertEqAst(q"$CASE class Point(x: Int, y: Int)", "case class Point(private[this] val x: Int, private[this] val y: Int)")
  }

  property("generate default constructors automatically") = test {
    val parents = List.empty[Tree]
    assertEqAst(q"class Foo extends ..$parents", "class Foo")
  }

  property("unquote term name into class") = forAll { (rname: TypeName) =>
    // add prefix to avoid failure in case rname is keyword
    val name = TypeName("prefix$" + rname)
    eqAst(q"class $name", "class " + name.toString)
  }

  property("unquote method into class") = forAll { (name: TypeName, method: DefDef) =>
    q"class $name { $method }" ≈ classWith(name, body = List(method))
  }

  property("unquote members into class") = forAll { (name: TypeName, defs: List[DefDef], extra: DefDef) =>
    q"""class $name {
      ..$defs
      $extra
    }""" ≈ classWith(name, body = defs :+ extra)
  }

  property("unquote type name into class parents") = forAll { (name: TypeName, parent: TypeName) =>
    q"class $name extends $parent" ≈ classWith(name, parents = List(Ident(parent)))
  }

  property("param flags are consistent with raw code") = test {
    val pubx = q"val x: Int"
    val privx = q"private[this] val x: Int"
    assertEqAst(q"     class C(x: Int)", "     class C(x: Int)                  ")
    assertEqAst(q"case class C(x: Int)", "case class C(x: Int)                  ")
    assertEqAst(q"     class C($pubx) ", "     class C(val x: Int)              ")
    assertEqAst(q"case class C($pubx) ", "case class C(x: Int)                  ")
    assertEqAst(q"     class C($privx)", "     class C(x: Int)                  ")
    assertEqAst(q"case class C($privx)", "case class C(private[this] val x: Int)")
  }

  property("SI-8333") = test {
    assertEqAst(q"{ $NoMods class C }", "{ class C }")
  }

  property("SI-8332") = test {
    val args = q"val a: Int; val b: Int"
    assertEqAst(q"class C(implicit ..$args)", "class C(implicit val a: Int, val b: Int)")
  }

  property("SI-8451: inline secondary constructors") = test {
    assertEqAst(q"class C(x: Int) { def this() = this(0) }", "class C(x: Int) { def this() = this(0) }")
  }

  property("SI-8451: unquoted secondary constructors") = test {
    val secondaryCtor = q"def this() = this(0)"
    assertEqAst(q"class C(x: Int) { $secondaryCtor }", "class C(x: Int) { def this() = this(0) }")
  }
}

trait TraitConstruction { self: QuasiquoteProperties =>
  property("unquote name into trait def") = test {
    val Foo = TypeName("Foo")
    assert(q"trait $Foo" ≈ q"trait Foo")
  }

  property("unquote type params into trait def") = test {
    val tparams = q"type A" :: q"type B" :: Nil
    assert(q"trait Foo[..$tparams]" ≈ q"trait Foo[A, B]")
  }

  property("unquote defs into trait body") = test {
    val body = q"def foo" :: q"val bar: Baz" :: Nil
    assert(q"trait Foo { ..$body }" ≈ q"trait Foo { def foo; val bar: Baz }")
  }

  property("unquote parents into trait") = test {
    val parents = tq"A" :: tq"B" :: Nil
    assert(q"trait Foo extends ..$parents" ≈ q"trait Foo extends A with B")
  }

  property("unquote early valdef into trait") = test {
    val x = q"val x: Int = 1"
    assertEqAst(q"trait T extends { $x } with Any", "trait T extends { val x: Int = 1} with Any")
  }

  property("construct trait with early valdef") = test {
    assertEqAst(q"trait T extends { val x: Int = 1 } with Any", "trait T extends { val x: Int = 1 } with Any")
  }

  property("unquote defs into early block") = test {
    val defs = q"val x: Int = 0" :: q"type Foo = Bar" :: Nil
    assert(q"trait T extends { ..$defs } with Bippy" ≈
           q"trait T extends { val x: Int = 0; type Foo = Bar} with Bippy")
  }

  property("fail on splicing of non-valid early tree") = test {
    val defn = q"def x: Int = 0"
    assertThrows[IllegalArgumentException] { q"trait T extends { $defn } with Bar" }
  }
}

trait TypeDefConstruction { self: QuasiquoteProperties =>
  property("unquote type name into typedef") = forAll { (name1: TypeName, name2: TypeName) =>
    q"type $name1 = $name2" ≈ TypeDef(Modifiers(), name1, List(), Ident(name2))
  }

  property("unquote type names into type bounds") = forAll { (T1: TypeName, T2: TypeName, T3: TypeName) =>
    q"type $T1 >: $T2 <: $T3" ≈
      TypeDef(
        Modifiers(DEFERRED), T1, List(),
        TypeBoundsTree(Ident(T2), Ident(T3)))
  }

  property("unquote trees names into type bounds") = forAll { (T: TypeName, t1: Tree, t2: Tree) =>
    q"type $T >: $t1 <: $t2" ≈
      TypeDef(
        Modifiers(DEFERRED), T, List(),
        TypeBoundsTree(t1, t2))
  }

  property("unquote tparams into typedef (1)") = forAll { (T: TypeName, targs: List[TypeDef], t: Tree) =>
    q"type $T[..$targs] = $t" ≈ TypeDef(Modifiers(), T, targs, t)
  }

  property("unquote tparams into typedef (2)") = forAll { (T: TypeName, targs1: List[TypeDef], targs2: List[TypeDef], t: Tree) =>
    q"type $T[..$targs1, ..$targs2] = $t" ≈ TypeDef(Modifiers(), T, targs1 ++ targs2, t)
  }

  property("unquote tparams into typedef (3)") = forAll { (T: TypeName, targ: TypeDef, targs: List[TypeDef], t: Tree) =>
    q"type $T[$targ, ..$targs] = $t" ≈ TypeDef(Modifiers(), T, targ :: targs, t)
  }

  property("unquote typename into typedef with default bounds") = forAll { (T1: TypeName, T2: TypeName, t: Tree) =>
    q"type $T1[$T2 >: Any <: Nothing] = $t" ≈
      TypeDef(
        Modifiers(), T1,
        List(TypeDef(
          Modifiers(PARAM), T2,
          List(),
          TypeBoundsTree(
            Ident(TypeName("Any")),
            Ident(TypeName("Nothing"))))),
        t)
  }

  property("unquote type names into compound type tree") = forAll { (T: TypeName, A: TypeName, B: TypeName) =>
    q"type $T = $A with $B" ≈
      TypeDef(
        Modifiers(), T, List(),
        CompoundTypeTree(
          Template(List(Ident(A), Ident(B)), ValDef(Modifiers(PRIVATE), termNames.WILDCARD, TypeTree(), EmptyTree), List())))
  }

  property("unquote trees into existential type tree") = forAll {
    (T1: TypeName, T2: TypeName, X: TypeName, Lo: TypeName, Hi: TypeName) =>

    q"type $T1 = $T2[$X] forSome { type $X >: $Lo <: $Hi }" ≈
      TypeDef(
        Modifiers(), T1, List(),
        ExistentialTypeTree(
          AppliedTypeTree(Ident(T2), List(Ident(X))),
          List(
            TypeDef(Modifiers(DEFERRED), X, List(), TypeBoundsTree(Ident(Lo), Ident(Hi))))))
  }

  property("unquote tree into singleton type tree") = forAll { (name: TypeName, t: Tree) =>
    q"type $name = $t.type" ≈ q"type $name = ${SingletonTypeTree(t)}"
  }

  property("unquote into applied type tree") = forAll { (T1: TypeName, T2: TypeName, args: List[Tree]) =>
    q"type $T1 = $T2[..$args]" ≈
      TypeDef(Modifiers(), T1, List(),
        if(args.nonEmpty) AppliedTypeTree(Ident(T2), args) else Ident(T2))
  }
}

trait ValDefConstruction { self: QuasiquoteProperties =>
  property("unquote into val") = forAll { (name: TermName, tpt: Tree, rhs: Tree) =>
    q"val $name: $tpt = $rhs" ≈ ValDef(Modifiers(), name, tpt, rhs)
  }

  property("unquote into var") = forAll { (name: TermName, tpt: Tree, rhs: Tree) =>
    q"var $name: $tpt = $rhs" ≈ ValDef(Modifiers(MUTABLE), name, tpt, rhs)
  }

  // left tree is not a pattern due to Si-8211
  property("SI-8202") = test {
    assertEqAst(q"val (x: Int) = 1", "val x: Int = 1")
  }
}

trait PatDefConstruction { self: QuasiquoteProperties =>
  property("unquote pattern into pat def") = test {
    val pat = pq"(a, b)"
    assertEqAst(q"val $pat = (1, 2)", "val (a, b) = (1, 2)")
    val tpt = tq"(Int, Int)"
    assertEqAst(q"val $pat: $tpt = (1, 2)", "val (a, b): (Int, Int) = (1, 2)")
  }

  property("unquote pattern into pat def within other pattern (1)") = test {
    val pat = pq"(a, b)"
    assertEqAst(q"val Foo($pat) = Foo((1, 2))", "val Foo((a, b)) = Foo((1, 2))")
    val tpt = tq"Foo"
    assertEqAst(q"val Foo($pat): $tpt = Foo((1, 2))", "val Foo((a, b)): Foo = Foo((1, 2))")
  }

  property("unquote patterns into pat def within other pattern (2)") = test {
    val pat1 = pq"(a, b)"; val pat2 = pq"(c, d)"
    assertEqAst(q"val ($pat1, $pat2) = ((1, 2), (3, 4))", "val ((a, b), (c, d)) = ((1, 2), (3, 4))")
    val tpt = tq"((Int, Int), (Int, Int))"
    assertEqAst(q"val ($pat1, $pat2): $tpt = ((1, 2), (3, 4))", "val ((a, b), (c, d)): ((Int, Int), (Int, Int)) = ((1, 2), (3, 4))")
  }

  property("unquote pattern without free vars into pat def") = test {
    val pat = pq"((1, 2), 3)"
    assertEqAst(q"val $pat = ((1, 2), 3)", "{ val ((1, 2), 3) = ((1, 2), 3) }")
    val tpt = tq"((Int, Int), Int)"
    assertEqAst(q"val $pat: $tpt = ((1, 2), 3)","{ val ((1, 2), 3): ((Int, Int), Int) = ((1, 2), 3) }")
  }

  // won't result into pattern match due to SI-8211
  property("unquote typed pat into pat def") = test {
    val pat = pq"x: Int"
    assertEqAst(q"val $pat = 2", "{ val x: Int = 2 }")
  }
}

trait MethodConstruction { self: QuasiquoteProperties =>
  property("unquote paramss into defdef") = test {
    val paramss = List(q"val x: Int") :: List(q"val y: Int = 1") :: Nil
    assert(q"def foo(...$paramss)" ≈ parse("def foo(x: Int)(y: Int = 1)"))
  }

  property("unquote tparams into defdef") = test {
    val tparams = q"type A" :: q"type B <: Bippy" :: Nil
    assert(q"def foo[..$tparams]" ≈ parse("def foo[A, B <: Bippy]"))
  }

  def assertSameAnnots(tree: {def mods: Modifiers}, annots: List[Tree]) =
    assert(tree.mods.annotations ≈ annots,
           s"${tree.mods.annotations} =/= ${annots}")

  def assertSameAnnots(tree1: {def mods: Modifiers}, tree2: {def mods: Modifiers}) =
    assert(tree1.mods.annotations ≈ tree2.mods.annotations,
           s"${tree1.mods.annotations} =/= ${tree2.mods.annotations}")

  property("unquote type name into annotation") = test {
    val name = TypeName("annot")
    assertSameAnnots(q"@$name def foo", List(q"new $name"))
  }

  property("unquote ident into annotation") = test {
    val name = TypeName("annot")
    val ident = Ident(name)
    assertSameAnnots(q"@$ident def foo", List(q"new $name"))
  }

  property("unquote idents into annotation") = test {
    val idents = List(Ident(TypeName("annot1")), Ident(TypeName("annot2")))
    assertSameAnnots(q"@..$idents def foo",
      idents.map { ident => Apply(Select(New(ident), termNames.CONSTRUCTOR), List()) })
  }

  property("unquote constructor calls into annotation") = test {
    val ctorcalls = List(q"new a1", q"new a2")
    assertSameAnnots(q"@..$ctorcalls def foo", ctorcalls)
  }

  property("unquote multiple annotations (1)") = test {
    val annot1 = q"new a1"
    val annot2 = q"new a2"
    val res = q"@$annot1 @$annot2 def foo"
    assertSameAnnots(res, List(annot1, annot2))
  }

  property("unquote multiple annotations (2)") = test {
    val annot1 = q"new a1"
    val annots = List(q"new a2", q"new a3")
    val res = q"@$annot1 @..$annots def foo"
    assertSameAnnots(res, annot1 :: annots)
  }

  property("unquote annotations with arguments (1)") = test {
    val a = q"new a(x)"
    assertSameAnnots(q"@$a def foo", q"@a(x) def foo")
  }

  property("unquote annotations with arguments (2)") = test {
    val a = TypeName("a")
    assertSameAnnots(q"@$a(x) def foo", q"@a(x) def foo")
  }

  property("unquote annotations with arguments (3") = test {
    val a = Ident(TypeName("a"))
    assertSameAnnots(q"@$a(x) def foo", q"@a(x) def foo")
  }

  property("unquote improper tree into annot") = test {
    val t = tq"Foo[Baz]"
    assertThrows[IllegalArgumentException] {
      q"@$t def foo"
    }
  }

  property("can't unquote annotations with arguments specified twice") = test {
    val a = q"new a(x)"
    assertThrows[IllegalArgumentException] {
      q"@$a(y) def foo"
    }
  }

  property("unquote annotation with targs") = test {
    val a = q"new Foo[A, B]"
    assertEqAst(q"@$a def foo", "@Foo[A,B] def foo")
  }

  property("unquote annotation with multiple argument lists") = test {
    val a = q"new Foo(a)(b)"
    assertEqAst(q"@$a def foo", "@Foo(a)(b) def foo")
  }
}

trait PackageConstruction { self: QuasiquoteProperties =>
  property("unquote select into package name") = test {
    val name = q"foo.bar"
    assertEqAst(q"package $name { }", "package foo.bar { }")
  }

  property("splice name into package name") = test{
    val name = TermName("bippy")
    assertEqAst(q"package $name { }", "package bippy { }")
  }

  property("unquote members into package body") = test {
    val members = q"class C" :: q"object O" :: Nil
    assertEqAst(q"package foo { ..$members }", "package foo { class C; object O }")
  }

  property("unquote illegal members into package body") = test {
    val f = q"def f"
    assertThrows[IllegalArgumentException] { q"package foo { $f }" }
    val v = q"val v = 0"
    assertThrows[IllegalArgumentException] { q"package foo { $v }" }
    val expr = q"x + 1"
    assertThrows[IllegalArgumentException] { q"package foo { $expr }" }
  }

  property("unquote name into package object") = test {
    val foo = TermName("foo")
    assertEqAst(q"package object $foo", "package object foo")
  }

  property("unquote parents into package object") = test {
    val parents = tq"a" :: tq"b" :: Nil
    assertEqAst(q"package object foo extends ..$parents",
                 "package object foo extends a with b")
  }

  property("unquote members into package object") = test {
    val members = q"def foo" :: q"val x = 1" :: Nil
    assertEqAst(q"package object foo { ..$members }",
                 "package object foo { def foo; val x = 1 }")
  }

  property("unquote early def into package object") = test {
    val edefs = q"val x = 1" :: q"type I = Int" :: Nil
    assertEqAst(q"package object foo extends { ..$edefs } with Any",
                 "package object foo extends { val x = 1; type I = Int } with Any")
  }
}

trait DefConstruction { self: QuasiquoteProperties =>
  property("construct implicit args (1)") = test {
    val x = q"val x: Int"
    assertEqAst(q"def foo(implicit $x) = x", "def foo(implicit x: Int) = x")
  }

  property("construct implicit args (2)") = test {
    val xs = q"val x1: Int" :: q"val x2: Long" :: Nil
    assertEqAst(q"def foo(implicit ..$xs) = x1 + x2", "def foo(implicit x1: Int, x2: Long) = x1 + x2")
  }
}

trait ImportConstruction { self: QuasiquoteProperties =>
  property("construct wildcard import") = test {
    val sel = pq"_"
    assert(q"import foo.$sel" ≈ q"import foo._")
  }

  property("construct named import") = test {
    val sel = pq"bar"
    assert(q"import foo.$sel" ≈ q"import foo.bar")
  }

  property("construct renaming import") = test {
    val sel = pq"bar -> baz"
    assert(q"import foo.$sel" ≈ q"import foo.{bar => baz}")
  }

  property("construct unimport import") = test {
    val sels = pq"poison -> _" :: pq"_" :: Nil
    assert(q"import foo.{..$sels}" ≈ q"import foo.{poison => _, _}")
  }

  property("construct mixed import") = test {
    val sels = pq"a -> b" :: pq"c -> _" :: pq"_" :: Nil
    assert(q"import foo.{..$sels}" ≈ q"import foo.{a => b, c => _, _}")
  }
}
