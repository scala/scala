import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._, internal.reificationSupport.SyntacticClassDef

object DefinitionDeconstructionProps
  extends QuasiquoteProperties("definition deconstruction")
  with TraitDeconstruction
  with ClassDeconstruction
  with ObjectDeconstruction
  with ModsDeconstruction
  with ValVarDeconstruction
  with DefDeconstruction
  with PackageDeconstruction
  with ImportDeconstruction

trait TraitDeconstruction { self: QuasiquoteProperties =>
  property("exhaustive trait matcher") = test {
    def matches(line: String) {
      val q"""$mods trait $name[..$targs]
              extends { ..$early } with ..$parents { $self => ..$body }""" = parse(line)
    }
    matches("trait Foo")
    matches("trait Foo[T]")
    matches("trait Foo { def bar }")
    matches("trait Foo extends Bar with Baz")
    matches("trait Foo { self: Bippy => val x: Int = 1}")
    matches("trait Foo extends { val early: Int = 1 } with Bar { val late = early }")
    matches("private[Gap] trait Foo")
  }
}

trait ObjectDeconstruction { self: QuasiquoteProperties =>
  property("exhaustive object matcher") = test {
    def matches(line: String) = {
      val q"""$mods object $name extends { ..$early } with ..$parents { $self => ..$body }""" = parse(line)
    }
    matches("object Foo")
    matches("object Foo extends Bar[T]")
    matches("object Foo extends { val early: T = v } with Bar")
    matches("object Foo extends Foo { selfy => body }")
    matches("private[Bippy] object Foo extends Bar with Baz")
  }
}

trait ClassDeconstruction { self: QuasiquoteProperties =>
  property("class without params") = test {
    val q"class $name { ..$body }" = q"class Foo { def bar = 3 }"
    assert(body ≈ List(q"def bar = 3"))
  }

  property("class constructor") = test {
    val q"class $name(...$argss)" = q"class Foo(x: Int)(y: Int)"
    assert(argss.length == 2)
  }

  property("class parents") = test {
    val q"class $name extends ..$parents" = q"class Foo extends Bar with Blah"
    assert(parents ≈ List(tq"Bar", tq"Blah"))
  }

  property("class selfdef") = test {
    val q"class $name { $self => }" = q"class Foo { self: T => }"
    assert(self.name ≈ TermName("self") && self.tpt ≈ tq"T")
  }

  property("class tparams") = test {
    val q"class $name[..$tparams]" = q"class Foo[A, B]"
    assert(tparams.map { _.name } == List(TypeName("A"), TypeName("B")))
  }

  property("deconstruct bare case class") = test {
    val q"$mods class $name(..$args) extends ..$parents" = q"case class Foo(x: Int)"
  }

  property("exhaustive class matcher") = test {
    def matches(line: String) {
      val tree = parse(line)
      val q"""$classMods0 class $name0[..$targs0] $ctorMods0(...$argss0)
              extends { ..$early0 } with ..$parents0 { $self0 => ..$body0 }""" = tree
      val q"""$classMods1 class $name1[..$targs1] $ctorMods1(...$argss1)(implicit ..$impl)
              extends { ..$early1 } with ..$parents1 { $self1 => ..$body1 }""" = tree
    }
    matches("class Foo")
    matches("class Foo[T]")
    matches("class Foo[T] @annot")
    matches("class Foo extends Bar with Baz")
    matches("class Foo { body }")
    matches("class Foo extends { val early = 0 } with Any")
    matches("abstract class Foo")
    matches("private[Baz] class Foo")
    matches("class Foo(first: A)(second: B)")
    matches("class Foo(first: A) extends Bar(first) with Baz")
    matches("class Foo private (first: A) { def bar }")
    matches("class Foo { self => bar(self) }")
    matches("case class Foo(x: Int)")
  }

  property("SI-7979") = test {
    val PARAMACCESSOR = (1 << 29).toLong.asInstanceOf[FlagSet]
    assertThrows[MatchError] {
      val SyntacticClassDef(_, _, _, _, _, _, _, _, _) =
        ClassDef(
          Modifiers(), TypeName("Foo"), List(),
          Template(
            List(Select(Ident(TermName("scala")), TypeName("AnyRef"))),
            noSelfType,
            List(
              //ValDef(Modifiers(PRIVATE | LOCAL | PARAMACCESSOR), TermName("x"), Ident(TypeName("Int")), EmptyTree),
              DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List(ValDef(Modifiers(PARAM | PARAMACCESSOR), TermName("x"),
                Ident(TypeName("Int")), EmptyTree))), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))))))
    }
  }

  property("SI-8332") = test {
    val q"class C(implicit ..$args)" = q"class C(implicit i: I, j: J)"
    val q"$imods val i: I" :: q"$jmods val j: J" :: Nil = args
    assert(imods.hasFlag(IMPLICIT))
    assert(jmods.hasFlag(IMPLICIT))
  }
}

trait ModsDeconstruction { self: QuasiquoteProperties =>
  property("deconstruct mods") = test {
    val mods = Modifiers(IMPLICIT | PRIVATE, TermName("foobar"), Nil)
    val q"$mods0 def foo" = q"$mods def foo"
    assert(mods0 ≈ mods)
  }

  property("@$annot def foo") = forAll { (annotName: TypeName) =>
    val q"@$annot def foo" = q"@$annotName def foo"
    annot ≈ Apply(Select(New(Ident(annotName)), termNames.CONSTRUCTOR), List())
  }

  property("@$annot(..$args) def foo") = forAll { (annotName: TypeName, tree: Tree) =>
    val q"@$annot(..$args) def foo" = q"@$annotName($tree) def foo"
    annot ≈ Ident(annotName) && args ≈ List(tree)
  }

  property("@..$annots def foo") = test {
    val a = q"new a"
    val b = q"new b"
    val q"@..$annots def foo" = q"@$a @$b def foo"
    annots ≈ List(a, b)
  }

  property("@$annot @..$annots def foo") = test {
    val a = q"new a"
    val b = q"new b"
    val c = q"new c"
    val q"@$first @..$rest def foo" = q"@$a @$b @$c def foo"
    assert(first ≈ a)
    assert(rest ≈ List(b, c))
  }

  property("@..$anots @$annot def foo") = test {
    val a = q"new a"
    val b = q"new b"
    val c = q"new c"
    val q"@..$init @$last def foo" = q"@$a @$b @$c def foo"
    assert(init ≈ List(a, b))
    assert(last ≈ c)
  }
}

trait ValVarDeconstruction { self: QuasiquoteProperties =>
  property("exhaustive val matcher") = test {
    def matches(line: String) { val q"$mods val $name: $tpt = $rhs" = parse(line) }
    matches("val x: Int")
    matches("val x: Int = 1")
    matches("lazy val x: Int = 1")
    matches("implicit val x = 1")
    assertThrows[MatchError] { matches("var x = 1") }
  }

  property("exhaustive var matcher") = test {
    def matches(line: String) { val q"$mods var $name: $tpt = $rhs" = parse(line) }
    matches("var x: Int")
    matches("var x: Int = 1")
    matches("var x = 1")
    assertThrows[MatchError] { matches("val x = 1") }
  }
}

trait PackageDeconstruction { self: QuasiquoteProperties =>
  property("exhaustive package matcher") = test {
    def matches(line: String) { val q"package $name { ..$body }" = parse(line) }
    matches("package foo { }")
    matches("package foo { class C }")
    matches("package foo.bar { }")
    matches("package bippy.bongo { object A; object B }")
    matches("package bippy { package bongo { object O } }")
  }

  property("exhaustive package object matcher") = test {
    def matches(line: String) {
      val q"package object $name extends { ..$early } with ..$parents { $self => ..$body }" = parse(line)
    }
    matches("package object foo")
    matches("package object foo { def baz }")
    matches("package object foo { self => }")
    matches("package object foo extends mammy with daddy { def baz }")
    matches("package object foo extends { val early = 1 } with daddy")
    assertThrows[MatchError] { matches("object foo") }
  }
}

trait DefDeconstruction { self: QuasiquoteProperties =>
  property("exhaustive def matcher") = test {
    def matches(line: String) = {
      val t = parse(line)
      val q"$mods0 def $name0[..$targs0](...$argss0): $restpe0 = $body0" = t
      val q"$mods1 def $name1[..$targs1](...$argss1)(implicit ..$impl1): $restpe1 = $body1" = t
    }
    matches("def foo = foo")
    matches("implicit def foo: Int = 2")
    matches("def foo[T](x: T): T = x")
    matches("def foo[A: B] = implicitly[B[A]]")
    matches("private def foo = 0")
    matches("def foo[A <% B] = null")
    matches("def foo(one: One)(two: Two) = (one, two)")
    matches("def foo[T](args: T*) = args.toList")
  }

  property("extract implicit arg list (1)") = test {
    val q"def foo(...$argss)(implicit ..$impl)" = q"def foo(x: Int)(implicit y: Int)"
    assert(impl ≈ List(q"${Modifiers(IMPLICIT | PARAM)} val y: Int"))
  }

  property("extract implicit arg list (2)") = test {
    val q"def foo(...$argss)(implicit ..$impl)" = q"def foo(x: Int)"
    assert(impl.isEmpty)
  }

  property("SI-8451") = test {
    val q"def this(..$params) = this(..$args)" = q"def this(x: Int) = this(0)"
    assert(params ≈ List(q"${Modifiers(PARAM)} val x: Int"))
    assert(args ≈ List(q"0"))
  }
}

trait ImportDeconstruction { self: QuasiquoteProperties =>
  property("exhaustive import matcher") = test {
    def matches(line: String) = {
      val q"import $ref.{..$sels}" = parse(line)
    }
    matches("import foo.bar")
    matches("import foo.{bar, baz}")
    matches("import foo.{a => b, c => d}")
    matches("import foo.{poision => _, _}")
    matches("import foo.bar.baz._")
  }

  property("extract import binding") = test {
    val q"import $_.$sel" = q"import foo.bar"
    val pq"bar" = sel
  }

  property("extract import wildcard") = test {
    val q"import $_.$sel" = q"import foo._"
    val pq"_" = sel
  }

  property("extract import rename") = test {
    val q"import $_.$sel" = q"import foo.{bar => baz}"
    val pq"bar -> baz" = sel
    val pq"$left -> $right" = sel
    val pq"bar" = left
    val pq"baz" = right
  }

  property("extract import unimport") = test {
    val q"import $_.$sel" = q"import foo.{bar => _}"
    val pq"bar -> _" = sel
    val pq"$left -> $right" = sel
    val pq"bar" = left
    val pq"_" = right
  }

  property("unquote names into import selector") = forAll {
    (expr: Tree, plain: TermName, oldname: TermName, newname: TermName, discard: TermName) =>

    val Import(expr1, List(
      ImportSelector(plain11, _, plain12, _),
      ImportSelector(oldname1, _, newname1, _),
      ImportSelector(discard1, _, wildcard, _))) =
        q"import $expr.{$plain, $oldname => $newname, $discard => _}"

    expr1 ≈ expr && plain11 == plain12 && plain12 == plain &&
    oldname1 == oldname && newname1 == newname && discard1 == discard && wildcard == termNames.WILDCARD
  }
}
