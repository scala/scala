import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object DefinitionDeconstructionProps
  extends QuasiquoteProperties("definition deconstruction")
  with TraitDeconstruction
  with ClassDeconstruction
  with ObjectDeconstruction
  with ModsDeconstruction
  with ValVarDeconstruction

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
      val q"""$classMods class $name[..$targs] $ctorMods(...$argss)
              extends { ..$early } with ..$parents { $self => ..$body }""" = parse(line)
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
}

trait ModsDeconstruction { self: QuasiquoteProperties =>
  property("deconstruct mods") = test {
    val mods = Modifiers(IMPLICIT | PRIVATE, TermName("foobar"), Nil)
    val q"$mods0 def foo" = q"$mods def foo"
    assert(mods0 ≈ mods)
  }

  property("@$annot def foo") = forAll { (annotName: TypeName) =>
    val q"@$annot def foo" = q"@$annotName def foo"
    annot ≈ Apply(Select(New(Ident(annotName)), nme.CONSTRUCTOR), List())
  }

  property("@$annot(..$args) def foo") = forAll { (annotName: TypeName, tree: Tree) =>
    val q"@$annot(..$args) def foo" = q"@$annotName($tree) def foo"
    annot ≈ Ident(annotName) && args ≈ List(tree)
  }

  property("@..$annots def foo") = test {
    val a = annot("a")
    val b = annot("b")
    val q"@..$annots def foo" = q"@$a @$b def foo"
    annots ≈ List(a, b)
  }

  property("@$annot @..$annots def foo") = test {
    val a = annot("a")
    val b = annot("b")
    val c = annot("c")
    val q"@$first @..$rest def foo" = q"@$a @$b @$c def foo"
    first ≈ a && rest ≈ List(b, c)
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