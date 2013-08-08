import org.scalacheck._
import Prop._
import Gen._
import Arbitrary._

import scala.reflect.runtime.universe._
import Flag._

object ErrorProps extends QuasiquoteProperties("errors") {
  property("can't extract two .. cardinalities in a row") = fails(
    "Can't extract with .. here",
    """
      val xs = List(q"x1", q"x2")
      val q"f(..$xs1, ..$xs2)" = xs
    """)

  property("can't splice with given cardinality") = fails(
    "Can't splice List[reflect.runtime.universe.Ident], consider using ..",
    """
      val xs = List(q"x", q"x")
      q"$xs"
    """)

  property("splice typename into typedef with default bounds") = fails(
    "reflect.runtime.universe.Name expected but reflect.runtime.universe.TypeDef found",
    """
      val T1 = TypeName("T1")
      val T2 = q"type T"
      val t = EmptyTree
      q"type $T1[$T2 >: _root_.scala.Any <: _root_.scala.Nothing] = $t" ≈
        TypeDef(Modifiers(), T1, List(T2), t)
    """)

  property("can't splice annotations with ... cardinality") = fails(
    "Can't splice with ... here",
    """
      val annots = List(List(q"Foo"))
      q"@...$annots def foo"
    """)

  property("@..$first @$rest def foo") = fails(
    "Can't extract with .. here",
    """
      val a = annot("a")
      val b = annot("b")
      val c = annot("c")
      val q"@..$first @$rest def foo" = q"@$a @$b @$c def foo"
    """)

  property("only literal string arguments") = fails(
    "Quasiquotes can only be used with literal strings",
    """
      val s: String = "foo"
      StringContext(s).q()
    """)

  property("don't know how to splice inside of strings") = fails(
    "Don't know how to splice here",
    """
      val x: Tree = EmptyTree
      StringContext("\"", "\"").q(x)
    """)

  property("expected different cardinality") = fails(
    "Can't splice List[reflect.runtime.universe.Tree] with ..., consider using ..",
    """
      val args: List[Tree] = Nil
      q"f(...$args)"
    """)

  property("non-liftable type ..") = fails(
    "Can't splice List[StringBuilder] with .., consider omitting the dots or providing an implicit instance of Liftable[StringBuilder]",
    """
      import java.lang.StringBuilder
      val bazs = List(new StringBuilder)
      q"f(..$bazs)"
    """)

  property("non-liftable type ...") = fails(
    "Can't splice List[List[StringBuilder]] with .., consider using ... or providing an implicit instance of Liftable[StringBuilder]",
    """
      import java.lang.StringBuilder
      val bazs = List(List(new StringBuilder))
      q"f(..$bazs)"
    """)

  property("use .. card or provide liftable") = fails(
    "Can't splice List[StringBuilder], consider using .. or providing an implicit instance of Liftable[List[StringBuilder]]",
    """
      import java.lang.StringBuilder
      val lst: List[StringBuilder] = Nil
      q"f($lst)"
    """)

  property("use ... card or provide liftable") = fails(
    "Can't splice List[List[reflect.runtime.universe.Ident]], consider using ...",
    """
      val xs = List(List(q"x", q"x"))
      q"$xs"
    """)

  property("use zero card") = fails(
    "Can't splice reflect.runtime.universe.Tree with .., consider omitting the dots",
    """
      val t = EmptyTree
      q"f(..$t)"
    """)

  property("not liftable or natively supported") = fails(
    "Can't splice StringBuilder, consider providing an implicit instance of Liftable[StringBuilder]",
    """
      import java.lang.StringBuilder
      val sb = new StringBuilder
      q"f($sb)"
    """)

  property("casedef expected") = fails(
    "reflect.runtime.universe.CaseDef expected but reflect.runtime.universe.Tree found",
    """
      val t = EmptyTree
      q"_ { case $t }"
    """)

  property("can't splice with ... card here") = fails(
    "Can't splice with ... here",
    """
      val lst: List[List[Tree]] = Nil; val t = EmptyTree
      q"f(...$lst, $t)"
    """)

  property("name expected") = fails(
    "reflect.runtime.universe.Name expected but reflect.runtime.universe.Tree found",
    """
      val t = EmptyTree
      q"class $t"
    """)

  property("flags or mods expected") = fails(
    "reflect.runtime.universe.FlagSet or reflect.runtime.universe.Modifiers expected but reflect.runtime.universe.Tree found",
    """
      val t = EmptyTree
      q"$t def foo"
    """)

  property("cant splice flags together with mods") = fails(
    "Can't splice flags together with modifiers, consider merging flags into modifiers",
    """
      val f = Flag.IMPLICIT; val m = NoMods
      q"$f $m def foo"
    """)

  property("can't splice mods with annots") = fails(
    "Can't splice modifiers together with annotations, consider merging annotations into modifiers",
    """
      val m = NoMods
      q"@annot $m def foo"
    """)

  property("can't splice modifiers with inline flags") = fails(
    "Can't splice modifiers together with flags, consider merging flags into modifiers",
    """
      val m = NoMods
      q"$m implicit def foo"
    """)

  property("can't splice multiple mods") = fails(
    "Can't splice multiple modifiers, consider merging them into a single modifiers instance",
    """
      val m1 = NoMods; val m2 = NoMods
      q"$m1 $m2 def foo"
    """)

  property("can't extract with .. card here") = fails(
    "Can't extract with .. here",
    """
      val q"f(..$xs, $y)" = EmptyTree
    """)

  property("can't extract mods with annots") = fails(
    "Can't extract modifiers together with annotations, consider extracting just modifiers",
    """
      val q"@$annot $mods def foo" = EmptyTree
    """)

  property("can't extract multiple mods") = fails(
    "Can't extract multiple modifiers together, consider extracting a single modifiers instance",
    """
      val q"$m1 $m2 def foo" = EmptyTree
    """)

  // // Make sure a nice error is reported in this case
  // { import Flag._; val mods = NoMods; q"lazy $mods val x: Int" }
}