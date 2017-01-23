import org.scalacheck._, Prop._, Gen._, Arbitrary._

object ErrorProps extends QuasiquoteProperties("errors") {
  property("can't extract two .. rankinalities in a row") = fails(
    "Can't extract with .. here",
    """
      val xs = List(q"x1", q"x2")
      val q"f(..$xs1, ..$xs2)" = xs
    """)

  property("can't unquote with given rank") = fails(
    "Can't unquote List[StringBuilder], consider using .. or providing an implicit instance of Liftable[List[StringBuilder]]",
    """
      import java.lang.StringBuilder
      val xs: List[StringBuilder] = Nil
      q"$xs"
    """)

  property("unquote typename into typedef with default bounds") = fails(
    "reflect.runtime.universe.Name expected but reflect.runtime.universe.TypeDef found",
    """
      val T1 = TypeName("T1")
      val T2 = q"type T"
      val t = EmptyTree
      q"type $T1[$T2 >: _root_.scala.Any <: _root_.scala.Nothing] = $t" ≈
        TypeDef(Modifiers(), T1, List(T2), t)
    """)

  property("can't unquote annotations with ... rank") = fails(
    "Can't unquote with ... here",
    """
      val annots = List(List(q"Foo"))
      q"@...$annots def foo"
    """)

  property("only literal string arguments") = fails(
    "Quasiquotes can only be used with literal strings",
    """
      val s: String = "foo"
      StringContext(s).q()
    """)

  property("don't know how to unquote inside of strings") = fails(
    "Don't know how to unquote here",
    """
      val x: Tree = EmptyTree
      StringContext("\"", "\"").q(x)
    """)

  property("non-liftable type ..") = fails(
    "Can't unquote List[StringBuilder] with .., consider omitting the dots or providing an implicit instance of Liftable[StringBuilder]",
    """
      import java.lang.StringBuilder
      val bazs = List(new StringBuilder)
      q"f(..$bazs)"
    """)

  property("non-liftable type ...") = fails(
    "Can't unquote List[List[StringBuilder]] with .., consider using ... or providing an implicit instance of Liftable[StringBuilder]",
    """
      import java.lang.StringBuilder
      val bazs = List(List(new StringBuilder))
      q"f(..$bazs)"
    """)

  property("use .. rank or provide liftable") = fails(
    "Can't unquote List[StringBuilder], consider using .. or providing an implicit instance of Liftable[List[StringBuilder]]",
    """
      import java.lang.StringBuilder
      val lst: List[StringBuilder] = Nil
      q"f($lst)"
    """)

  property("use ... rank or provide liftable") = fails(
    "Can't unquote List[List[StringBuilder]], consider using ... or providing an implicit instance of Liftable[List[List[StringBuilder]]]",
    """
      import java.lang.StringBuilder
      val xs: List[List[StringBuilder]] = Nil
      q"$xs"
    """)

  property("not liftable or natively supported") = fails(
    "Can't unquote StringBuilder, consider providing an implicit instance of Liftable[StringBuilder]",
    """
      import java.lang.StringBuilder
      val sb = new StringBuilder
      q"f($sb)"
    """)

  property("can't unquote with ... rank here") = fails(
    "Can't unquote with ... here",
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

  property("cant unquote flags together with mods") = fails(
    "Can't unquote flags together with modifiers, consider merging flags into modifiers",
    """
      val f = Flag.IMPLICIT; val m = NoMods
      q"$f $m def foo"
    """)

  property("can't unquote mods with annots") = fails(
    "Can't unquote modifiers together with annotations, consider merging annotations into modifiers",
    """
      val m = NoMods
      q"@annot $m def foo"
    """)

  property("can't unquote modifiers with inline flags") = fails(
    "Can't unquote modifiers together with flags, consider merging flags into modifiers",
    """
      val m = NoMods
      q"$m implicit def foo"
    """)

  property("can't unquote multiple mods") = fails(
    "Can't unquote multiple modifiers, consider merging them into a single modifiers instance",
    """
      val m1 = NoMods; val m2 = NoMods
      q"$m1 $m2 def foo"
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

  property("can't unquote values of Null") = fails(
    "Can't unquote Null, bottom type values often indicate programmer mistake",
    """
      val n = null
      q"$n"
    """)

  property("can't unquote values of Nothing") = fails(
    "Can't unquote Nothing, bottom type values often indicate programmer mistake",
    """
      def n = ???
      q"$n"
    """)

  property("SI-8211: check unbound placeholder parameters") = fails(
    "unbound placeholder parameter",
    """
      q"_"
    """)

  property("SI-8211: check unbound wildcard types") = fails(
    "unbound wildcard type",
    """
      tq"_"
    """)

  property("SI-8420: don't crash on splicing of non-unliftable native type (1)") = fails(
    "Can't unquote List[reflect.runtime.universe.Symbol] with .., consider omitting the dots or providing an implicit instance of Liftable[reflect.runtime.universe.Symbol]",
    """
      val l: List[Symbol] = Nil
      q"f(..$l)"
    """)

  property("SI-8420: don't crash on splicing of non-unliftable native type (2)") = fails(
    "Can't unquote List[reflect.runtime.universe.FlagSet] with .., consider omitting the dots or providing an implicit instance of Liftable[reflect.runtime.universe.FlagSet]",
    """
      val l: List[FlagSet] = Nil
      q"f(..$l)"
    """)

  property("SI-8420: don't crash on splicing of non-unliftable native type (3)") = fails(
    "Can't unquote List[reflect.runtime.universe.Modifiers] with .., consider omitting the dots or providing an implicit instance of Liftable[reflect.runtime.universe.Modifiers]",
    """
      val l: List[Modifiers] = Nil
      q"f(..$l)"
    """)

  property("SI-8451 construction: disallow everything except for constructor calls in secondary constructor bodies") = fails(
    "'this' expected but unquotee found",
    """
      val rhs1 = q"this(0)"
      val ctor1 = q"def this(x: Int) = $rhs1"
    """)

  property("SI-8451 deconstruction: disallow everything except for constructor calls in secondary constructor bodies") = fails(
    "'this' expected but unquotee found",
    """
      val q"def this(..$params) = $rhs2" = q"def this(x: Int) = this(0)"
    """)

  // // Make sure a nice error is reported in this case
  // { import Flag._; val mods = NoMods; q"lazy $mods val x: Int" }
}
