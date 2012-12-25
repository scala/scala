import scala.reflect.macros.{Context => Ctx}

class C
object Macros {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    import Flag._
    val defxint = DefDef(Modifiers(DEFERRED), TermName("x"), Nil, Nil, Ident(TypeName("Int")), EmptyTree)
    CompoundTypeTree(Template(Nil, emptyValDef, List(defxint)))
  }

  type Foo(x: Int) = macro foo
}
