import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object Macros {
  def foo(name: String): Any = macro foo_impl
  def foo_impl(c: Context)(name: c.Expr[String]) = {
    import c.universe._

    val Literal(Constant(lit: String)) = name.tree
    val anon = newTypeName(c.fresh)

    c.Expr(Block(
      ClassDef(
        Modifiers(Flag.FINAL), anon, Nil, Template(
          Nil, noSelfType, List(
            DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
            TypeDef(Modifiers(), TypeName(lit), Nil, TypeTree(typeOf[Int]))
          )
        )
      ),
      Apply(Select(New(Ident(anon)), termNames.CONSTRUCTOR), Nil)
    ))
  }

  def bar(name: String): Any = macro bar_impl
  def bar_impl(c: Context)(name: c.Expr[String]) = {
    import c.universe._

    val Literal(Constant(lit: String)) = name.tree
    val anon = newTypeName(c.fresh)

    c.Expr(Block(
      ClassDef(
        Modifiers(Flag.FINAL), anon, Nil, Template(
          Nil, noSelfType, List(
            DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
            DefDef(
              Modifiers(), TermName(lit), Nil, Nil, TypeTree(),
              c.literal(42).tree
            )
          )
        )
      ),
      Apply(Select(New(Ident(anon)), termNames.CONSTRUCTOR), Nil)
    ))
  }

  def baz(name: String): Any = macro baz_impl
  def baz_impl(c: Context)(name: c.Expr[String]) = {
    import c.universe._

    val Literal(Constant(lit: String)) = name.tree
    val anon = newTypeName(c.fresh)
    val wrapper = newTypeName(c.fresh)

    c.Expr(Block(
      ClassDef(
        Modifiers(), anon, Nil, Template(
          Nil, emptyValDef, List(
            DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
            DefDef(
              Modifiers(), TermName(lit), Nil, Nil, TypeTree(),
              c.literal(42).tree
            )
          )
        )
      ),
      ClassDef(
        Modifiers(Flag.FINAL), wrapper, Nil,
        Template(Ident(anon) :: Nil, noSelfType, DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))) :: Nil)
      ),
      Apply(Select(New(Ident(wrapper)), termNames.CONSTRUCTOR), Nil)
    ))
  }
}