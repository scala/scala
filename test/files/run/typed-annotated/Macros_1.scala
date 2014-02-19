import scala.reflect.macros.blackbox.Context
import language.experimental.macros

class ann extends scala.annotation.StaticAnnotation

object Macros {
  def impl(c: Context) = {
    import c.universe._
    // val tpt = Annotated(Apply(Select(New(Ident(newTypeName("ann"))), termNames.CONSTRUCTOR), List()), Ident(newTypeName("Int")))
    val tpt = Annotated(Apply(Select(New(Ident(newTypeName("ann"))), termNames.CONSTRUCTOR), List()), TypeTree(weakTypeOf[Int]))
    c.Expr[Unit](Block(
      List(ValDef(Modifiers(), newTermName("x"), tpt, Literal(Constant(42)))),
      Apply(Ident(newTermName("println")), List(Ident(newTermName("x"))))))
  }

  def foo = macro impl
}