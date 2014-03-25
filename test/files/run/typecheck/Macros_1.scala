import scala.reflect.macros.Context
import scala.language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val classDef = ClassDef(
      Modifiers(), newTypeName("C"), List(),
      Template(
        List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef,
        List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))))))
    c.typeCheck(classDef)
    c.Expr[Any](Literal(Constant(())))
  }

  def foo: Any = macro impl
}