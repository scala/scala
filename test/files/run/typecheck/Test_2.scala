import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  Macros.foo

  val tb = cm.mkToolBox()
  val classDef = ClassDef(
    Modifiers(), newTypeName("C"), List(),
    Template(
      List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef,
      List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))))))
  tb.typeCheck(classDef)
}