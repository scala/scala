import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import definitions._
import Flag._

object Test extends App {
  def pendingSuperCall() = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), Nil)
  // equivalent to q"class C"
  def cdef() = ClassDef(
    NoMods, TypeName("C"), Nil,
    Template(
      List(Select(Ident(TermName("scala")), TypeName("AnyRef"))),
      emptyValDef,
      List(
        DefDef(NoMods, nme.CONSTRUCTOR, Nil, List(Nil), TypeTree(), Block(List(pendingSuperCall()), Literal(Constant(())))),
        DefDef(Modifiers(OVERRIDE), TermName("toString"), Nil, Nil, TypeTree(), Literal(Constant("C")))
    )))
  def newc(csym: Symbol) = Apply(Select(New(Ident(csym)), nme.CONSTRUCTOR), List())

  val tb = cm.mkToolBox()
  val csym = tb.define(cdef())
  println(tb.eval(newc(csym)))
}