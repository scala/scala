import scala.reflect.runtime.universe._
import Flag._
import definitions._
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox
import scala.tools.reflect.Eval

object Test extends App {
  val PARAMACCESSOR = (1L << 29).asInstanceOf[FlagSet]

  // these trees can be acquired by running the following incantation:
  //   echo 'class C(val x: Int); class D extends C(2)' > foo.scala
  //   ./scalac -Xprint:parser -Yshow-trees-stringified -Yshow-trees-compact foo.scala

  val c = ClassDef(
    Modifiers(), newTypeName("C"), List(),
    Template(
      List(Select(Ident(ScalaPackage), newTypeName("AnyRef"))),
      emptyValDef,
      List(
        ValDef(Modifiers(PARAMACCESSOR), newTermName("x"), Ident(newTypeName("Int")), EmptyTree),
        DefDef(
          Modifiers(),
          nme.CONSTRUCTOR,
          List(),
          List(List(ValDef(Modifiers(PARAM | PARAMACCESSOR), newTermName("x"), Ident(newTypeName("Int")), EmptyTree))),
          TypeTree(),
          Block(
            List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())),
            Literal(Constant(())))))))
  val d = ClassDef(
    Modifiers(), newTypeName("D"), List(),
    Template(
      List(Ident(newTypeName("C"))),
      emptyValDef,
      List(
        DefDef(
          Modifiers(),
          nme.CONSTRUCTOR,
          List(),
          List(List()),
          TypeTree(),
          Block(
            List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Literal(Constant(2))))),
            Literal(Constant(())))))))
  val result = Select(Apply(Select(New(Ident(newTypeName("D"))), nme.CONSTRUCTOR), List()), newTermName("x"))
  println(cm.mkToolBox().eval(Block(List(c, d), result)))
}