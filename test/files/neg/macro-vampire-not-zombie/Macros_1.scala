// As per http://meta.plasm.us/posts/2013/08/31/feeding-our-vampires/

import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context
import scala.language.experimental.macros

class body(tree: Any) extends StaticAnnotation

object Macros {
  def selFieldImpl(c: Context) = {
    import c.universe._
    val field = c.macroApplication.symbol
    val bodyAnn = field.annotations.filter(_.tpe <:< typeOf[body]).head
    c.Expr[Any](bodyAnn.scalaArgs.head)
  }

  def mkObjectImpl(c: Context)(xs: c.Expr[Any]*) = {
    import c.universe._
    import Flag._
    // val kvps = xs.toList map { case q"${_}(${Literal(Constant(name: String))}).->[${_}]($value)" => name -> value }
    val kvps = xs.map(_.tree).toList map { case Apply(TypeApply(Select(Apply(_, List(Literal(Constant(name: String)))), _), _), List(value)) => name -> value }
    // val fields = kvps map { case (k, v) => q"@body($v) def ${TermName(k)} = macro Macros.selFieldImpl" }
    val fields = kvps map { case (k, v) => DefDef(
      Modifiers(MACRO, tpnme.EMPTY, List(Apply(Select(New(Ident(newTypeName("body"))), nme.CONSTRUCTOR), List(v)))),
      newTermName(k), Nil, Nil, TypeTree(), Select(Ident(newTermName("Macros")), newTermName("selFieldImpl"))) }
    // q"import scala.language.experimental.macros; class Workaround { ..$fields }; new Workaround{}"
    c.Expr[Any](Block(
      List(
        Import(Select(Select(Ident(newTermName("scala")), newTermName("language")), newTermName("experimental")), List(ImportSelector(newTermName("macros"), 51, newTermName("macros"), 51))),
        ClassDef(
          NoMods, newTypeName("Workaround"), Nil,
          Template(
            List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef,
            DefDef(
              NoMods, nme.CONSTRUCTOR, Nil, List(Nil), TypeTree(),
              Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))
            +: fields)),
        ClassDef(
          Modifiers(FINAL), newTypeName("$anon"), Nil,
          Template(
            List(Ident(newTypeName("Workaround"))), emptyValDef,
            List(
              DefDef(
                NoMods, nme.CONSTRUCTOR, Nil, List(Nil), TypeTree(),
                Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))))))),
      Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())))
  }
}

object mkObject {
  def apply(xs: Any*) = macro Macros.mkObjectImpl
}
