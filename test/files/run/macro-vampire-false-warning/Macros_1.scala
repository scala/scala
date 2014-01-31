// As per http://meta.plasm.us/posts/2013/08/31/feeding-our-vampires/

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

class body(tree: Any) extends StaticAnnotation

object Macros {
  def selFieldImpl(c: Context) = {
    import c.universe._
    val field = c.macroApplication.symbol
    val bodyAnn = field.annotations.filter(_.tree.tpe <:< typeOf[body]).head
    c.Expr[Any](bodyAnn.tree.children(1))
  }

  def mkObjectImpl(c: Context)(xs: c.Expr[Any]*) = {
    import c.universe._
    import Flag._
    // val kvps = xs.toList map { case q"${_}(${Literal(Constant(name: String))}).->[${_}]($value)" => name -> value }
    val kvps = xs.map(_.tree).toList map { case Apply(TypeApply(Select(Apply(_, List(Literal(Constant(name: String)))), _), _), List(value)) => name -> value }
    // val fields = kvps map { case (k, v) => q"@body($v) def ${TermName(k)} = macro Macros.selFieldImpl" }
    val fields = kvps map { case (k, v) => DefDef(
      Modifiers(MACRO, typeNames.EMPTY, List(Apply(Select(New(Ident(TypeName("body"))), termNames.CONSTRUCTOR), List(v)))),
      TermName(k), Nil, Nil, Ident(TypeName("Any")), Select(Ident(TermName("Macros")), TermName("selFieldImpl"))) }
    // q"import scala.language.experimental.macros; class Workaround { ..$fields }; new Workaround{}"
    c.Expr[Any](Block(
      List(
        Import(Select(Select(Ident(TermName("scala")), TermName("language")), TermName("experimental")), List(ImportSelector(TermName("macros"), 51, TermName("macros"), 51))),
        ClassDef(
          NoMods, TypeName("Workaround"), Nil,
          Template(
            List(Select(Ident(TermName("scala")), TypeName("AnyRef"))), noSelfType,
            DefDef(
              NoMods, termNames.CONSTRUCTOR, Nil, List(Nil), TypeTree(),
              Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(()))))
            +: fields)),
        ClassDef(
          Modifiers(FINAL), TypeName("$anon"), Nil,
          Template(
            List(Ident(TypeName("Workaround"))), noSelfType,
            List(
              DefDef(
                NoMods, termNames.CONSTRUCTOR, Nil, List(Nil), TypeTree(),
                Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))))))),
      Apply(Select(New(Ident(TypeName("$anon"))), termNames.CONSTRUCTOR), List())))
  }
}

object mkObject {
  def apply(xs: Any*): Any = macro Macros.mkObjectImpl
}
