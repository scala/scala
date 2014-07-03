import scala.reflect.macros.Context
import scala.language.experimental.macros

trait Tree
case object SomeTree extends Tree

object NewQuasiquotes {
  implicit class QuasiquoteInterpolation(c: StringContext) {
    object nq {
      def unapply(t: Tree): Any = macro QuasiquoteMacros.unapplyImpl
    }
  }
}

object QuasiquoteMacros {
  def unapplyImpl(c: Context)(t: c.Expr[Tree]) = {
    import c.universe._
    import Flag._
    // q"""
    //   new {
    //     def unapply(t: Tree) = t match {
    //       case SomeTree => Some((SomeTree, SomeTree))
    //       case _ => None
    //     }
    //   }.unapply($t)
    // """
    c.Expr[Any](Apply(
      Select(
        Block(List(
          ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(),
            Template(List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef, List(
              DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
                Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
              DefDef(Modifiers(), newTermName("unapply"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("t"), Ident(newTypeName("Tree")), EmptyTree))), TypeTree(),
                Match(
                  Ident(newTermName("t")), List(
                    CaseDef(Ident(newTermName("SomeTree")), EmptyTree, Apply(Ident(newTermName("Some")), List(Apply(Select(Ident(newTermName("scala")), newTermName("Tuple2")), List(Ident(newTermName("SomeTree")), Ident(newTermName("SomeTree"))))))),
                    CaseDef(Ident(nme.WILDCARD), EmptyTree, Ident(newTermName("None")))))))))),
          Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())),
      newTermName("unapply")),
    List(t.tree)))
  }
}
