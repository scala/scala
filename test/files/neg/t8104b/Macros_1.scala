import scala.reflect.macros.Context

object Macros {
  def impl[T](c: Context)(implicit T: c.WeakTypeTag[T]) = {
    import c.universe._
    import Flag._
    import definitions._
    val fields = T.tpe.declarations.toList.collect{ case x: TermSymbol if x.isVal && x.isCaseAccessor => x }
    val Repr = appliedType(TupleClass(fields.length).asType.toType, fields.map(_.typeSignature))
    c.Expr(Block(
      List(ClassDef(
        Modifiers(FINAL),
        newTypeName("$anon"),
        List(),
        Template(
          List(AppliedTypeTree(Ident(newTypeName("Generic")), List(TypeTree(T.tpe)))),
          emptyValDef,
          List(
            DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),
            TypeDef(Modifiers(), newTypeName("Repr"), List(), TypeTree(Repr)))))),
      Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())))
  }
}