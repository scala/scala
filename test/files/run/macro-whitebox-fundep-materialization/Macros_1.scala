import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

trait Iso[T, U] {
  def to(t : T) : U
  // def from(u : U) : T
}

object Iso {
  implicit def materializeIso[T, U]: Iso[T, U] = macro impl[T, U]
  def impl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context): c.Expr[Iso[T, U]] = {
    import c.universe._
    import definitions._
    import Flag._

    val sym = c.weakTypeOf[T].typeSymbol
    if (!sym.isClass || !sym.asClass.isCaseClass) c.abort(c.enclosingPosition, s"$sym is not a case class")
    val fields = sym.info.decls.toList.collect{ case x: TermSymbol if x.isVal && x.isCaseAccessor => x }

    def mkTpt() = {
      val core = Ident(TupleClass(fields.length) orElse UnitClass)
      if (fields.length == 0) core
      else AppliedTypeTree(core, fields map (f => TypeTree(f.info)))
    }

    def mkFrom() = {
      if (fields.length == 0) Literal(Constant(Unit))
      else Apply(Ident(newTermName("Tuple" + fields.length)), fields map (f => Select(Ident(newTermName("f")), newTermName(f.name.toString.trim))))
    }

    val evidenceClass = ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(
      List(AppliedTypeTree(Ident(newTypeName("Iso")), List(Ident(sym), mkTpt()))),
      emptyValDef,
      List(
        DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))),
        DefDef(Modifiers(), newTermName("to"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("f"), Ident(sym), EmptyTree))), TypeTree(), mkFrom()))))
    c.Expr[Iso[T, U]](Block(List(evidenceClass), Apply(Select(New(Ident(newTypeName("$anon"))), termNames.CONSTRUCTOR), List())))
  }
}
