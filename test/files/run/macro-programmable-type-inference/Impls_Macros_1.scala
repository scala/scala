import scala.reflect.macros.{Context, Macro}
import language.experimental.macros

trait Iso[C, L] {
  def from(f: C): L
  // def to(l: L): C
}

object Iso {
  implicit def materializeIso[C, L]: Iso[C, L] = macro MaterializeIso.expand[C, L]
}

trait MaterializeIso extends Macro {
  def expand[C: c.WeakTypeTag, L: c.WeakTypeTag]: c.Expr[Iso[C, L]] = {
    import c.universe._
    import definitions._
    import Flag._

    val sym = c.weakTypeOf[C].typeSymbol
    if (!sym.isClass || !sym.asClass.isCaseClass) c.abort(c.enclosingPosition, s"$sym is not a case class")
    val fields = sym.typeSignature.declarations.toList.collect{ case x: TermSymbol if x.isVal && x.isCaseAccessor => x }

    def mkTpt() = {
      val core = Ident(TupleClass(fields.length) orElse UnitClass)
      if (fields.length == 0) core
      else AppliedTypeTree(core, fields map (f => TypeTree(f.typeSignature)))
    }

    def mkFrom() = {
      if (fields.length == 0) Literal(Constant(Unit))
      else Apply(Ident(TermName("Tuple" + fields.length)), fields map (f => Select(Ident(TermName("f")), TermName(f.name.toString.trim))))
    }

    val evidenceClass = ClassDef(Modifiers(FINAL), TypeName("$anon"), List(), Template(
      List(AppliedTypeTree(Ident(TypeName("Iso")), List(Ident(sym), mkTpt()))),
      emptyValDef,
      List(
        DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))),
        DefDef(Modifiers(), TermName("from"), List(), List(List(ValDef(Modifiers(PARAM), TermName("f"), Ident(sym), EmptyTree))), TypeTree(), mkFrom()))))
    c.Expr[Iso[C, L]](Block(List(evidenceClass), Apply(Select(New(Ident(TypeName("$anon"))), nme.CONSTRUCTOR), List())))
  }

  override def onInfer(tic: c.TypeInferenceContext): Unit = {
    val C = tic.unknowns(0)
    val L = tic.unknowns(1)
    import c.universe._
    import definitions._
    val TypeRef(_, _, caseClassTpe :: _ :: Nil) = tic.expectedType // Iso[Test.Foo,?]
    tic.infer(C, caseClassTpe)
    val fields = caseClassTpe.typeSymbol.typeSignature.declarations.toList.collect{ case x: TermSymbol if x.isVal && x.isCaseAccessor => x }
    val core = (TupleClass(fields.length) orElse UnitClass).asType.toType
    val tequiv = if (fields.length == 0) core else appliedType(core, fields map (_.typeSignature))
    tic.infer(L, tequiv)
  }
}
