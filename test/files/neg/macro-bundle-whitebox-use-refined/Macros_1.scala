import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

// whitebox use case #1: return type refinement

class ReturnTypeRefinementBundle(val c: Context { type PrefixType = Nothing }) {
  import c.universe._
  def impl = {
    q"""
      trait Foo {
        def x = 2
      }
      new Foo {}
    """
  }
}

object ReturnTypeRefinement {
  def foo: Any = macro ReturnTypeRefinementBundle.impl
}

// whitebox use case #2: fundep materialization

trait FundepMaterialization[T, U] {
  def to(t : T) : U
  // def from(u : U) : T
}

class FundepMaterializationBundle(val c: Context { type PrefixType = Nothing }) {
  import c.universe._
  import definitions._
  import Flag._

  def impl[T: c.WeakTypeTag, U: c.WeakTypeTag]: c.Expr[FundepMaterialization[T, U]] = {
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
      List(AppliedTypeTree(Ident(newTypeName("FundepMaterialization")), List(Ident(sym), mkTpt()))),
      emptyValDef,
      List(
        DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))),
        DefDef(Modifiers(), newTermName("to"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("f"), Ident(sym), EmptyTree))), TypeTree(), mkFrom()))))
    c.Expr[FundepMaterialization[T, U]](Block(List(evidenceClass), Apply(Select(New(Ident(newTypeName("$anon"))), termNames.CONSTRUCTOR), List())))
  }
}

object FundepMaterialization {
  implicit def materializeIso[T, U]: FundepMaterialization[T, U] = macro FundepMaterializationBundle.impl[T, U]
}

// whitebox use case #3: dynamic materialization

trait DynamicMaterialization[T]

class C1(val x: Int)
class C2(val x: String)

trait LowPriority {
  implicit def lessSpecific[T]: DynamicMaterialization[T] = null
}

object DynamicMaterialization extends LowPriority {
  implicit def moreSpecific[T]: DynamicMaterialization[T] = macro DynamicMaterializationBundle.impl[T]
}

class DynamicMaterializationBundle(val c: Context { type PrefixType = Nothing }) {
  import c.universe._
  def impl[T: c.WeakTypeTag] = {
    val tpe = weakTypeOf[T]
    if (tpe.members.exists(_.info =:= typeOf[Int]))
      c.abort(c.enclosingPosition, "I don't like classes that contain integers")
    q"new DynamicMaterialization[$tpe]{ override def toString = ${tpe.toString} }"
  }
}

// whitebox use case #4: extractor macros

object ExtractorMacro {
  def unapply(x: Int): Any = macro ExtractorBundle.unapplyImpl
}

class ExtractorBundle(val c: Context { type PrefixType = Nothing }) {
  import c.universe._
  def unapplyImpl(x: Tree) = {
    q"""
      new {
        class Match(x: Int) {
          def isEmpty = false
          def get = x
        }
        def unapply(x: Int) = new Match(x)
      }.unapply($x)
    """
  }
}
