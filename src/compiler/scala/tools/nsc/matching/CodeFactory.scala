/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

import scala.tools.nsc.util.Position

/** Helper methods that build trees for pattern matching.
 *
 *  @author Burak Emir
 */
trait CodeFactory {
  self: transform.ExplicitOuter with PatternNodes =>

  import global.{typer => _, _}
  import analyzer.Typer;

  import definitions._             // standard classes and methods
  import posAssigner.atPos         // for filling in tree positions

  final def mkIdent(sym:Symbol) = Ident(sym) setType sym.tpe

  final def typedValDef(x:Symbol, rhs:Tree)(implicit typer : Typer) = {
    x.tpe match {
      case WildcardType => rhs.setType(null); val rhs1 = typer.typed(rhs); x setInfo rhs1.tpe; typer.typed{ValDef(x, rhs)}
      case _ => typer.typed{ValDef(x, typer.typed(rhs, x.tpe))}
    }
  }

  final def mk_(tpe:Type) = Ident(nme.WILDCARD) setType tpe

  /**
   * Convert a pattern binding into a list of value definitions.
   */
  final def targetParams(subst:Binding)(implicit typer : Typer):List[ValDef] = subst match {
    case NoBinding => Nil;
    case Binding(v,t,n) => ValDef(v, typer.typed(mkIdent(t)))::targetParams(n)
  }

  /** returns A for T <: Sequence[ A ]
   */
  final def getElemType_Sequence(tpe: Type): Type = {
    val tpe1 = tpe.widen.baseType(definitions.SeqClass)

    if (tpe1 == NoType)
      Predef.error("arg " + tpe + " not subtype of Seq[A]")

    tpe1.typeArgs(0)
  }

  final def emptynessCheck(vsym: Symbol) = {
    if (vsym.tpe.typeSymbol == definitions.SomeClass)  // is Some[_]
      Literal(Constant(true))
    else                                          // is Option[_]
      Not(Select(mkIdent(vsym), nme.isEmpty))
  }

  /** for tree of sequence type, returns tree that drops first i elements */
  final def seqDrop(sel:Tree, ix: Int)(implicit typer : Typer) = if (ix == 0) sel else
    typer.typed { Select(Apply(Select(sel, nme.drop), List(Literal(Constant(ix)))), nme.toSeq) }

  /** for tree of sequence type, returns tree that drops first i elements */
  final def seqElement(sel:Tree, ix: Int)(implicit typer : Typer) =
    typer.typed { Apply(Select(sel, sel.tpe.member(nme.apply)), List(Literal(Constant(ix)))) }

  /** for tree of sequence type, returns boolean tree testing that the sequence has length i */
  final def seqHasLength(sel: Tree, ntpe: Type, i: Int)(implicit typer : Typer) =
    typer.typed(
      Equals(
        Apply(Select(sel, ntpe.member(nme.lengthCompare)), List(Literal(Constant(i)))),
        Literal(Constant(0))
      )
    )

  /** for tree of sequence type sel, returns boolean tree testing that length >= i
   */
  final def seqLongerThan(sel:Tree, tpe:Type, i:Int)(implicit typer : Typer) =
    GreaterThanOrEquals(
      typer.typed(Apply(Select(sel, tpe.member(nme.lengthCompare)), List(Literal(Constant(i))))),
      typer.typed(Literal(Constant(0))))

  final def Not(arg:Tree) =
    Select(arg, definitions.Boolean_not)

  def And(left: Tree, right: Tree): Tree =
    Apply(Select(left, definitions.Boolean_and), List(right))

  final def Equals(left: Tree, right: Tree): Tree =
    Apply(Select(left, nme.EQ), List(right))

  final def Eq(left: Tree, right: Tree): Tree =
    Apply(Select(left, nme.eq), List(right))

  final def GreaterThanOrEquals(left: Tree, right: Tree): Tree =
    Apply(Select(left, nme.GE), List(right))

  final def ThrowMatchError(pos: Position, obj: Tree) =
    atPos(pos) {
      Throw(
        New(
          TypeTree(definitions.MatchErrorClass.tpe),
          List(List(
            obj
          ))))
    }

  final def NotNull(tree:Tree)(implicit typer : Typer) =
    typer.typed {
      Apply(Select(tree, nme.ne), List(Literal(Constant(null))))
    }

  final def Get(tree : Tree)
      = Apply(Select(tree, nme.get), List())

}
