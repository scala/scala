/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
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
  import Code._

  /** Methods to simplify code generation
   */
  object Code {
    // function application
    def fn(lhs: Tree, op:   Name, args: Tree*)  = Apply(Select(lhs, op), args.toList)
    def fn(lhs: Tree, op: Symbol, args: Tree*)  = Apply(Select(lhs, op), args.toList)

    val AND   = definitions.Boolean_and
    val NOT   = definitions.Boolean_not
    val SEQ   = definitions.SeqClass
    val SOME  = definitions.SomeClass
    val TRUE  = Const(true)
    val FALSE = Const(false)
    val NULL  = Const(null)

    object Const {
      def apply(x: Any) = Literal(Constant(x))
      def unapply(x: Any) = x match {
        case Literal(Constant(value)) => Some(value)
        case _ => None
      }
    }
  }

  final def typedValDef(x: Symbol, rhs: Tree)(implicit typer: Typer) = x.tpe match {
    case WildcardType =>
      rhs setType null
      x setInfo typer.typed(rhs).tpe
      typer.typed(ValDef(x, rhs))
    case _ =>
      typer.typed(ValDef(x, typer.typed(rhs, x.tpe)))
  }

  final def mkIdent(sym: Symbol)  = Ident(sym) setType sym.tpe
  final def mk_(tpe: Type)        = Ident(nme.WILDCARD) setType tpe

  /** returns A for T <: Sequence[ A ]
   */
  final def getElemType_Sequence(tpe: Type): Type = {
    val tpe1 = tpe.widen.baseType(SEQ)
    if (tpe1 == NoType)
      Predef.error("arg " + tpe + " not subtype of Seq[A]")

    tpe1.typeArgs(0)
  }

  // Option fullness check
  final def nonEmptinessCheck(vsym: Symbol) =
    if (vsym.tpe.typeSymbol == SOME) TRUE           // is Some[_]
    else Not(Select(mkIdent(vsym), nme.isEmpty))    // is Option[_]

  /** for tree of sequence type, returns tree that drops first i elements */
  final def seqDrop(sel:Tree, ix: Int)(implicit typer : Typer) =
    if (ix == 0) sel else
    typer.typed(Select(fn(sel, nme.drop, Const(ix)), nme.toSeq))

  /** for tree of sequence type, returns tree that represents element at index i */
  final def seqElement(sel:Tree, ix: Int)(implicit typer : Typer) =
    typer.typed(fn(sel, sel.tpe.member(nme.apply), Const(ix)))

  /** for tree of sequence type, returns boolean tree testing that the sequence has length i */
  final def seqHasLength(sel: Tree, ntpe: Type, i: Int)(implicit typer : Typer) =
    typer.typed( Equals(fn(sel, ntpe.member(nme.lengthCompare), Const(i)), Const(0)) )     // defs.Seq_length ?

  /** for tree of sequence type sel, returns boolean tree testing that length >= i
   */
  final def seqLongerThan(sel:Tree, tpe:Type, i:Int)(implicit typer : Typer) = {
    val cmp = fn(sel, tpe.member(nme.lengthCompare), Const(i))
    GTE(typer.typed(cmp), typer.typed(Const(0)))  // defs.Seq_length instead of tpe.member?
  }

  final def Equals  (left: Tree, right: Tree): Tree = fn(left, nme.EQ, right)
  final def Eq      (left: Tree, right: Tree): Tree = fn(left, nme.eq, right)
  final def GTE     (left: Tree, right: Tree): Tree = fn(left, nme.GE, right) // >=
  final def And     (terms: Tree*): Tree = terms.reduceLeft((left, right) => fn(left, AND, right))
  final def Not     (arg: Tree): Tree = Select(arg, NOT)

  final def ThrowMatchError(pos: Position, obj: Tree) = atPos(pos) {
    Throw( New(TypeTree(MatchErrorClass.tpe), List(List(obj))) )
  }

  final def NotNull(tree: Tree)(implicit typer : Typer) = typer.typed(fn(tree, nme.ne, NULL))
  final def Get(tree: Tree) = fn(tree, nme.get)

  // statistics
  var nremoved = 0
  var nsubstituted = 0

  final def squeezedBlock(vds: List[Tree], exp: Tree)(implicit theOwner: Symbol): Tree =
    if (settings_squeeze) Block(Nil, squeezedBlock1(vds, exp))
    else                  Block(vds, exp)

  final def squeezedBlock1(vds: List[Tree], exp: Tree)(implicit theOwner: Symbol): Tree = {
    val tpe = exp.tpe

    class RefTraverser(sym: Symbol) extends Traverser {
      var nref = 0
      var nsafeRef = 0
      override def traverse(tree: Tree) = tree match {
        case t:Ident if t.symbol eq sym =>
          nref += 1
          if(sym.owner == currentOwner)  { // oldOwner should match currentOwner
            nsafeRef += 1
          }
        case LabelDef(_,args,rhs) =>
          var args1 = args; while(args1 ne Nil) {
            if(args1.head.symbol eq sym) {
              nref += 2   // will abort traversal, cannot substitute this one
              args1 = Nil // break
            } else {
              args1 = args1.tail
            }
          }
          traverse(rhs)
        case t if nref > 1 =>
          // abort, no story to tell
        case t =>
          super.traverse(t)
      }
    }

    class Subst(sym: Symbol, rhs: Tree) extends Transformer {
      var stop = false
      override def transform(tree: Tree) = tree match {
        case t:Ident if t.symbol == sym =>
          stop = true
          rhs
        case t if stop =>
          t
        case t =>
          super.transform(t)
      }
    }
    vds match {
      case Nil =>
        exp
      case (vd:ValDef) :: rest =>
        // recurse
        val exp1 = squeezedBlock(rest, exp)

        val sym = vd.symbol
        val rt = new RefTraverser(sym)
        rt.atOwner (theOwner) (rt.traverse(exp1))
        rt.nref match {
          case 0 =>
            nremoved += 1
            exp1
          case 1 if rt.nsafeRef == 1 =>
            nsubstituted += 1
            new Subst(sym, vd.rhs).transform(exp1)
          case _ =>
            exp1 match {
              case Block(vds2, exp2) => Block(vd::vds2, exp2)
              case exp2              => Block(vd::Nil,  exp2)
            }
        }
      case x::xs =>
        squeezedBlock(xs, exp) match {
          case Block(vds2, exp2) => Block(x::vds2, exp2)
          case exp2              => Block(x::Nil,  exp2)
        }
    }
  }
}

