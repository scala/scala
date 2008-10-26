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

  import global._

  import definitions._             // standard classes and methods
  import typer.typed               // methods to type trees
  import posAssigner.atPos         // for filling in tree positions

  final def mkIdent(sym:Symbol) = Ident(sym) setType sym.tpe

  final def typedValDef(x:Symbol, rhs:Tree) = {
    x.tpe match {
      case WildcardType => rhs.setType(null); val rhs1 = typed(rhs); x setInfo rhs1.tpe; typed{ValDef(x, rhs)}
      case _ => typed{ValDef(x, typed(rhs, x.tpe))}
    }
  }

  final def mk_(tpe:Type) = Ident(nme.WILDCARD) setType tpe

  /**
   * Convert a pattern binding into a list of value definitions.
   */
  final def targetParams(subst:Binding):List[ValDef] = subst match {
    case NoBinding => Nil;
    case Binding(v,t,n) => ValDef(v, typed(mkIdent(t)))::targetParams(n)
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
  final def seqDrop(sel:Tree, ix: Int) = if (ix == 0) sel else
    typed { Select(Apply(Select(sel, nme.drop), List(Literal(Constant(ix)))), nme.toSeq) }

  /** for tree of sequence type, returns tree that drops first i elements */
  final def seqElement(sel:Tree, ix: Int) =
    typed { Apply(Select(sel, sel.tpe.member(nme.apply)), List(Literal(Constant(ix)))) }

  /** for tree of sequence type, returns boolean tree testing that the sequence has length i */
  final def seqHasLength(sel: Tree, ntpe: Type, i: Int) =
    typed(
      Equals(
        Apply(Select(sel, ntpe.member(nme.lengthCompare)), List(Literal(Constant(i)))),
        Literal(Constant(0))
      )
    )/*defs.Seq_length ?*/

  /** for tree of sequence type sel, returns boolean tree testing that length >= i
   */
  final def seqLongerThan(sel:Tree, tpe:Type, i:Int) =
    GreaterThanOrEquals(
      typed(Apply(Select(sel, tpe.member(nme.lengthCompare)), List(Literal(Constant(i))))),
      typed(Literal(Constant(0))))
      //defs.Seq_length instead of tpe.member ?

  final def Not(arg:Tree) = arg match {
    case Literal(Constant(true))  => Literal(Constant(false))
    case Literal(Constant(false)) => Literal(Constant(true))
    case t                        => Select(arg, definitions.Boolean_not)
  }
  /*protected*/ def And(left: Tree, right: Tree): Tree = left match {
    case Literal(Constant(value: Boolean)) =>
      if (value) right else left
    case _ =>
      right match {
        case Literal(Constant(true)) =>
	  left
        case _ =>
          Apply(Select(left, definitions.Boolean_and), List(right))
      }
  }

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

  final def NotNull(tree:Tree) =
    typed {
      Apply(Select(tree, nme.ne), List(Literal(Constant(null))))
    }

    // statistics
    var nremoved = 0
    var nsubstituted = 0

  final def squeezedBlock(vds: List[Tree], exp: Tree)(implicit theOwner: Symbol): Tree =
    if (settings_squeeze)
      squeezedBlock1(vds, exp)
    else
      Block(vds,exp)

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
          } /*else if(nref == 1) {
            Console.println("sym owner: "+sym.owner+" but currentOwner = "+currentOwner)
          }*/
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

        //Console.println("squeezedBlock for valdef "+vd)
        val sym = vd.symbol
        val rt = new RefTraverser(sym)
        rt.atOwner (theOwner) (rt.traverse(exp1))
      //Console.println("hello, ref count = "+rt.nref+"/"+rt.nsafeRef)
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

  final def debugStrings(tps:List[Type]):String = tps.foldLeft("[")({ (x:String,y:Type) => x+","+debugString(y) })+"]"

  final def debugString(tp:Type):String = tp match {
    case ErrorType => "ErrorType"
    // internal: error
    case WildcardType => "WildcardType"
    // internal: unknown
    case NoType => "NoType"
    case NoPrefix => "NoPrefix"
    case ThisType(sym) => "ThisType("+sym.toString+")"
    // sym.this.type
    case SingleType(pre, sym) => "SingleType("+debugString(pre)+","+sym+")"
    // pre.sym.type
    case ConstantType(value) => "ConstantType("+value+")"
    // int(2)
    case TypeRef(pre, sym, args) => "TypeRef("+debugString(pre)+","+sym.getClass()+"(=="+sym+")"+","+ debugStrings(args)+")"
    // pre.sym[targs]
    case RefinedType(parents, defs) => "RefinedType("+debugStrings(parents)+","+defs+")"
    // parent1 with ... with parentn { defs }
    case AnnotatedType(attribs, tp, selfsym) => "AnnotatedType("+attribs+","+ debugString(tp)+","+ selfsym+")"
    // tp @attribs
    case p:Product => tp.getClass.toString+"(=="+tp.toString+")"+runtime.ScalaRunTime._toString(p)

    case _ => tp.getClass.toString+"(=="+tp.toString+")"
    /*
     // the following are non-value types; you cannot write them down in Scala source.

     case TypeBounds(lo, hi) =>
     // >: lo <: hi
     case ClassInfoType(parents, defs, clazz) =>
     // same as RefinedType except as body of class
     case MethodType(paramtypes, result) =>
     // (paramtypes)result
     case PolyType(tparams, result) =>
     // [tparams]result where result is a MethodType or ClassInfoType
     // or
     // []T  for a eval-by-name type
     case ExistentialType(tparams, result) =>
     // exists[tparams]result
     */
  }
}

