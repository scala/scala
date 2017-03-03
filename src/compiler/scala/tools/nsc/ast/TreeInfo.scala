/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo extends scala.reflect.internal.TreeInfo {
  val global: Global
  import global._
  import definitions._

  // arg1.op(arg2) returns (arg1, op.symbol, arg2)
  object BinaryOp {
    def unapply(t: Tree): Option[(Tree, Symbol, Tree)] = t match {
      case Apply(sel @ Select(arg1, _), arg2 :: Nil) => Some((arg1, sel.symbol, arg2))
      case _                                         => None
    }
  }
  // recv.op[T1, ...] returns (recv, op.symbol, type argument types)
  object TypeApplyOp {
    def unapply(t: Tree): Option[(Tree, Symbol, List[Type])] = t match {
      case TypeApply(sel @ Select(recv, _), targs) => Some((recv, sel.symbol, targs map (_.tpe)))
      case _                                       => None
    }
  }

  // x.asInstanceOf[T] returns (x, typeOf[T])
  object AsInstanceOf {
    def unapply(t: Tree): Option[(Tree, Type)] = t match {
      case Apply(TypeApplyOp(recv, Object_asInstanceOf, tpe :: Nil), Nil) => Some((recv, tpe))
      case _                                                              => None
    }
  }

  // Extractors for value classes.
  object ValueClass {
    def isValueClass(tpe: Type)                  = enteringErasure(tpe.typeSymbol.isDerivedValueClass)
    def valueUnbox(tpe: Type)                    = enteringErasure(tpe.typeSymbol.derivedValueClassUnbox)

    // B.unbox. Returns B.
    object Unbox {
      def unapply(t: Tree): Option[Tree] = t match {
        case Apply(sel @ Select(ref, _), Nil) if valueUnbox(ref.tpe) == sel.symbol => Some(ref)
        case _                                                                     => None
      }
    }
    // new B(v). Returns B and v.
    object Box {
      def unapply(t: Tree): Option[(Tree, Type)] = t match {
        case Apply(sel @ Select(New(tpt), nme.CONSTRUCTOR), v :: Nil) => Some((v, tpt.tpe.finalResultType))
        case _                                                        => None
      }
    }
    // (new B(v)).unbox. returns v.
    object BoxAndUnbox {
      def unapply(t: Tree): Option[Tree] = t match {
        case Unbox(Box(v, tpe)) if isValueClass(tpe) => Some(v)
        case _                                       => None
      }
    }
    // new B(v1) op new B(v2) where op is == or !=. Returns v1, op, v2.
    object BoxAndCompare {
      def unapply(t: Tree): Option[(Tree, Symbol, Tree)] = t match {
        case BinaryOp(Box(v1, tpe1), op @ (Object_== | Object_!=), Box(v2, tpe2)) if isValueClass(tpe1) && tpe1 =:= tpe2 => Some((v1, op, v2))
        case _                                                                                                           => None
      }
    }
  }

  // TODO these overrides, and the slow trickle of bugs that they solve (e.g. SI-8479),
  //      suggest that we should pursue an alternative design in which the DocDef nodes
  //      are eliminated from the tree before typer, and instead are modelled as tree
  //      attachments.

  /** Is tree legal as a member definition of an interface?
   */
  override def isInterfaceMember(tree: Tree): Boolean = tree match {
    case DocDef(_, definition)         => isInterfaceMember(definition)
    case _ => super.isInterfaceMember(tree)
  }

  override def isConstructorWithDefault(t: Tree) = t match {
    case DocDef(_, definition) => isConstructorWithDefault(definition)
    case _ => super.isConstructorWithDefault(t)
  }

  /** Is tree a pure (i.e. non-side-effecting) definition?
   */
  override def isPureDef(tree: Tree): Boolean = tree match {
    case DocDef(_, definition) => isPureDef(definition)
    case _ => super.isPureDef(tree)
  }

  override def firstConstructor(stats: List[Tree]): Tree = {
    def unwrap(stat: Tree): Tree = stat match {
      case DocDef(_, defn) => unwrap(defn)
      case tree => tree
    }
    super.firstConstructor(stats map unwrap)
  }
}
