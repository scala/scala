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
      case Apply(TypeApply(sel @ Select(Apply(recv, Nil), _), targs), Nil) => Some((recv, sel.symbol, targs map (_.tpe)))
      case _                                                               => None
    }
  }
  // x.asInstanceOf[T] returns (x, typeOf[T])
  object AsInstanceOf {
    def unapply(t: Tree): Option[(Tree, Type)] = t match {
      case TypeApplyOp(recv, Object_asInstanceOf, tpe :: Nil) => Some((recv, tpe))
      case _                                                  => None
    }
  }

  // Extractors for value classes.
  object ValueClass {
    def isValueClass(tpt: Tree)                = enteringErasure(tpt.tpe.typeSymbol.isDerivedValueClass)
    def valueUnbox(tpe: Type)                  = tpe.typeSymbol.derivedValueClassUnbox
    def isValueClassOp(tpt1: Tree, tpt2: Tree) = enteringErasure(isValueClass(tpt1) && tpt2.tpe.typeSymbol == tpt1.tpe.typeSymbol)

    // arg1.op(arg2) where arg1 and arg2 are the same boxed value class. Returns unboxed values.
    object BinaryOp {
      def unapply(t: Tree): Option[(Tree, Symbol, Tree)] = t match {
        case Apply(sel @ Select(Box(t1, arg1), _), Box(t2, arg2) :: Nil) if isValueClassOp(t1, t2) => Some((arg1, sel.symbol, arg2))
        case _                                                                                     => None
      }
    }
    // x.underlying() where x is a value class and underlying is its field.
    object Underlying {
      def unapply(t: Tree): Option[Tree] = t match {
        case Apply(sel @ Select(Box(tpt, arg), _), Nil) if sel.symbol == valueUnbox(tpt.tpe) => Some(arg)
        case _                                                                               => None
      }
    }
    // new Boxed(underlying).  Returns "Boxed" and underlying.
    object Box {
      def unapply(t: Tree): Option[(Tree, Tree)] = t match {
        case Apply(Select(New(tpt), nme.CONSTRUCTOR), arg :: Nil) => Some((tpt, arg))
        case _                                                    => None
      }
    }
    // x.asInstanceOf[T] where x is a call to a value class's unbox method.  Returns the underlying value of the receiving instance.
    object Cast {
      def unapply(t: Tree): Option[Tree] = t match {
        case AsInstanceOf(sel @ Select(arg, _), tpe) if sel.symbol == valueUnbox(tpe) => Some(arg)
        case _                                                                        => None
      }
    }
  }

  /** Is tree legal as a member definition of an interface?
   */
  override def isInterfaceMember(tree: Tree): Boolean = tree match {
    case DocDef(_, definition)         => isInterfaceMember(definition)
    case _ => super.isInterfaceMember(tree)
  }

  /** Is tree a pure (i.e. non-side-effecting) definition?
   */
  override def isPureDef(tree: Tree): Boolean = tree match {
    case DocDef(_, definition) => isPureDef(definition)
    case _ => super.isPureDef(tree)
  }

 /** Does list of trees start with a definition of
   *  a class of module with given name (ignoring imports)
   */
  override def firstDefinesClassOrObject(trees: List[Tree], name: Name): Boolean = trees match {
    case ClassDef(_, `name`, _, _) :: Nil => true
    case _ => super.firstDefinesClassOrObject(trees, name)
  }
}
