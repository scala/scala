/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package typechecker

import java.lang.ArithmeticException

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class ConstantFolder {

  val global: Global
  import global._

  // We can fold side effect free terms and their types
  object FoldableTerm {
    def unapply(tree: Tree): Option[Constant] = tree match {
      case Literal(x) => Some(x)
      case term if term.symbol != null && !term.symbol.isLazy && (term.symbol.isVal || (term.symbol.isGetter && term.symbol.accessed.isVal)) =>
        extractConstant(term.tpe)
      case _ => None
    }
  }

  // We can fold the types of side effecting terms, but not the terms themselves
  object ConstantTerm {
    def unapply(tree: Tree): Option[Constant] = extractConstant(tree.tpe)
  }

  private def extractConstant(tpe: Type): Option[Constant] =
    tpe match {
      case ConstantType(x) => Some(x)
      case st: SingleType =>
        st.underlying match {
          case ConstantType(x) => Some(x)
          case _ => None
        }
      case _ => None
    }

  /** If tree is a constant operation, replace with result. */
  def apply(tree: Tree): Tree = {
    try {
      tree match {
        case Apply(Select(FoldableTerm(x), op), List(FoldableTerm(y))) => fold(tree, foldBinop(op, x, y), true)
        case Apply(Select(ConstantTerm(x), op), List(ConstantTerm(y))) => fold(tree, foldBinop(op, x, y), false)
        case Select(FoldableTerm(x), op) => fold(tree, foldUnop(op, x), true)
        case Select(ConstantTerm(x), op) => fold(tree, foldUnop(op, x), false)
        case _ => tree
      }
    } catch {
      case e: ArithmeticException =>
        if (settings.warnConstant)
          reporter.warning(tree.pos, s"Evaluation of a constant expression results in an arithmetic error: ${e.getMessage}")
        tree
    }
  }

  /** If tree is a constant value that can be converted to type `pt`, perform
   *  the conversion.
   */
  def apply(tree: Tree, pt: Type): Tree = {
    val orig = apply(tree)
    orig.tpe match {
      case tp@ConstantType(x) => fold(orig, x convertTo pt, isConstantType(tp))
      case _ => orig
    }
  }

  private def fold(orig: Tree, folded: Constant, foldable: Boolean): Tree =
    if ((folded eq null) || folded.tag == UnitTag) orig
    else if(foldable) orig setType FoldableConstantType(folded)
    else orig setType LiteralType(folded)

  private def foldUnop(op: Name, x: Constant): Constant = (op, x.tag) match {
    case (nme.UNARY_!, BooleanTag) => Constant(!x.booleanValue)

    case (nme.UNARY_~ , IntTag    ) => Constant(~x.intValue)
    case (nme.UNARY_~ , LongTag   ) => Constant(~x.longValue)

    case (nme.UNARY_+ , IntTag    ) => Constant(+x.intValue)
    case (nme.UNARY_+ , LongTag   ) => Constant(+x.longValue)
    case (nme.UNARY_+ , FloatTag  ) => Constant(+x.floatValue)
    case (nme.UNARY_+ , DoubleTag ) => Constant(+x.doubleValue)

    case (nme.UNARY_- , IntTag    ) => Constant(-x.intValue)
    case (nme.UNARY_- , LongTag   ) => Constant(-x.longValue)
    case (nme.UNARY_- , FloatTag  ) => Constant(-x.floatValue)
    case (nme.UNARY_- , DoubleTag ) => Constant(-x.doubleValue)

    case _ => null
  }

  /** These are local helpers to keep foldBinop from overly taxing the
   *  optimizer.
   */
  private def foldBooleanOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.ZOR  => Constant(x.booleanValue | y.booleanValue)
    case nme.OR   => Constant(x.booleanValue | y.booleanValue)
    case nme.XOR  => Constant(x.booleanValue ^ y.booleanValue)
    case nme.ZAND => Constant(x.booleanValue & y.booleanValue)
    case nme.AND  => Constant(x.booleanValue & y.booleanValue)
    case nme.EQ   => Constant(x.booleanValue == y.booleanValue)
    case nme.NE   => Constant(x.booleanValue != y.booleanValue)
    case _        => null
  }
  private def foldSubrangeOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.OR  => Constant(x.intValue | y.intValue)
    case nme.XOR => Constant(x.intValue ^ y.intValue)
    case nme.AND => Constant(x.intValue & y.intValue)
    case nme.LSL => Constant(x.intValue << y.intValue)
    case nme.LSR => Constant(x.intValue >>> y.intValue)
    case nme.ASR => Constant(x.intValue >> y.intValue)
    case nme.EQ  => Constant(x.intValue == y.intValue)
    case nme.NE  => Constant(x.intValue != y.intValue)
    case nme.LT  => Constant(x.intValue < y.intValue)
    case nme.GT  => Constant(x.intValue > y.intValue)
    case nme.LE  => Constant(x.intValue <= y.intValue)
    case nme.GE  => Constant(x.intValue >= y.intValue)
    case nme.ADD => Constant(x.intValue + y.intValue)
    case nme.SUB => Constant(x.intValue - y.intValue)
    case nme.MUL => Constant(x.intValue * y.intValue)
    case nme.DIV => Constant(x.intValue / y.intValue)
    case nme.MOD => Constant(x.intValue % y.intValue)
    case _       => null
  }
  private def foldLongOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.OR  => Constant(x.longValue | y.longValue)
    case nme.XOR => Constant(x.longValue ^ y.longValue)
    case nme.AND => Constant(x.longValue & y.longValue)
    case nme.LSL if x.tag <= IntTag
                 => Constant(x.intValue << y.longValue)
    case nme.LSL => Constant(x.longValue <<  y.longValue)
    case nme.LSR if x.tag <= IntTag
                 => Constant(x.intValue >>> y.longValue)
    case nme.LSR => Constant(x.longValue >>> y.longValue)
    case nme.ASR if x.tag <= IntTag
                 => Constant(x.intValue >> y.longValue)
    case nme.ASR => Constant(x.longValue >> y.longValue)
    case nme.EQ  => Constant(x.longValue == y.longValue)
    case nme.NE  => Constant(x.longValue != y.longValue)
    case nme.LT  => Constant(x.longValue < y.longValue)
    case nme.GT  => Constant(x.longValue > y.longValue)
    case nme.LE  => Constant(x.longValue <= y.longValue)
    case nme.GE  => Constant(x.longValue >= y.longValue)
    case nme.ADD => Constant(x.longValue + y.longValue)
    case nme.SUB => Constant(x.longValue - y.longValue)
    case nme.MUL => Constant(x.longValue * y.longValue)
    case nme.DIV => Constant(x.longValue / y.longValue)
    case nme.MOD => Constant(x.longValue % y.longValue)
    case _       => null
  }
  private def foldFloatOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.EQ  => Constant(x.floatValue == y.floatValue)
    case nme.NE  => Constant(x.floatValue != y.floatValue)
    case nme.LT  => Constant(x.floatValue < y.floatValue)
    case nme.GT  => Constant(x.floatValue > y.floatValue)
    case nme.LE  => Constant(x.floatValue <= y.floatValue)
    case nme.GE  => Constant(x.floatValue >= y.floatValue)
    case nme.ADD => Constant(x.floatValue + y.floatValue)
    case nme.SUB => Constant(x.floatValue - y.floatValue)
    case nme.MUL => Constant(x.floatValue * y.floatValue)
    case nme.DIV => Constant(x.floatValue / y.floatValue)
    case nme.MOD => Constant(x.floatValue % y.floatValue)
    case _       => null
  }
  private def foldDoubleOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.EQ  => Constant(x.doubleValue == y.doubleValue)
    case nme.NE  => Constant(x.doubleValue != y.doubleValue)
    case nme.LT  => Constant(x.doubleValue < y.doubleValue)
    case nme.GT  => Constant(x.doubleValue > y.doubleValue)
    case nme.LE  => Constant(x.doubleValue <= y.doubleValue)
    case nme.GE  => Constant(x.doubleValue >= y.doubleValue)
    case nme.ADD => Constant(x.doubleValue + y.doubleValue)
    case nme.SUB => Constant(x.doubleValue - y.doubleValue)
    case nme.MUL => Constant(x.doubleValue * y.doubleValue)
    case nme.DIV => Constant(x.doubleValue / y.doubleValue)
    case nme.MOD => Constant(x.doubleValue % y.doubleValue)
    case _       => null
  }

  private def foldBinop(op: Name, x: Constant, y: Constant): Constant = {
    val optag =
      if (x.tag == y.tag) x.tag
      else if (x.isNumeric && y.isNumeric) math.max(x.tag, y.tag)
      else NoTag

    optag match {
      case BooleanTag                               => foldBooleanOp(op, x, y)
      case ByteTag | ShortTag | CharTag | IntTag    => foldSubrangeOp(op, x, y)
      case LongTag                                  => foldLongOp(op, x, y)
      case FloatTag                                 => foldFloatOp(op, x, y)
      case DoubleTag                                => foldDoubleOp(op, x, y)
      case StringTag if op == nme.ADD               => Constant(x.stringValue + y.stringValue)
      case _                                        => null
    }
  }
}
