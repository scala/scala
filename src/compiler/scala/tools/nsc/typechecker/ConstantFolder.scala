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

  /** Assign a ConstantType to a tree that evaluates to a statically known constant that's coercible to `pt`.
    *
    * Used during type checking. In erasure, the tree will be replaced by the literal denoted by its type.
    */
  def apply(tree: Tree, pt: Type = WildcardType): Tree = tree match {
    case Constable(const) =>
      if (pt eq WildcardType) tree setType ConstantType(const)
      else {
        val adaptedConst = const convertTo pt
        if (adaptedConst ne null) tree setType ConstantType(adaptedConst)
        else tree
      }
    case _ => tree
  }

  // Can tree be folded to a constant?
  private object Constable {
    import treeInfo.LiteralLike
    def fold(tree: Tree): Constant =
      try tree match {
        case Literal(c) => c
        // the Const extractor in TreeInfo looks both at the tree and its type (tree was folded previously)
        // TODO should we go deep and use our own extractor (Constable)? probably doesn't make sense because the type checker already drives the recursion
        case Apply(Select(LiteralLike(x), op), List(LiteralLike(y))) => foldBinop(op, x, y)
        case Select(LiteralLike(x), op) => foldUnop(op, x)
        case _ => tree.tpe match {
          case ConstantType(c) => c
          case _ => null
        }
      } catch { case _: ArithmeticException => null }

    def unapply(t: Tree): Option[Constant] = fold(t) match {
      case const if (const ne null) && const.tag != UnitTag => Some(const)
      case _ => None
    }
  }

  private def foldUnop(op: Name, x: Constant): Constant = (op, x.tag) match {
    case (nme.UNARY_!, BooleanTag)  => Constant(!x.booleanValue)

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
    case _ => null
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
    case _ => null
  }
  private def foldLongOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.OR  => Constant(x.longValue | y.longValue)
    case nme.XOR => Constant(x.longValue ^ y.longValue)
    case nme.AND => Constant(x.longValue & y.longValue)
    case nme.LSL => Constant(x.longValue << y.longValue)
    case nme.LSR => Constant(x.longValue >>> y.longValue)
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
    case _ => null
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
    case _ => null
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
    case _ => null
  }

  private def foldBinop(op: Name, x: Constant, y: Constant): Constant = {
    val optag =
      if (x.tag == y.tag) x.tag
      else if (x.isNumeric && y.isNumeric) math.max(x.tag, y.tag)
      else NoTag // TODO: string concatenation when either tag is a String? TODO: also support string interpolation?

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
