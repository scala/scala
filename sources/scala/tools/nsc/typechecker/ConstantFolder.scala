/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

abstract class ConstantFolder {

  val global: Global;
  import global._;
  import definitions._;

  /** If tree is a constant operation, replace with result. */
  def apply(tree: Tree): Tree = fold(tree, tree match {
    case Apply(Select(Literal(x), op), List(Literal(y))) => foldBinop(op, x, y)
    case Select(Literal(x), op) => foldUnop(op, x)
    case _ => null
  });

  /** If tree is a constant value that can be converted to type `pt', perform the conversion */
  def apply(tree: Tree, pt: Type): Tree = fold(tree, tree.tpe match {
    case ConstantType(x) => x convertTo pt
    case _ => null
  });

  private def fold(tree: Tree, x: Constant): Tree =
    if (x != null && x.tag != UnitTag) tree setType ConstantType(x) else tree;

  private def foldUnop(op: Name, x: Constant): Constant = Pair(op, x.tag) match {
    case Pair(nme.ZNOT, BooleanTag) => Constant(!x.booleanValue)

    case Pair(nme.NOT , IntTag    ) => Constant(~x.intValue)
    case Pair(nme.NOT , LongTag   ) => Constant(~x.longValue)

    case Pair(nme.ADD , IntTag    ) => Constant(+x.intValue)
    case Pair(nme.ADD , LongTag   ) => Constant(+x.longValue)
    case Pair(nme.ADD , FloatTag  ) => Constant(+x.floatValue)
    case Pair(nme.ADD , DoubleTag ) => Constant(+x.doubleValue)

    case Pair(nme.SUB , IntTag    ) => Constant(-x.intValue)
    case Pair(nme.SUB , LongTag   ) => Constant(-x.longValue)
    case Pair(nme.SUB , FloatTag  ) => Constant(-x.floatValue)
    case Pair(nme.SUB , DoubleTag ) => Constant(-x.doubleValue)

    case _ => null
  }

  private def foldBinop(op: Name, x: Constant, y: Constant): Constant = {
    val optag = if (x.tag > y.tag) x.tag else y.tag;
    optag match {
      case BooleanTag =>
	op match {
	  case nme.ZOR  => Constant(x.booleanValue | y.booleanValue)
	  case nme.OR   => Constant(x.booleanValue | y.booleanValue)
	  case nme.XOR  => Constant(x.booleanValue ^ y.booleanValue)
	  case nme.ZAND => Constant(x.booleanValue & y.booleanValue)
	  case nme.AND  => Constant(x.booleanValue & y.booleanValue)
	  case nme.EQ   => Constant(x.booleanValue == y.booleanValue)
	  case nme.NE   => Constant(x.booleanValue != y.booleanValue)
	  case _ => null
	}
      case ByteTag | ShortTag | LongTag | IntTag =>
	op match {
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
      case LongTag =>
	op match {
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
      case FloatTag =>
	op match {
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
      case DoubleTag =>
	op match {
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
      case StringTag =>
	op match {
	  case nme.ADD => Constant(x.stringValue + y.stringValue)
	  case _ => null
	}
      case _ =>
	null
    }
  }
}
