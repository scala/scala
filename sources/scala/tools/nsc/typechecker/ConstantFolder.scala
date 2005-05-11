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

  private val NoValue = new Object();

  /** If tree is a constant operation, replace with result. */
  def apply(tree: Tree): Tree = {
    val newvalue = tree match {
      case Apply(Select(Literal(x), op), List(Literal(y))) => foldBinop(op, x, y)
      case Select(Literal(x), op) => foldUnop(op, x)
      case _ => NoValue
    }
    if (newvalue != NoValue) Literal(newvalue) else tree
  }

  /** If tree is a constant value that can be converted to type `pt', perform the conversion */
  def apply(tree: Tree, pt: Type): Tree = {
    val newvalue = tree match {
      case Literal(value) => foldTyped(value, pt)
      case _ => NoValue
    }
    if (newvalue != NoValue) Literal(newvalue) else tree
  }

  private def foldUnop(op: Name, value: Any): Any = Pair(op, value) match {
    case Pair(nme.ZNOT, x: boolean) => !x

    case Pair(nme.NOT , x: int    ) => ~x
    case Pair(nme.NOT , x: long   ) => ~x

    case Pair(nme.ADD , x: int    ) => +x
    case Pair(nme.ADD , x: long   ) => +x
    case Pair(nme.ADD , x: float  ) => +x
    case Pair(nme.ADD , x: double ) => +x

    case Pair(nme.SUB , x: int    ) => -x
    case Pair(nme.SUB , x: long   ) => -x
    case Pair(nme.SUB , x: float  ) => -x
    case Pair(nme.SUB , x: double ) => -x

    case _ => NoValue
  }

  private def foldBinop(op: Name, lvalue: Any, rvalue: Any): Any = Triple(op, lvalue, rvalue) match {
    case Triple(nme.ZOR , x: boolean, y: boolean) => x | y
    case Triple(nme.OR  , x: boolean, y: boolean) => x | y
    case Triple(nme.OR  , x: int    , y: int    ) => x | y
    case Triple(nme.OR  , x: long   , y: long   ) => x | y

    case Triple(nme.XOR , x: boolean, y: boolean) => x ^ y
    case Triple(nme.XOR , x: int    , y: int    ) => x ^ y
    case Triple(nme.XOR , x: long   , y: long   ) => x ^ y

    case Triple(nme.ZAND, x: boolean, y: boolean) => x & y
    case Triple(nme.AND , x: boolean, y: boolean) => x & y
    case Triple(nme.AND , x: int    , y: int    ) => x & y
    case Triple(nme.AND , x: long   , y: long   ) => x & y

    case Triple(nme.LSL , x: int    , y: int    ) => x << y
    case Triple(nme.LSL , x: long   , y: int    ) => x << y
    case Triple(nme.LSL , x: long   , y: long   ) => x << y

    case Triple(nme.LSR , x: int    , y: int    ) => x >>> y
    case Triple(nme.LSR , x: long   , y: int    ) => x >>> y
    case Triple(nme.LSR , x: long   , y: long   ) => x >>> y

    case Triple(nme.ASR , x: int    , y: int    ) => x >> y
    case Triple(nme.ASR , x: long   , y: int    ) => x >> y
    case Triple(nme.ASR , x: long   , y: long   ) => x >> y

    case Triple(nme.EQ  , x: boolean, y: boolean) => x == y
    case Triple(nme.EQ  , x: int    , y: int    ) => x == y
    case Triple(nme.EQ  , x: long   , y: long   ) => x == y
    case Triple(nme.EQ  , x: float  , y: float  ) => x == y
    case Triple(nme.EQ  , x: double , y: double ) => x == y

    case Triple(nme.NE  , x: boolean, y: boolean) => x != y
    case Triple(nme.NE  , x: int    , y: int    ) => x != y
    case Triple(nme.NE  , x: long   , y: long   ) => x != y
    case Triple(nme.NE  , x: float  , y: float  ) => x != y
    case Triple(nme.NE  , x: double , y: double ) => x != y

    case Triple(nme.LT  , x: int    , y: int    ) => x < y
    case Triple(nme.LT  , x: long   , y: long   ) => x < y
    case Triple(nme.LT  , x: float  , y: float  ) => x < y
    case Triple(nme.LT  , x: double , y: double ) => x < y

    case Triple(nme.GT  , x: int    , y: int    ) => x > y
    case Triple(nme.GT  , x: long   , y: long   ) => x > y
    case Triple(nme.GT  , x: float  , y: float  ) => x > y
    case Triple(nme.GT  , x: double , y: double ) => x > y

    case Triple(nme.LE  , x: int    , y: int    ) => x <= y
    case Triple(nme.LE  , x: long   , y: long   ) => x <= y
    case Triple(nme.LE  , x: float  , y: float  ) => x <= y
    case Triple(nme.LE  , x: double , y: double ) => x <= y

    case Triple(nme.GE  , x: int    , y: int    ) => x >= y
    case Triple(nme.GE  , x: long   , y: long   ) => x >= y
    case Triple(nme.GE  , x: float  , y: float  ) => x >= y
    case Triple(nme.GE  , x: double , y: double ) => x >= y

    case Triple(nme.ADD , x: int    , y: int    ) => x + y
    case Triple(nme.ADD , x: long   , y: long   ) => x + y
    case Triple(nme.ADD , x: float  , y: float  ) => x + y
    case Triple(nme.ADD , x: double , y: double ) => x + y
    case Triple(nme.ADD , x: String , y: String ) => x + y

    case Triple(nme.SUB , x: int    , y: int    ) => x - y
    case Triple(nme.SUB , x: long   , y: long   ) => x - y
    case Triple(nme.SUB , x: float  , y: float  ) => x - y
    case Triple(nme.SUB , x: double , y: double ) => x - y

    case Triple(nme.MUL , x: int    , y: int    ) => x * y
    case Triple(nme.MUL , x: long   , y: long   ) => x * y
    case Triple(nme.MUL , x: float  , y: float  ) => x * y
    case Triple(nme.MUL , x: double , y: double ) => x * y

    case Triple(nme.DIV , x: int    , y: int    ) => x / y
    case Triple(nme.DIV , x: long   , y: long   ) => x / y
    case Triple(nme.DIV , x: float  , y: float  ) => x / y
    case Triple(nme.DIV , x: double , y: double ) => x / y

    case Triple(nme.MOD , x: int    , y: int    ) => x % y
    case Triple(nme.MOD , x: long   , y: long   ) => x % y
    case Triple(nme.MOD , x: float  , y: float  ) => x % y
    case Triple(nme.MOD , x: double , y: double ) => x % y

    case _ => NoValue
  }

  /** Widen constant value to conform to given type */
  private def foldTyped(value: Any, pt: Type): Any = {
    val target = pt.symbol;
    value match {
      case x: byte =>
	if (target == ShortClass) x.asInstanceOf[short]
	else if (target == CharClass) x.asInstanceOf[char]
	else if (target == IntClass) x.asInstanceOf[int]
	else if (target == LongClass) x.asInstanceOf[long]
	else if (target == FloatClass) x.asInstanceOf[float]
	else if (target == DoubleClass) x.asInstanceOf[double]
	else NoValue
      case x: short =>
	if (target == IntClass) x.asInstanceOf[int]
	else if (target == LongClass) x.asInstanceOf[long]
	else if (target == FloatClass) x.asInstanceOf[float]
	else if (target == DoubleClass) x.asInstanceOf[double]
	else NoValue
      case x: char =>
	if (target == IntClass) x.asInstanceOf[int]
	else if (target == LongClass) x.asInstanceOf[long]
	else if (target == FloatClass) x.asInstanceOf[float]
	else if (target == DoubleClass) x.asInstanceOf[double]
	else NoValue
      case x: int =>
	if (target == ByteClass && -128 <= x && x <= 127) x.asInstanceOf[byte]
	else if (target == ShortClass && -32768 <= x && x <= 32767) x.asInstanceOf[short]
	else if (target == CharClass && 0 <= x && x <= 65535) x.asInstanceOf[char]
	else if (target == LongClass) x.asInstanceOf[long]
	else if (target == FloatClass) x.asInstanceOf[float]
	else if (target == DoubleClass) x.asInstanceOf[double]
	else NoValue
      case x: long =>
	if (target == FloatClass) x.asInstanceOf[float]
	else if (target == DoubleClass) x.asInstanceOf[double]
	else NoValue
      case x: float =>
	if (target == DoubleClass) x.asInstanceOf[double]
	else NoValue
      case x =>
	NoValue
    }
  }
}
