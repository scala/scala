/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools.nsc
package typechecker

import java.lang.ArithmeticException

import scala.tools.nsc.Reporting.WarningCategory
import scala.util.control.ControlThrowable

/** This class ...
 *
 *  @author Martin Odersky
 */
abstract class ConstantFolder {

  val global: Global
  import global._

  val foldableUnaryOps: Set[Name] = nme.isEncodedUnary ++ List(nme.toChar, nme.toInt, nme.toLong, nme.toFloat, nme.toDouble)

  // We can fold side effect free terms and their types
  object FoldableTerm {
    @inline private def effectless(sym: Symbol): Boolean = sym != null && !sym.isLazy && (sym.isVal || sym.isGetter && sym.accessed.isVal)

    def unapply(tree: Tree): Option[Constant] = tree match {
      case Literal(x)                      => Some(x)
      case term if effectless(term.symbol) => extractConstant(term.tpe)
      case _                               => None
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
  def apply(tree: Tree, site: Symbol): Tree = if (isPastTyper) tree else
    try {
      tree match {
        case Apply(Select(FoldableTerm(x), op), List(FoldableTerm(y))) => fold(tree, safelyFoldBinop(tree, site)(op, x, y), foldable = true)
        case Apply(Select(ConstantTerm(x), op), List(ConstantTerm(y))) => fold(tree, safelyFoldBinop(tree, site)(op, x, y), foldable = false)
        case Select(FoldableTerm(x), op) => fold(tree, foldUnop(op, x), foldable = true)
        case Select(ConstantTerm(x), op) => fold(tree, foldUnop(op, x), foldable = false)
        case _ => tree
      }
    } catch {
      case e: ArithmeticException =>
        if (settings.warnConstant)
          runReporting.warning(tree.pos, s"Evaluation of a constant expression results in an arithmetic error: ${e.getMessage}", WarningCategory.LintConstant, site)
        tree
    }

  /** If tree is a constant value that can be converted to type `pt`, perform the conversion.
   */
  def apply(tree: Tree, pt: Type, site: Symbol): Tree = {
    val orig = apply(tree, site)
    orig.tpe match {
      case tp@ConstantType(x) => fold(orig, x.convertTo(pt), foldable = isConstantType(tp))
      case _ => orig
    }
  }

  /** Set the computed constant type.
   */
  private def fold(orig: Tree, folded: Constant, foldable: Boolean): Tree =
    if (folded == null || folded.tag == UnitTag) orig
    else orig.setType {
      if (foldable) FoldableConstantType(folded)
      else LiteralType(folded)
    }

  private def foldUnop(op: Name, x: Constant): Constant = {
    val N = nme
    import N._
    val value: Any = op match {
      case UNARY_! => if (x.tag == BooleanTag) !x.booleanValue else null
      case UNARY_~ => x.tag match {
        case IntTag  => ~x.intValue
        case LongTag => ~x.longValue
        case _ => null
      }
      case UNARY_+ => x.tag match {
        case IntTag    => +x.intValue
        case LongTag   => +x.longValue
        case FloatTag  => +x.floatValue
        case DoubleTag => +x.doubleValue
        case _ => null
      }
      case UNARY_- => x.tag match {
        case IntTag    => -x.intValue
        case LongTag   => -x.longValue
        case FloatTag  => -x.floatValue
        case DoubleTag => -x.doubleValue
        case _ => null
      }
      case _ if x.isNumeric => op match {
        case `toChar`   => x.charValue
        case `toInt`    => x.intValue
        case `toLong`   => x.longValue
        case `toFloat`  => x.floatValue
        case `toDouble` => x.doubleValue
        case _ => null
      }
      case _ => null
    }
    if (value != null) Constant(value) else null
  }

  /** These are local helpers to keep foldBinop from overly taxing the optimizer.
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
    case nme.ADD => Constant(safely(Math.addExact(x.intValue, y.intValue), x.intValue + y.intValue))
    case nme.SUB => Constant(safely(Math.subtractExact(x.intValue, y.intValue), x.intValue - y.intValue))
    case nme.MUL => Constant(safely(Math.multiplyExact(x.intValue, y.intValue), x.intValue * y.intValue))
    case nme.DIV =>
      val xd = x.intValue
      val yd = y.intValue
      val value =
        if (yd == 0) xd / yd // Math.divideExact(xd, yd) // de-optimize
        else if (yd == -1 && xd == Int.MinValue)
          safely(throw new ArithmeticException("integer overflow"), xd / yd)
        else xd / yd
      Constant(value)
    case nme.MOD => Constant(x.intValue % y.intValue)
    case _       => null
  }
  private def foldLongOp(op: Name, x: Constant, y: Constant): Constant = op match {
    case nme.OR  => Constant(x.longValue | y.longValue)
    case nme.XOR => Constant(x.longValue ^ y.longValue)
    case nme.AND => Constant(x.longValue & y.longValue)
    case nme.LSL if x.tag <= IntTag
                 => Constant(x.intValue << y.longValue.toInt)
    case nme.LSL => Constant(x.longValue <<  y.longValue)
    case nme.LSR if x.tag <= IntTag
                 => Constant(x.intValue >>> y.longValue.toInt)
    case nme.LSR => Constant(x.longValue >>> y.longValue)
    case nme.ASR if x.tag <= IntTag
                 => Constant(x.intValue >> y.longValue.toInt)
    case nme.ASR => Constant(x.longValue >> y.longValue)
    case nme.EQ  => Constant(x.longValue == y.longValue)
    case nme.NE  => Constant(x.longValue != y.longValue)
    case nme.LT  => Constant(x.longValue < y.longValue)
    case nme.GT  => Constant(x.longValue > y.longValue)
    case nme.LE  => Constant(x.longValue <= y.longValue)
    case nme.GE  => Constant(x.longValue >= y.longValue)
    case nme.ADD => Constant(safely(Math.addExact(x.longValue, y.longValue), x.longValue + y.longValue))
    case nme.SUB => Constant(safely(Math.subtractExact(x.longValue, y.longValue), x.longValue - y.longValue))
    case nme.MUL => Constant(safely(Math.multiplyExact(x.longValue, y.longValue), x.longValue * y.longValue))
    case nme.DIV =>
      val xd = x.longValue
      val yd = y.longValue
      val value =
        if (yd == 0) xd / yd // Math.divideExact(xd, yd) // de-optimize
        else if (yd == -1 && xd == Long.MinValue)
          safely(throw new ArithmeticException("long overflow"), xd / yd)
        else xd / yd
      Constant(value)
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
  private def safelyFoldBinop(tree: Tree, site: Symbol)(op: Name, x: Constant, y: Constant): Constant =
    try foldBinop(op, x, y)
    catch {
      case e: ConstFoldException =>
        if (settings.warnConstant)
          runReporting.warning(tree.pos, s"Evaluation of a constant expression results in an arithmetic error: ${e.getMessage}, using ${e.value}", WarningCategory.LintConstant, site)
        Constant(e.value)
    }
  private def safely[A](exact: => A, inexact: A): A =
    try exact
    catch { case e: ArithmeticException => throw new ConstFoldException(e.getMessage, inexact) }
  private class ConstFoldException(msg: String, val value: Any) extends ControlThrowable(msg)
}
