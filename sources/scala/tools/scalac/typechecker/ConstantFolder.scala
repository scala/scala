/* ************************************************************************
 * Pizza constant folder
 * by Martin Odersky
 *
 * Copyright (C) 1996,97 Martin Odersky. All rights reserved.
 * Permission is hereby granted to modify and use this software for research
 * and teaching purposes. Modification for commercial purposes requires
 * prior written permission by the author.
 * The software, or modifications thereof, may be redistributed only
 * if this copyright notice stays attached.
 *************************************************************************/

import scalac.util._;
import scalac.ast._;
import scalac.symtab._;
import java.lang.{Byte,Character,Short,Integer,Long,Float,Double,Boolean,Object,Number}

package scala.tools.scalac.typechecker {

class ConstantFolder(ana: Analyzer) {

  import TypeTags._;

  /** fold binary operation.
  */
  def foldBinary(pos: int, left: Type$ConstantType, right: Type$ConstantType, op: Name): Type = {
    try {
      var optype: Type = left.deconst();
      if (optype.symbol() == ana.definitions.BYTE_CLASS ||
	  optype.symbol() == ana.definitions.CHAR_CLASS ||
	  optype.symbol() == ana.definitions.SHORT_CLASS)
	optype = ana.definitions.INT_TYPE();
      if (optype.isSubType(right.deconst()))
	optype = right.deconst();
      var value: Object = null;
      optype.unbox() match {
	case Type$UnboxedType(INT) =>
	  if (op == Names.ADD)
	    value = new Integer(left.intValue() + right.intValue());
	  else if (op == Names.SUB)
	    value = new Integer(left.intValue() - right.intValue());
	  else if (op == Names.MUL)
	    value = new Integer(left.intValue() * right.intValue());
	  else if (op == Names.DIV)
	    value = new Integer(left.intValue() / right.intValue());
	  else if (op == Names.MOD)
	    value = new Integer(left.intValue() % right.intValue());
	  else if (op == Names.EQ)
	    value = new Boolean(left.intValue() == right.intValue());
	  else if (op == Names.NE)
	    value = new Boolean(left.intValue() != right.intValue());
	  else if (op == Names.LT)
	    value = new Boolean(left.intValue() < right.intValue());
	  else if (op == Names.GT)
	    value = new Boolean(left.intValue() > right.intValue());
	  else if (op == Names.LE)
	    value = new Boolean(left.intValue() <= right.intValue());
	  else if (op == Names.GE)
	    value = new Boolean(left.intValue() >= right.intValue());
	  else if (op == Names.OR)
	    value = new Integer(left.intValue() | right.intValue());
	  else if (op == Names.AND)
	    value = new Integer(left.intValue() & right.intValue());
	  else if (op == Names.XOR)
	    value = new Integer(left.intValue() ^ right.intValue());
	  else if (op == Names.LSL)
	    value = new Integer(left.intValue() << right.intValue());
	  else if (op == Names.LSR)
	    value = new Integer(left.intValue() >>> right.intValue());
	  else if (op == Names.ASR)
	    value = new Integer(left.intValue() >> right.intValue());

	case Type$UnboxedType(LONG) =>
	  if (op == Names.ADD)
	    value = new Long(left.longValue() + right.longValue());
	  else if (op == Names.SUB)
	    value = new Long(left.longValue() - right.longValue());
	  else if (op == Names.MUL)
	    value = new Long(left.longValue() * right.longValue());
	  else if (op == Names.DIV)
	    value = new Long(left.longValue() / right.longValue());
	  else if (op == Names.MOD)
	    value = new Long(left.longValue() % right.longValue());
	  else if (op == Names.EQ)
	    value = new Boolean(left.longValue() == right.longValue());
	  else if (op == Names.NE)
	    value = new Boolean(left.longValue() != right.longValue());
	  else if (op == Names.LT)
	    value = new Boolean(left.longValue() < right.longValue());
	  else if (op == Names.GT)
	    value = new Boolean(left.longValue() > right.longValue());
	  else if (op == Names.LE)
	    value = new Boolean(left.longValue() <= right.longValue());
	  else if (op == Names.GE)
	    value = new Boolean(left.longValue() >= right.longValue());
	  else if (op == Names.OR)
	    value = new Long(left.longValue() | right.longValue());
	  else if (op == Names.AND)
	    value = new Long(left.longValue() & right.longValue());
	  else if (op == Names.XOR)
	    value = new Long(left.longValue() ^ right.longValue());
	  else if (op == Names.LSL)
	    value = new Long(left.longValue() << right.intValue());
	  else if (op == Names.LSR)
	    value = new Long(left.longValue() >>> right.intValue());
	  else if (op == Names.ASR)
	    value = new Long(left.longValue() >> right.intValue());

	case Type$UnboxedType(FLOAT) =>
	  if (op == Names.ADD)
	    value = new Float(left.floatValue() + right.floatValue());
	  else if (op == Names.SUB)
	    value = new Float(left.floatValue() - right.floatValue());
	  else if (op == Names.MUL)
	    value = new Float(left.floatValue() * right.floatValue());
	  else if (op == Names.DIV)
	    value = new Float(left.floatValue() / right.floatValue());
	  else if (op == Names.MOD)
	    value = new Float(left.floatValue() % right.floatValue());
	  else if (op == Names.EQ)
	    value = new Boolean(left.floatValue() == right.floatValue());
	  else if (op == Names.NE)
	    value = new Boolean(left.floatValue() != right.floatValue());
	  else if (op == Names.LT)
	    value = new Boolean(left.floatValue() < right.floatValue());
	  else if (op == Names.GT)
	    value = new Boolean(left.floatValue() > right.floatValue());
	  else if (op == Names.LE)
	    value = new Boolean(left.floatValue() <= right.floatValue());
	  else if (op == Names.GE)
	    value = new Boolean(left.floatValue() >= right.floatValue());

	case Type$UnboxedType(DOUBLE) =>
	  if (op == Names.ADD)
	    value = new Double(left.doubleValue() + right.doubleValue());
	  else if (op == Names.SUB)
	    value = new Double(left.doubleValue() - right.doubleValue());
	  else if (op == Names.MUL)
	    value = new Double(left.doubleValue() * right.doubleValue());
	  else if (op == Names.DIV)
	    value = new Double(left.doubleValue() / right.doubleValue());
	  else if (op == Names.MOD)
	    value = new Double(left.doubleValue() % right.doubleValue());
	  else if (op == Names.EQ)
	    value = new Boolean(left.doubleValue() == right.doubleValue());
	  else if (op == Names.NE)
	    value = new Boolean(left.doubleValue() != right.doubleValue());
	  else if (op == Names.LT)
	    value = new Boolean(left.doubleValue() < right.doubleValue());
	  else if (op == Names.GT)
	    value = new Boolean(left.doubleValue() > right.doubleValue());
	  else if (op == Names.LE)
	    value = new Boolean(left.doubleValue() <= right.doubleValue());
	  else if (op == Names.GE)
	    value = new Boolean(left.doubleValue() >= right.doubleValue());

	case Type$UnboxedType(BOOLEAN) =>
	  if (op == Names.EQ)
	    value = new Boolean(left.booleanValue() == right.booleanValue());
	  else if (op == Names.NE)
	    value = new Boolean(left.booleanValue() != right.booleanValue());
	  else if (op == Names.OR || op == Names.ZOR)
	    value = new Boolean(left.booleanValue() | right.booleanValue());
	  else if (op == Names.AND || op == Names.ZAND)
	    value = new Boolean(left.booleanValue() & right.booleanValue());
	  else if (op == Names.XOR)
	    value = new Boolean(left.booleanValue() ^ right.booleanValue());

	case _ =>
	  if (optype.symbol() == ana.definitions.JAVA_STRING_CLASS &&
	      op == Names.ADD)
	    value = left.stringValue() + right.stringValue();
      }
      if (value != null) Type.constantType(value) else Type.NoType;
    } catch {
      case e: ArithmeticException =>
	ana.unit.warning(pos, e.toString());
	Type.NoType
    }
  }

  /** fold unary operation.
  */
  def foldUnary(pos: int, od: Type$ConstantType, op: Name): Type = {
    try {
      var value: Object = null;
      od.deconst().unbox() match {
	case Type$UnboxedType(INT) =>
	  if (op == Names.ADD)
	    value = new Integer(od.intValue());
	  else if (op == Names.SUB)
	    value = new Integer(-od.intValue());
	  else if (op == Names.NOT)
	    value = new Integer(~od.intValue());

	case Type$UnboxedType(LONG) =>
	  if (op == Names.ADD)
	    value = new Long(od.longValue());
	  else if (op == Names.SUB)
	    value = new Long(-od.longValue());
	  else if (op == Names.NOT)
	    value = new Long(~od.longValue());

	case Type$UnboxedType(FLOAT) =>
	  if (op == Names.ADD)
	    value = new Float(od.floatValue());
	  else if (op == Names.SUB)
	    value = new Float(-od.floatValue());

	case Type$UnboxedType(DOUBLE) =>
	  if (op == Names.ADD)
	    value = new Double(od.doubleValue());
	  else if (op == Names.SUB)
	    value = new Double(-od.doubleValue());

	case Type$UnboxedType(BOOLEAN) =>
	  if (op == Names.ZNOT)
	    value = new Boolean(!od.booleanValue());

	case _ =>
      }
      if (value != null) Type.constantType(value) else Type.NoType;
    } catch {
      case e: ArithmeticException =>
	ana.unit.warning(pos, e.toString());
	Type.NoType
    }
  }

  /** fold cast operation
  */
  def foldAsInstanceOf(pos: int, od: Type$ConstantType, argtype: Type): Type = {
    try {
      var value: Object = null;
      argtype.unbox() match {
	case Type$UnboxedType(BYTE) =>
	  value = new Byte(od.intValue().asInstanceOf[byte]);

	case Type$UnboxedType(CHAR) =>
	  value = new Character(od.intValue().asInstanceOf[char]);

	case Type$UnboxedType(SHORT) =>
	  value = new Short(od.intValue().asInstanceOf[short]);

	case Type$UnboxedType(INT) =>
	  value = new Integer(od.intValue());

	case Type$UnboxedType(LONG) =>
	  value = new Long(od.longValue());

	case Type$UnboxedType(FLOAT) =>
	  value = new Float(od.longValue());

	case Type$UnboxedType(DOUBLE) =>
	  value = new Double(od.doubleValue());

	case Type$UnboxedType(BOOLEAN) =>
	  value = new Boolean(od.booleanValue());

	case _ =>
      }
      if (value != null) new Type$ConstantType(argtype, value) else Type.NoType;
    } catch {
      case e: ClassCastException =>
	ana.unit.warning(pos, e.toString());
	Type.NoType
    }
  }

  /** attempt to constant fold tree.
  */
  def tryToFold(tree: Tree): Tree = {
    var ctp: Type = Type.NoType;
    tree match {
      case Tree$Apply(Tree$Select(qual, op), args) =>
	if (qual.getType().isInstanceOf[Type$ConstantType]) {
	  if (args.length == 0)
	    ctp = foldUnary(
	      tree.pos, qual.getType().asInstanceOf[Type$ConstantType], op);
	  else if (args.length == 1 &&
		   args(0).getType().isInstanceOf[Type$ConstantType])
	    ctp = foldBinary(
	      tree.pos,
	      qual.getType().asInstanceOf[Type$ConstantType],
	      args(0).getType().asInstanceOf[Type$ConstantType],
	      op);
	}

      case Tree$TypeApply(Tree$Select(qual, op), targs) =>
	qual.getType() match {
	  case ct: Type$ConstantType if (op == Names.asInstanceOf) =>
	    ctp = foldAsInstanceOf(tree.pos, ct, targs(0).getType());
	  case _ =>
	}

      case _ =>
    }
    ctp match {
      case Type$ConstantType(base, value) =>
	ana.make.Literal(tree.pos, value).setType(ctp);
      case _ =>
	tree
    }
  }
}
}
