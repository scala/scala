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

package scalac.typechecker;

import scalac.util.*;
import scalac.symtab.*;
import scalac.symtab.Type.*;

class ConstantFolder implements /*imports*/ TypeTags {

    private final Analyzer ana;

    ConstantFolder(Analyzer ana) {
	this.ana = ana;
    }

    /** fold binary operation.
     */
    Type foldBinary(int pos, ConstantType left, ConstantType right,
			   Name op, Type restype) {
	try {
	    Object value = null;
	    switch (restype.unbox()) {
	    case UnboxedType(INT):
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
		break;
	    case UnboxedType(LONG):
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
		break;
	    case UnboxedType(FLOAT):
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
		break;
	    case UnboxedType(DOUBLE):
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
		break;
	    case UnboxedType(BOOLEAN):
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
		break;
	    default:
		if (restype.symbol() == ana.definitions.JAVA_STRING_CLASS &&
		    op == Names.ADD)
		    value = left.stringValue() + right.stringValue();
	    }
	    return (value != null) ? new ConstantType(restype, value)
		: Type.NoType;
        } catch (ArithmeticException e) {
	    ana.unit.error(pos, e.toString());
	    return Type.NoType;
	}
    }

    /** fold unary operation.
     */
    Type foldUnary(int pos, ConstantType od, Name op, Type restype) {
	try {
	    Object value = null;
	    switch (restype.unbox()) {
	    case UnboxedType(INT):
		if (op == Names.ADD)
		    value = new Integer(od.intValue());
		else if (op == Names.SUB)
		    value = new Integer(-od.intValue());
		else if (op == Names.NOT)
		    value = new Integer(~od.intValue());
		break;
	    case UnboxedType(LONG):
		if (op == Names.ADD)
		    value = new Long(od.longValue());
		else if (op == Names.SUB)
		    value = new Long(-od.longValue());
		else if (op == Names.NOT)
		    value = new Long(~od.longValue());
		break;
	    case UnboxedType(FLOAT):
		if (op == Names.ADD)
		    value = new Float(od.floatValue());
		else if (op == Names.SUB)
		    value = new Float(-od.floatValue());
		break;
	    case UnboxedType(DOUBLE):
		if (op == Names.ADD)
		    value = new Double(od.doubleValue());
		else if (op == Names.SUB)
		    value = new Double(-od.doubleValue());
		break;
	    case UnboxedType(BOOLEAN):
		if (op == Names.ZNOT)
		    value = new Boolean(!od.booleanValue());
		break;
	    }
	    return (value != null) ? new ConstantType(restype, value)
		: Type.NoType;
        } catch (ArithmeticException e) {
	    ana.unit.error(pos, e.toString());
	    return Type.NoType;
	}
    }

    /** fold cast operation
     */
    Type foldAsInstanceOf(int pos, ConstantType od, Type restype) {
	try {
	    Object value = null;
	    switch (restype.unbox()) {
	    case UnboxedType(BYTE):
		value = new Byte((byte)od.intValue());
		break;
	    case UnboxedType(CHAR):
		value = new Character((char)od.intValue());
		break;
	    case UnboxedType(SHORT):
		value = new Short((short)od.intValue());
		break;
	    case UnboxedType(INT):
		value = new Integer(od.intValue());
		break;
	    case UnboxedType(LONG):
		value = new Long(od.longValue());
		break;
	    case UnboxedType(FLOAT):
		value = new Float(od.longValue());
		break;
	    case UnboxedType(DOUBLE):
		value = new Double(od.doubleValue());
		break;
	    case UnboxedType(BOOLEAN):
		value = new Boolean(od.booleanValue());
		break;
	    }
	    return (value != null) ? new ConstantType(restype, value)
		: Type.NoType;
        } catch (ClassCastException e) {
	    ana.unit.error(pos, e.toString());
	    return Type.NoType;
	}
    }
}






