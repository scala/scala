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
import scalac.ast.*;
import scalac.symtab.*;
import scalac.symtab.Type.*;

class ConstantFolder implements /*imports*/ TypeTags {

    private final Analyzer ana;

    ConstantFolder(Analyzer ana) {
	this.ana = ana;
    }

    /** fold binary operation.
     */
    Type foldBinary(int pos, ConstantType left, ConstantType right, Name op) {
	try {
            Type ltype = left.deconst();
            Symbol lsymbol = left.symbol();
	    if (lsymbol == ana.definitions.BYTE_CLASS ||
		lsymbol == ana.definitions.CHAR_CLASS ||
		lsymbol == ana.definitions.SHORT_CLASS) {
		ltype = ana.definitions.INT_TYPE();
                lsymbol = ana.definitions.INT_CLASS;
            }
            Type rtype = right.deconst();
            Symbol rsymbol = right.symbol();
	    if (rsymbol == ana.definitions.BYTE_CLASS ||
		rsymbol == ana.definitions.CHAR_CLASS ||
		rsymbol == ana.definitions.SHORT_CLASS) {
		rtype = ana.definitions.INT_TYPE();
                rsymbol = ana.definitions.INT_CLASS;
            }
	    Type optype;
            if (ltype.isSameAs(rtype))
                optype = ltype;
            else if (lsymbol == ana.definitions.JAVA_STRING_CLASS)
                optype = ltype;
            else if (rsymbol == ana.definitions.JAVA_STRING_CLASS)
                optype = rtype;
            else if (lsymbol == ana.definitions.INT_CLASS)
                optype = rtype;
            else if (rsymbol == ana.definitions.INT_CLASS)
                optype = ltype;
            else if (lsymbol == ana.definitions.LONG_CLASS)
                optype = rtype;
            else if (rsymbol == ana.definitions.LONG_CLASS)
                optype = ltype;
            else if (lsymbol == ana.definitions.FLOAT_CLASS)
                optype = rtype;
            else if (rsymbol == ana.definitions.FLOAT_CLASS)
                optype = ltype;
            else
                throw Debug.abort("illegal case", ltype + " - " + rtype);
	    Object value = null;
	    switch (optype.unbox()) {
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
		if (optype.symbol() == ana.definitions.JAVA_STRING_CLASS &&
		    op == Names.ADD)
		    value = left.stringValue() + right.stringValue();
	    }
	    return (value != null) ? Type.constantType(value) : Type.NoType;
        } catch (ArithmeticException e) {
	    ana.unit.warning(pos, e.toString());
	    return Type.NoType;
	}
    }

    /** fold unary operation.
     */
    Type foldUnary(int pos, ConstantType od, Name op) {
	try {
	    Object value = null;
	    switch (od.deconst().unbox()) {
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
	    return (value != null) ? Type.constantType(value) : Type.NoType;
        } catch (ArithmeticException e) {
	    ana.unit.warning(pos, e.toString());
	    return Type.NoType;
	}
    }

    /** fold cast operation
     */
    Type foldAsInstanceOf(int pos, ConstantType od, Type argtype) {
	try {
	    Object value = null;
	    switch (argtype.unbox()) {
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
	    return (value != null) ? new ConstantType(argtype, value)
		: Type.NoType;
        } catch (ClassCastException e) {
	    ana.unit.warning(pos, e.toString());
	    return Type.NoType;
	}
    }

    /** attempt to constant fold tree.
     */
    Tree tryToFold(Tree tree) {
	Type ctp = Type.NoType;
	switch (tree) {
	case Apply(Select(Tree qual, Name op), Tree[] args):
	    if (qual.type instanceof ConstantType) {
		if (args.length == 0)
		    ctp = foldUnary(
			tree.pos, (ConstantType)qual.type, op);
		else if (args.length == 1 &&
			 args[0].type instanceof ConstantType)
		    ctp = foldBinary(
			tree.pos,
			(ConstantType)qual.type,
			(ConstantType)(args[0].type),
			op);
	    }
	    break;
	case TypeApply(Select(Tree qual, Name op), Tree[] targs):
		if (qual.type instanceof Type.ConstantType &&
		    op == Names.asInstanceOf)
		    ctp = foldAsInstanceOf(
			tree.pos,
			(ConstantType)qual.type,
			targs[0].type);
		break;
	}
	switch (ctp) {
	case ConstantType(Type base, Object value):
	    return ana.make.Literal(tree.pos, value).setType(ctp);
	default:
	    return tree;
	}
    }

}






