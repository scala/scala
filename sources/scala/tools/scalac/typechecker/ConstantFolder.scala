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

import java.lang.Object;

import scalac.{Global => scalac_Global}
import scalac.util.Name;
import scalac.util.Names;
import scalac.ast._;
import scalac.ast.TreeGen;
import scalac.atree._;
import scalac.atree.ATypeKind;
import scalac.atree.ATypeKind._;
import scalac.symtab._;

package scala.tools.scalac.typechecker {

class ConstantFolder(global: scalac_Global) {

    //########################################################################
    // Private Fields

    /** The global definitions */
    private val definitions: Definitions = global.definitions;

    /** The tree generator */
    private val gen: TreeGen = global.treeGen;

    //########################################################################
    // Public Methods

    /**
     * Attempts to constant fold given tree. Returns the input tree if
     * constant folding fails.
     */
    def tryToFold(tree: Tree): Tree = {
        val value: AConstant = evaluate(tree);
        if (value != null) gen.Literal(tree.pos, value) else tree;
    }

    /**
     * Evaluates the expression represented by the tree and returns
     * the resulting value. Returns null if the evaluation can't be
     * performed.
     */
    def evaluate(tree: Tree): AConstant = tree match {
      case Tree$TypeApply(Tree$Select(qualifier, Names.asInstanceOf), args) if args.length == 1 =>
	qualifier.getType() match {
          case Type$ConstantType(_, lvalue) => cast(lvalue, args(0).getType());
	  case _ => null;
        }

      case Tree$Apply(Tree$Select(qualifier, op), args) if args.length == 1=>
        qualifier.getType() match {
          case Type$ConstantType(_, lvalue) =>
	    args(0).getType() match {
              case Type$ConstantType(_, rvalue) => evaluate(op, lvalue, rvalue)
	      case _ => null
            }
	  case _ => null
        }

      case Tree$Select(qualifier, op) =>
	qualifier.getType() match {
          case Type$ConstantType(_, lvalue) => evaluate(op, lvalue)
	  case _ => null
	}

      case _ => null
    }

    /**
     * Evaluates the unary operation and returns the resulting value.
     * Returns null if the evaluation can't be performed.
     */
    def evaluate(op: Name, value: AConstant): AConstant = kind(value) match {
      case ATypeKind.BOOL =>
	val v = value.booleanValue();
	if (op == Names.ZNOT) AConstant.BOOLEAN(!v)
	else null
      case ATypeKind.I4 =>
        val v = value.intValue();
	if (op == Names.ADD) AConstant.INT(+v)
	else if (op == Names.SUB) AConstant.INT(-v)
	else if (op == Names.NOT) AConstant.INT(~v)
	else null;
      case ATypeKind.I8 =>
        val v = value.longValue();
	if (op == Names.ADD) AConstant.LONG(+v)
        else if (op == Names.SUB) AConstant.LONG(-v)
        else if (op == Names.NOT) AConstant.LONG(~v)
        else null;
      case ATypeKind.R4 =>
        val v = value.floatValue();
	if (op == Names.ADD) AConstant.FLOAT(+v)
	else if (op == Names.SUB) AConstant.FLOAT(-v)
	else null
      case ATypeKind.R8 =>
        val v = value.doubleValue();
	if (op == Names.ADD) AConstant.DOUBLE(+v)
	else if (op == Names.SUB) AConstant.DOUBLE(-v)
	else null
      case _ =>
        null
    }

    /**
     * Evaluates the binary operation and returns the resulting value.
     * Returns null if the evaluation can't be performed.
     */
    def evaluate(op: Name, lvalue: AConstant, rvalue: AConstant): AConstant = {
      val k = kind(lvalue, rvalue);
      if (k == null) return null;
      k match {
        case ATypeKind.I4 =>
          val l = lvalue.intValue();
          val r = rvalue.intValue();
          if (op == Names.OR ) AConstant.INT(l | r)
          else if (op == Names.AND) AConstant.INT(l & r)
          else if (op == Names.XOR) AConstant.INT(l ^ r)
          else if (op == Names.ADD) AConstant.INT(l + r)
          else if (op == Names.SUB) AConstant.INT(l - r)
          else if (op == Names.MUL) AConstant.INT(l * r)
          else if (op == Names.DIV) if (r == 0) null else AConstant.INT(l / r)
          else if (op == Names.MOD) if (r == 0) null else AConstant.INT(l % r)
          else if (op == Names.EQ ) AConstant.BOOLEAN(l == r)
          else if (op == Names.NE ) AConstant.BOOLEAN(l != r)
          else if (op == Names.LT ) AConstant.BOOLEAN(l <  r)
          else if (op == Names.GT ) AConstant.BOOLEAN(l >  r)
          else if (op == Names.LE ) AConstant.BOOLEAN(l <= r)
          else if (op == Names.GE ) AConstant.BOOLEAN(l >= r)
          else if (op == Names.LSL) AConstant.INT(l <<  r)
          else if (op == Names.LSR) AConstant.INT(l >>> r)
          else if (op == Names.ASR) AConstant.INT(l >>  r)
          else null

        case ATypeKind.I8 =>
            val l = lvalue.longValue();
            val r = rvalue.longValue();
            if (op == Names.OR ) AConstant.LONG(l | r)
            else if (op == Names.AND) AConstant.LONG(l & r)
            else if (op == Names.XOR) AConstant.LONG(l ^ r)
            else if (op == Names.ADD) AConstant.LONG(l + r)
            else if (op == Names.SUB) AConstant.LONG(l - r)
            else if (op == Names.MUL) AConstant.LONG(l * r)
            else if (op == Names.DIV) if (r == 0) null else AConstant.LONG(l / r)
            else if (op == Names.MOD) if (r == 0) null else AConstant.LONG(l % r)
            else if (op == Names.EQ ) AConstant.BOOLEAN(l == r)
            else if (op == Names.NE ) AConstant.BOOLEAN(l != r)
            else if (op == Names.LT ) AConstant.BOOLEAN(l < r)
            else if (op == Names.GT ) AConstant.BOOLEAN(l > r)
            else if (op == Names.LE ) AConstant.BOOLEAN(l <= r)
            else if (op == Names.GE ) AConstant.BOOLEAN(l >= r)
            else if (kind(lvalue) == ATypeKind.I4) {
                val li = lvalue.intValue();
                if (op == Names.LSL) AConstant.INT(li <<  r)
                else if (op == Names.LSR) AConstant.INT(li >>> r)
                else if (op == Names.ASR) AConstant.INT(li >>  r)
		else null
            } else {
                if (op == Names.LSL) AConstant.LONG(l <<  r)
                else if (op == Names.LSR) AConstant.LONG(l >>> r)
                else if (op == Names.ASR) AConstant.LONG(l >>  r)
		else null
            }

        case ATypeKind.R4 =>
            val l = lvalue.floatValue();
            val r = rvalue.floatValue();
            if (op == Names.ADD) AConstant.FLOAT(l + r)
            else if (op == Names.SUB) AConstant.FLOAT(l - r)
            else if (op == Names.MUL) AConstant.FLOAT(l * r)
            else if (op == Names.DIV) AConstant.FLOAT(l / r)
            else if (op == Names.MOD) AConstant.FLOAT(l % r)
            else if (op == Names.EQ ) AConstant.BOOLEAN(l == r)
            else if (op == Names.NE ) AConstant.BOOLEAN(l != r)
            else if (op == Names.LT ) AConstant.BOOLEAN(l < r)
            else if (op == Names.GT ) AConstant.BOOLEAN(l > r)
            else if (op == Names.LE ) AConstant.BOOLEAN(l <= r)
            else if (op == Names.GE ) AConstant.BOOLEAN(l >= r)
            else null

        case ATypeKind.R8 =>
            val l = lvalue.doubleValue();
            val r = rvalue.doubleValue();
            if (op == Names.ADD) AConstant.DOUBLE(l + r)
            else if (op == Names.SUB) AConstant.DOUBLE(l - r)
            else if (op == Names.MUL) AConstant.DOUBLE(l * r)
            else if (op == Names.DIV) AConstant.DOUBLE(l / r)
            else if (op == Names.MOD) AConstant.DOUBLE(l % r)
            else if (op == Names.EQ ) AConstant.BOOLEAN(l == r)
            else if (op == Names.NE ) AConstant.BOOLEAN(l != r)
            else if (op == Names.LT ) AConstant.BOOLEAN(l <  r)
            else if (op == Names.GT ) AConstant.BOOLEAN(l >  r)
            else if (op == Names.LE ) AConstant.BOOLEAN(l <= r)
            else if (op == Names.GE ) AConstant.BOOLEAN(l >= r)
            else null

        case ATypeKind.BOOL =>
            val l = lvalue.booleanValue();
            val r = rvalue.booleanValue();
            if (op == Names.ZOR ) AConstant.BOOLEAN(l |  r)
            else if (op == Names.OR  ) AConstant.BOOLEAN(l |  r)
            else if (op == Names.ZAND) AConstant.BOOLEAN(l &  r)
            else if (op == Names.AND ) AConstant.BOOLEAN(l &  r)
            else if (op == Names.XOR ) AConstant.BOOLEAN(l ^  r)
            else if (op == Names.EQ  ) AConstant.BOOLEAN(l == r)
            else if (op == Names.NE  ) AConstant.BOOLEAN(l != r)
            else null

        case ATypeKind.STR =>
            if (op == Names.ADD) AConstant.STRING(lvalue.stringValue()+rvalue.stringValue())
            else null

        case _ =>
            null
        }
    }

    /**
     * Casts the value to given type and returns the resulting value.
     * Returns null if the cast can't be performed.
     */
    def cast(value: AConstant, tpe: Type): AConstant = value.kind() match {
      case UNIT =>
        if (tpe.isSameAs(definitions.UNIT_CLASS.getType())) value
	else null
      case BOOL =>
        if (tpe.isSameAs(definitions.BOOLEAN_TYPE())) value
	else null
      case U1 | U2 | U4 | U8 | I1 | I2 | I4 | I8 | R4 | R8 =>
        if (tpe.isSameAs(definitions.BYTE_TYPE()))
          AConstant.BYTE(value.byteValue())
        else if (tpe.isSameAs(definitions.SHORT_TYPE()))
          AConstant.SHORT(value.shortValue())
        else if (tpe.isSameAs(definitions.CHAR_TYPE()))
          AConstant.CHAR(value.charValue())
        else if (tpe.isSameAs(definitions.INT_TYPE()))
          AConstant.INT(value.intValue())
        else if (tpe.isSameAs(definitions.LONG_TYPE()))
          AConstant.LONG(value.longValue())
        else if (tpe.isSameAs(definitions.FLOAT_TYPE()))
          AConstant.FLOAT(value.floatValue())
        else if (tpe.isSameAs(definitions.DOUBLE_TYPE()))
          AConstant.DOUBLE(value.doubleValue())
	else null
      case STR =>
        if (tpe.isSameAs(definitions.STRING_TYPE())) value
	else null
      case _ =>
        null
    }

    //########################################################################
    // Private Methods

    /** Returns the kind of given value. */
    private def kind(value: AConstant): ATypeKind = {
        val k = value.kind();
        k match {
        case I1 | I2 | U2 =>
            ATypeKind.I4
        case _ =>
            k
        }
    }

    /**
     * Returns the combined kind of given values or null if the values
     * can't be combined.
     */
    private def kind(lvalue: AConstant, rvalue: AConstant): ATypeKind = {
        val lkind = kind(lvalue);
        val rkind = kind(rvalue);
        if (lkind == rkind) lkind
        else if (lkind == ATypeKind.ZERO) null
        else if (rkind == ATypeKind.ZERO) null
        else if (lkind == ATypeKind.STR) lkind
        else if (rkind == ATypeKind.STR) rkind
        else lkind match {
          case I4 =>
            rkind match {
              case I4 => lkind
              case I8 => rkind
              case R4 => rkind
              case R8 => rkind
              case _ => null
            }
          case I8 =>
            rkind match {
              case I4 => lkind
              case I8 => lkind
              case R4 => rkind
              case R8 => rkind
              case _ => null
            }
          case R4 =>
            rkind match {
              case I4 => lkind
              case I8 => lkind
              case R4 => lkind
              case R8 => rkind
              case _ => null
            }
          case R8 =>
            rkind match {
              case I4 => lkind
              case I8 => lkind
              case R4 => lkind
              case R8 => lkind
              case _ => null
            }
          case _ =>
            null
        }
    }

    //########################################################################
}
}
