/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.ast.Tree;
import scalac.symtab.Symbol;
import scalac.symtab.Type;

/** This class implements an attributed tree factory. */
public class ATreeFactory {

    //########################################################################
    // Public Fields

    /** The unique Void node */
    public final ACode Void = ACode.Void;

    /** The unique UNIT constant */
    public final AConstant UNIT = AConstant.UNIT;

    /** The unique NULL constant */
    public final AConstant NULL = AConstant.NULL;

    /** The unique ZERO constant */
    public final AConstant ZERO = AConstant.ZERO;

    //########################################################################
    // Public Methods - Building code

    /** Builds a This node. */
    public ACode This(Tree t, Symbol clasz) {
        ACode.This code = ACode.This(clasz);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Constant node. */
    public ACode Constant(Tree t, AConstant constant) {
        ACode.Constant code = ACode.Constant(constant);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Load node. */
    public ACode Load(Tree t, ALocation location) {
        ACode.Load code = ACode.Load(location);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Store node. */
    public ACode Store(Tree t, ALocation location, ACode value) {
        ACode.Store code = ACode.Store(location, value);
        code.pos = t.pos;
        return code;
    }

    /** Builds an Apply node. */
    public ACode Apply(Tree t, AFunction function, Type[] ts, ACode[] vs) {
        ACode.Apply code = ACode.Apply(function, ts, vs);
        code.pos = t.pos;
        return code;
    }

    /** Builds an Apply node. */
    public ACode Apply(Tree t, AFunction function, Type[] ts) {
        return Apply(t, function, ts, ACode.EMPTY_ARRAY);
    }

    /** Builds an Apply node. */
    public ACode Apply(Tree t, AFunction function, ACode[] vs) {
        return Apply(t, function, Type.EMPTY_ARRAY, vs);
    }

    /** Builds an Apply node. */
    public ACode Apply(Tree t, AFunction function) {
        return Apply(t, function, Type.EMPTY_ARRAY, ACode.EMPTY_ARRAY);
    }

    /** Builds an Apply node. */
    public ACode Apply(Tree t, APrimitive primitive, ACode v0) {
        return Apply(t, AFunction.Primitive(primitive), new ACode[] {v0});
    }

    /** Builds an Apply node. */
    public ACode Apply(Tree t, APrimitive primitive, ACode v0, ACode v1) {
        return Apply(t, AFunction.Primitive(primitive), new ACode[] {v0,v1});
    }

    /** Builds an IsAs node. */
    public ACode IsAs(Tree t, ACode value, Type type, boolean cast) {
        ACode.IsAs code = ACode.IsAs(value, type, cast);
        code.pos = t.pos;
        return code;
    }

    /** Builds an If node. */
    public ACode If(Tree t, ACode test, ACode success, ACode failure) {
        ACode.If code = ACode.If(test, success, failure);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Switch node. */
    public ACode Switch(Tree t, ACode test, int[][] tags, ACode[] bodies) {
        ACode.Switch code = ACode.Switch(test, tags, bodies);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Synchronized node. */
    public ACode Synchronized(Tree t, ACode lock, ACode value) {
        ACode.Synchronized code = ACode.Synchronized(lock, value);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Block node. */
    public ACode Block(Tree t, Symbol[] locals,ACode[] statements,ACode value){
        ACode.Block code = ACode.Block(locals, statements, value);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Label node. */
    public ACode Label(Tree t, Symbol label, Symbol[] locals, ACode value) {
        ACode.Label code = ACode.Label(label, locals, value);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Goto node. */
    public ACode Goto(Tree t, Symbol label, ACode[] vargs) {
        ACode.Goto code = ACode.Goto(label, vargs);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Return node. */
    public ACode Return(Tree t, Symbol function, ACode value) {
        ACode.Return code = ACode.Return(function, value);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Throw node. */
    public ACode Throw(Tree t, ACode value) {
        ACode.Throw code = ACode.Throw(value);
        code.pos = t.pos;
        return code;
    }

    /** Builds a Drop node. */
    public ACode Drop(Tree t, ACode value, Type type) {
        ACode.Drop code = ACode.Drop(value, type);
        code.pos = t.pos;
        return code;
    }

    //########################################################################
    // Public Methods - Building primitive operations

    /** Builds a negation operation */
    public ACode NOT(Tree t, ATypeKind kind, ACode v0) {
        return Apply(t, NOT(kind), v0);
    }

    /** Builds an EQ operation */
    public ACode EQ(Tree t, ATypeKind kind, ACode v0) {
        return Apply(t, EQ(kind, true), v0);
    }

    /** Builds a NE  operation */
    public ACode NE(Tree t, ATypeKind kind, ACode v0) {
        return Apply(t, NE(kind, true), v0);
    }

    /** Builds a LT operation */
    public ACode LT(Tree t, ATypeKind kind, ACode v0) {
        return Apply(t, LT(kind, true), v0);
    }

    /** Builds a GE operation */
    public ACode GE(Tree t, ATypeKind kind, ACode v0) {
        return Apply(t, GE(kind, true), v0);
    }

    /** Builds a LE operation */
    public ACode LE(Tree t, ATypeKind kind, ACode v0) {
        return Apply(t, LE(kind, true), v0);
    }

    /** Builds a GT operation */
    public ACode GT(Tree t, ATypeKind kind, ACode v0) {
        return Apply(t, GT(kind, true), v0);
    }

    /** Builds an EQ operation */
    public ACode EQ(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, EQ(kind, false), v0, v1);
    }

    /** Builds a NE  operation */
    public ACode NE(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, NE(kind, false), v0, v1);
    }

    /** Builds a LT operation */
    public ACode LT(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, LT(kind, false), v0, v1);
    }

    /** Builds a GE operation */
    public ACode GE(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, GE(kind, false), v0, v1);
    }

    /** Builds a LE operation */
    public ACode LE(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, LE(kind, false), v0, v1);
    }

    /** Builds a GT operation */
    public ACode GT(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, GT(kind, false), v0, v1);
    }

    /** Builds a CMPL operation */
    public ACode CMPL(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, CMPL(kind), v0, v1);
    }

    /** Builds a CMP operation */
    public ACode CMP(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, CMP(kind), v0, v1);
    }

    /** Builds a CMPG operation */
    public ACode CMPG(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, CMPG(kind), v0, v1);
    }

    /** Builds an ADD operation */
    public ACode ADD(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, ADD(kind), v0, v1);
    }

    /** Builds a SUB operation */
    public ACode SUB(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, SUB(kind), v0, v1);
    }

    /** Builds a MUL operation */
    public ACode MUL(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, MUL(kind), v0, v1);
    }

    /** Builds a DIV operation */
    public ACode DIV(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, DIV(kind), v0, v1);
    }

    /** Builds a REM operation */
    public ACode REM(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, REM(kind), v0, v1);
    }

    /** Builds an AND operation. */
    public ACode AND(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, AND(kind), v0, v1);
    }

    /** Builds an OR operation. */
    public ACode OR(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, OR(kind), v0, v1);
    }

    /** Builds an XOR operation. */
    public ACode XOR(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, XOR(kind), v0, v1);
    }

    /** Builds a LSL operation. */
    public ACode LSL(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, LSL(kind), v0, v1);
    }

    /** Builds a LSR operation. */
    public ACode LSR(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, LSR(kind), v0, v1);
    }

    /** Builds an ASR operation. */
    public ACode ASR(Tree t, ATypeKind kind, ACode v0, ACode v1) {
        return Apply(t, ASR(kind), v0, v1);
    }

    /** Builds a value conversion operation. */
    public ACode CONVERT(Tree t, ATypeKind src, ATypeKind dst, ACode v0) {
        return Apply(t, CONVERT(src, dst), v0);
    }

    /** Builds an array length operation. */
    public ACode LENGTH(Tree t, ATypeKind kind, ACode v0) {
        return Apply(t, LENGTH(kind), v0);
    }

    /** Builds a string concatenation operation. */
    public ACode CONCAT(Tree t, ATypeKind lf, ATypeKind rg, ACode v0,ACode v1){
        return Apply(t, CONCAT(lf, rg), v0, v1);
    }

    //########################################################################
    // Public Methods - Building primitives

    /** Builds a negation primitive */
    public APrimitive NOT(ATypeKind kind) {
        return APrimitive.Negation(kind);
    }

    /** Builds an EQ primitive */
    public APrimitive EQ(ATypeKind kind, boolean zero) {
        return APrimitive.Test(ATestOp.EQ, kind, zero);
    }

    /** Builds a NE  primitive */
    public APrimitive NE(ATypeKind kind, boolean zero) {
        return APrimitive.Test(ATestOp.NE, kind, zero);
    }

    /** Builds a LT primitive */
    public APrimitive LT(ATypeKind kind, boolean zero) {
        return APrimitive.Test(ATestOp.LT, kind, zero);
    }

    /** Builds a GE primitive */
    public APrimitive GE(ATypeKind kind, boolean zero) {
        return APrimitive.Test(ATestOp.GE, kind, zero);
    }

    /** Builds a LE primitive */
    public APrimitive LE(ATypeKind kind, boolean zero) {
        return APrimitive.Test(ATestOp.LE, kind, zero);
    }

    /** Builds a GT primitive */
    public APrimitive GT(ATypeKind kind, boolean zero) {
        return APrimitive.Test(ATestOp.GT, kind, zero);
    }

    /** Builds a CMPL primitive */
    public APrimitive CMPL(ATypeKind kind) {
        return APrimitive.Comparison(AComparisonOp.CMPL, kind);
    }

    /** Builds a CMP primitive */
    public APrimitive CMP(ATypeKind kind) {
        return APrimitive.Comparison(AComparisonOp.CMP, kind);
    }

    /** Builds a CMPG primitive */
    public APrimitive CMPG(ATypeKind kind) {
        return APrimitive.Comparison(AComparisonOp.CMPG, kind);
    }

    /** Builds an ADD primitive */
    public APrimitive ADD(ATypeKind kind) {
        return APrimitive.Arithmetic(AArithmeticOp.ADD, kind);
    }

    /** Builds a SUB primitive */
    public APrimitive SUB(ATypeKind kind) {
        return APrimitive.Arithmetic(AArithmeticOp.SUB, kind);
    }

    /** Builds a MUL primitive */
    public APrimitive MUL(ATypeKind kind) {
        return APrimitive.Arithmetic(AArithmeticOp.MUL, kind);
    }

    /** Builds a DIV primitive */
    public APrimitive DIV(ATypeKind kind) {
        return APrimitive.Arithmetic(AArithmeticOp.DIV, kind);
    }

    /** Builds a REM primitive */
    public APrimitive REM(ATypeKind kind) {
        return APrimitive.Arithmetic(AArithmeticOp.REM, kind);
    }

    /** Builds an AND primitive. */
    public APrimitive AND(ATypeKind kind) {
        return APrimitive.Logical(ALogicalOp.AND, kind);
    }

    /** Builds an OR primitive. */
    public APrimitive OR(ATypeKind kind) {
        return APrimitive.Logical(ALogicalOp.OR, kind);
    }

    /** Builds an XOR primitive. */
    public APrimitive XOR(ATypeKind kind) {
        return APrimitive.Logical(ALogicalOp.XOR, kind);
    }

    /** Builds a LSL primitive. */
    public APrimitive LSL(ATypeKind kind) {
        return APrimitive.Shift(AShiftOp.ASL, kind);
    }

    /** Builds a LSR primitive. */
    public APrimitive LSR(ATypeKind kind) {
        return APrimitive.Shift(AShiftOp.LSR, kind);
    }

    /** Builds an ASR primitive. */
    public APrimitive ASR(ATypeKind kind) {
        return APrimitive.Shift(AShiftOp.ASR, kind);
    }

    /** Builds a value conversion primitive. */
    public APrimitive CONVERT(ATypeKind src, ATypeKind dst) {
        return APrimitive.Conversion(src, dst);
    }

    /** Builds an array length primitive. */
    public APrimitive LENGTH(ATypeKind kind) {
        return APrimitive.ArrayLength(kind);
    }

    /** Builds a string concatenation primitive. */
    public APrimitive CONCAT(ATypeKind lf, ATypeKind rg) {
        return APrimitive.StringConcat(lf, rg);
    }

    //########################################################################
    // Public Methods - Building constants

    /** Builds a BOOLEAN constant of given value. */
    public AConstant BOOLEAN(Boolean value) {
        return BOOLEAN(value.booleanValue());
    }

    /** Builds a BOOLEAN constant of given value. */
    public AConstant BOOLEAN(boolean value) {
        return AConstant.BOOLEAN(value);
    }

    /** Builds a BYTE constant of given value. */
    public AConstant BYTE(Byte value) {
        return BYTE(value.byteValue());
    }

    /** Builds a BYTE constant of given value. */
    public AConstant BYTE(byte value) {
        return AConstant.BYTE(value);
    }

    /** Builds a SHORT constant of given value. */
    public AConstant SHORT(Short value) {
        return SHORT(value.shortValue());
    }

    /** Builds a SHORT constant of given value. */
    public AConstant SHORT(short value) {
        return AConstant.SHORT(value);
    }

    /** Builds a CHAR constant of given value. */
    public AConstant CHAR(Character value) {
        return CHAR(value.charValue());
    }

    /** Builds a CHAR constant of given value. */
    public AConstant CHAR(char value) {
        return AConstant.CHAR(value);
    }

    /** Builds an INT constant of given value. */
    public AConstant INT(Integer value) {
        return INT(value.intValue());
    }

    /** Builds an INT constant of given value. */
    public AConstant INT(int value) {
        return AConstant.INT(value);
    }

    /** Builds a LONG constant of given value. */
    public AConstant LONG(Long value) {
        return LONG(value.longValue());
    }

    /** Builds a LONG constant of given value. */
    public AConstant LONG(long value) {
        return AConstant.LONG(value);
    }

    /** Builds a FLOAT constant of given value. */
    public AConstant FLOAT(Float value) {
        return FLOAT(value.floatValue());
    }

    /** Builds a FLOAT constant of given value. */
    public AConstant FLOAT(float value) {
        return AConstant.FLOAT(value);
    }

    /** Builds a DOUBLE constant of given value. */
    public AConstant DOUBLE(Double value) {
        return DOUBLE(value.doubleValue());
    }

    /** Builds a DOUBLE constant of given value. */
    public AConstant DOUBLE(double value) {
        return AConstant.DOUBLE(value);
    }

    /** Builds a STRING constant of given value. */
    public AConstant STRING(String value) {
        return AConstant.STRING(value);
    }

    //########################################################################
}
