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
    public ACode Apply(Tree t, AFunction function, Type[] targs,ACode[] vargs){
        ACode.Apply code = ACode.Apply(function, targs, vargs);
        code.pos = t.pos;
        return code;
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
    public ACode Switch(Tree t, ACode test, int[][] tags, ACode[] bodies,
        ACode other)
    {
        ACode.Switch code = ACode.Switch(test, tags, bodies, other);
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
