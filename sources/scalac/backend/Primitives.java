/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Primitives.java,v 1.12 2002/11/19 12:01:11 paltherr Exp $
// $Id$

package scalac.backend;

import java.util.Map;
import java.util.HashMap;

import scalac.Global;
import scalac.symtab.Definitions;
import scalac.symtab.TypeTags;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Debug;

/**
 * Primitive functions.
 *
 * @author Michel Schinz, Philippe Altherr
 * @version 1.0
 */

public class Primitives {

    //########################################################################
    // Primitives constants

    private static final Name RUNTIME_N   = Name.fromString("RunTime");

    private static final Name ZARRAY_N    = Name.fromString("zarray");
    private static final Name BARRAY_N    = Name.fromString("barray");
    private static final Name SARRAY_N    = Name.fromString("sarray");
    private static final Name CARRAY_N    = Name.fromString("carray");
    private static final Name IARRAY_N    = Name.fromString("iarray");
    private static final Name LARRAY_N    = Name.fromString("larray");
    private static final Name FARRAY_N    = Name.fromString("farray");
    private static final Name DARRAY_N    = Name.fromString("darray");
    private static final Name OARRAY_N    = Name.fromString("oarray");

    private static final Name ZARRAY_GET_N = Name.fromString("zarray_get");
    private static final Name BARRAY_GET_N = Name.fromString("barray_get");
    private static final Name SARRAY_GET_N = Name.fromString("sarray_get");
    private static final Name CARRAY_GET_N = Name.fromString("carray_get");
    private static final Name IARRAY_GET_N = Name.fromString("iarray_get");
    private static final Name LARRAY_GET_N = Name.fromString("larray_get");
    private static final Name FARRAY_GET_N = Name.fromString("farray_get");
    private static final Name DARRAY_GET_N = Name.fromString("darray_get");
    private static final Name OARRAY_GET_N = Name.fromString("oarray_get");

    private static final Name ZARRAY_SET_N = Name.fromString("zarray_set");
    private static final Name BARRAY_SET_N = Name.fromString("barray_set");
    private static final Name SARRAY_SET_N = Name.fromString("sarray_set");
    private static final Name CARRAY_SET_N = Name.fromString("carray_set");
    private static final Name IARRAY_SET_N = Name.fromString("iarray_set");
    private static final Name LARRAY_SET_N = Name.fromString("larray_set");
    private static final Name FARRAY_SET_N = Name.fromString("farray_set");
    private static final Name DARRAY_SET_N = Name.fromString("darray_set");
    private static final Name OARRAY_SET_N = Name.fromString("oarray_set");

    private static final Name BOX_N       = Name.fromString("box");

    private static final Name AS_UVALUE_N = Name.fromString("asUnit");
    private static final Name AS_ZVALUE_N = Name.fromString("asBoolean");
    private static final Name AS_BVALUE_N = Name.fromString("asByte");
    private static final Name AS_SVALUE_N = Name.fromString("asShort");
    private static final Name AS_CVALUE_N = Name.fromString("asChar");
    private static final Name AS_IVALUE_N = Name.fromString("asInt");
    private static final Name AS_LVALUE_N = Name.fromString("asLong");
    private static final Name AS_FVALUE_N = Name.fromString("asFloat");
    private static final Name AS_DVALUE_N = Name.fromString("asDouble");

    private static final Name AS_ZARRAY_N = Name.fromString("asBooleanArray");
    private static final Name AS_BARRAY_N = Name.fromString("asByteArray");
    private static final Name AS_SARRAY_N = Name.fromString("asShortArray");
    private static final Name AS_CARRAY_N = Name.fromString("asCharArray");
    private static final Name AS_IARRAY_N = Name.fromString("asIntArray");
    private static final Name AS_LARRAY_N = Name.fromString("asLongArray");
    private static final Name AS_FARRAY_N = Name.fromString("asFloatArray");
    private static final Name AS_DARRAY_N = Name.fromString("asDoubleArray");
    private static final Name AS_OARRAY_N = Name.fromString("asObjectArray");

    private static final Name IS_BVALUE_N = Name.fromString("isByte");
    private static final Name IS_SVALUE_N = Name.fromString("isShort");
    private static final Name IS_CVALUE_N = Name.fromString("isChar");
    private static final Name IS_IVALUE_N = Name.fromString("isInt");
    private static final Name IS_LVALUE_N = Name.fromString("isLong");
    private static final Name IS_FVALUE_N = Name.fromString("isFloat");
    private static final Name IS_DVALUE_N = Name.fromString("isDouble");

    //########################################################################
    // Primitives state

    private final Global global;
    private final Definitions definitions;
    private final Map/*<Symbol,Primitive>*/ primitives;

    public final Symbol RUNTIME;
    public final Type   RUNTIME_TYPE;

    public final Symbol NEW_ZARRAY;
    public final Symbol NEW_BARRAY;
    public final Symbol NEW_SARRAY;
    public final Symbol NEW_CARRAY;
    public final Symbol NEW_IARRAY;
    public final Symbol NEW_LARRAY;
    public final Symbol NEW_FARRAY;
    public final Symbol NEW_DARRAY;
    public final Symbol NEW_OARRAY;

    public final Symbol ZARRAY_GET;
    public final Symbol BARRAY_GET;
    public final Symbol SARRAY_GET;
    public final Symbol CARRAY_GET;
    public final Symbol IARRAY_GET;
    public final Symbol LARRAY_GET;
    public final Symbol FARRAY_GET;
    public final Symbol DARRAY_GET;
    public final Symbol OARRAY_GET;

    public final Symbol ZARRAY_SET;
    public final Symbol BARRAY_SET;
    public final Symbol SARRAY_SET;
    public final Symbol CARRAY_SET;
    public final Symbol IARRAY_SET;
    public final Symbol LARRAY_SET;
    public final Symbol FARRAY_SET;
    public final Symbol DARRAY_SET;
    public final Symbol OARRAY_SET;

    public final Symbol BOX_UVALUE;
    public final Symbol BOX_ZVALUE;
    public final Symbol BOX_BVALUE;
    public final Symbol BOX_SVALUE;
    public final Symbol BOX_CVALUE;
    public final Symbol BOX_IVALUE;
    public final Symbol BOX_LVALUE;
    public final Symbol BOX_FVALUE;
    public final Symbol BOX_DVALUE;

    public final Symbol BOX_ZARRAY;
    public final Symbol BOX_BARRAY;
    public final Symbol BOX_SARRAY;
    public final Symbol BOX_CARRAY;
    public final Symbol BOX_IARRAY;
    public final Symbol BOX_LARRAY;
    public final Symbol BOX_FARRAY;
    public final Symbol BOX_DARRAY;
    public final Symbol BOX_OARRAY;

    public final Symbol AS_UVALUE;
    public final Symbol AS_ZVALUE;
    public final Symbol AS_BVALUE;
    public final Symbol AS_SVALUE;
    public final Symbol AS_CVALUE;
    public final Symbol AS_IVALUE;
    public final Symbol AS_LVALUE;
    public final Symbol AS_FVALUE;
    public final Symbol AS_DVALUE;

    public final Symbol AS_ZARRAY;
    public final Symbol AS_BARRAY;
    public final Symbol AS_SARRAY;
    public final Symbol AS_CARRAY;
    public final Symbol AS_IARRAY;
    public final Symbol AS_LARRAY;
    public final Symbol AS_FARRAY;
    public final Symbol AS_DARRAY;
    public final Symbol AS_OARRAY;

    public final Symbol IS_BVALUE;
    public final Symbol IS_SVALUE;
    public final Symbol IS_CVALUE;
    public final Symbol IS_IVALUE;
    public final Symbol IS_LVALUE;
    public final Symbol IS_FVALUE;
    public final Symbol IS_DVALUE;

    //########################################################################
    // Primitives constructor

    public Primitives(Global global) {
        this.global = global;
        this.definitions = global.definitions;
        this.primitives = new HashMap();
        this.RUNTIME = definitions.getModule(Names.scala_runtime_RunTime);
        this.RUNTIME_TYPE =
            Type.singleType(definitions.SCALARUNTIME_TYPE, RUNTIME);
        this.NEW_ZARRAY = getUniqueTerm(RUNTIME, ZARRAY_N);
        this.NEW_BARRAY = getUniqueTerm(RUNTIME, BARRAY_N);
        this.NEW_SARRAY = getUniqueTerm(RUNTIME, SARRAY_N);
        this.NEW_CARRAY = getUniqueTerm(RUNTIME, CARRAY_N);
        this.NEW_IARRAY = getUniqueTerm(RUNTIME, IARRAY_N);
        this.NEW_LARRAY = getUniqueTerm(RUNTIME, LARRAY_N);
        this.NEW_FARRAY = getUniqueTerm(RUNTIME, FARRAY_N);
        this.NEW_DARRAY = getUniqueTerm(RUNTIME, DARRAY_N);
        this.NEW_OARRAY = getUniqueTerm(RUNTIME, OARRAY_N);
        this.ZARRAY_GET = getUniqueTerm(RUNTIME, ZARRAY_GET_N);
        this.BARRAY_GET = getUniqueTerm(RUNTIME, BARRAY_GET_N);
        this.SARRAY_GET = getUniqueTerm(RUNTIME, SARRAY_GET_N);
        this.CARRAY_GET = getUniqueTerm(RUNTIME, CARRAY_GET_N);
        this.IARRAY_GET = getUniqueTerm(RUNTIME, IARRAY_GET_N);
        this.LARRAY_GET = getUniqueTerm(RUNTIME, LARRAY_GET_N);
        this.FARRAY_GET = getUniqueTerm(RUNTIME, FARRAY_GET_N);
        this.DARRAY_GET = getUniqueTerm(RUNTIME, DARRAY_GET_N);
        this.OARRAY_GET = getUniqueTerm(RUNTIME, OARRAY_GET_N);
        this.ZARRAY_SET = getUniqueTerm(RUNTIME, ZARRAY_SET_N);
        this.BARRAY_SET = getUniqueTerm(RUNTIME, BARRAY_SET_N);
        this.SARRAY_SET = getUniqueTerm(RUNTIME, SARRAY_SET_N);
        this.CARRAY_SET = getUniqueTerm(RUNTIME, CARRAY_SET_N);
        this.IARRAY_SET = getUniqueTerm(RUNTIME, IARRAY_SET_N);
        this.LARRAY_SET = getUniqueTerm(RUNTIME, LARRAY_SET_N);
        this.FARRAY_SET = getUniqueTerm(RUNTIME, FARRAY_SET_N);
        this.DARRAY_SET = getUniqueTerm(RUNTIME, DARRAY_SET_N);
        this.OARRAY_SET = getUniqueTerm(RUNTIME, OARRAY_SET_N);
        Symbol[] boxes = getTerm(RUNTIME, BOX_N).alternatives();
        this.BOX_UVALUE = getBoxUnit(boxes);
        this.BOX_ZVALUE = getBoxValue(boxes, definitions.BOOLEAN_TYPE);
        this.BOX_BVALUE = getBoxValue(boxes, definitions.BYTE_TYPE);
        this.BOX_SVALUE = getBoxValue(boxes, definitions.SHORT_TYPE);
        this.BOX_CVALUE = getBoxValue(boxes, definitions.CHAR_TYPE);
        this.BOX_IVALUE = getBoxValue(boxes, definitions.INT_TYPE);
        this.BOX_LVALUE = getBoxValue(boxes, definitions.LONG_TYPE);
        this.BOX_FVALUE = getBoxValue(boxes, definitions.FLOAT_TYPE);
        this.BOX_DVALUE = getBoxValue(boxes, definitions.DOUBLE_TYPE);
        this.BOX_ZARRAY = getBoxArray(boxes, definitions.BOOLEAN_TYPE);
        this.BOX_BARRAY = getBoxArray(boxes, definitions.BYTE_TYPE);
        this.BOX_SARRAY = getBoxArray(boxes, definitions.SHORT_TYPE);
        this.BOX_CARRAY = getBoxArray(boxes, definitions.CHAR_TYPE);
        this.BOX_IARRAY = getBoxArray(boxes, definitions.INT_TYPE);
        this.BOX_LARRAY = getBoxArray(boxes, definitions.LONG_TYPE);
        this.BOX_FARRAY = getBoxArray(boxes, definitions.FLOAT_TYPE);
        this.BOX_DARRAY = getBoxArray(boxes, definitions.DOUBLE_TYPE);
        this.BOX_OARRAY = getBoxArray(boxes, definitions.JAVA_OBJECT_TYPE);
        this.AS_UVALUE = getUniqueTerm(definitions.UNIT_CLASS, AS_UVALUE_N);
        this.AS_ZVALUE = getUniqueTerm(definitions.BOOLEAN_CLASS, AS_ZVALUE_N);
        this.AS_BVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, AS_BVALUE_N);
        this.AS_SVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, AS_SVALUE_N);
        this.AS_CVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, AS_CVALUE_N);
        this.AS_IVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, AS_IVALUE_N);
        this.AS_LVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, AS_LVALUE_N);
        this.AS_FVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, AS_FVALUE_N);
        this.AS_DVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, AS_DVALUE_N);
        this.AS_ZARRAY = getUniqueTerm(definitions.ARRAY_CLASS, AS_ZARRAY_N);
        this.AS_BARRAY = getUniqueTerm(definitions.ARRAY_CLASS, AS_BARRAY_N);
        this.AS_SARRAY = getUniqueTerm(definitions.ARRAY_CLASS, AS_SARRAY_N);
        this.AS_CARRAY = getUniqueTerm(definitions.ARRAY_CLASS, AS_CARRAY_N);
        this.AS_IARRAY = getUniqueTerm(definitions.ARRAY_CLASS, AS_IARRAY_N);
        this.AS_LARRAY = getUniqueTerm(definitions.ARRAY_CLASS, AS_LARRAY_N);
        this.AS_FARRAY = getUniqueTerm(definitions.ARRAY_CLASS, AS_FARRAY_N);
        this.AS_DARRAY = getUniqueTerm(definitions.ARRAY_CLASS, AS_DARRAY_N);
        this.AS_OARRAY = getUniqueTerm(definitions.ARRAY_CLASS, AS_OARRAY_N);
        this.IS_BVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, IS_BVALUE_N);
        this.IS_SVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, IS_SVALUE_N);
        this.IS_CVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, IS_CVALUE_N);
        this.IS_IVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, IS_IVALUE_N);
        this.IS_LVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, IS_LVALUE_N);
        this.IS_FVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, IS_FVALUE_N);
        this.IS_DVALUE = getUniqueTerm(definitions.DOUBLE_CLASS, IS_DVALUE_N);
        initPrimitives();
    }

    //########################################################################
    // Private interface

    private Symbol getBoxUnit(Symbol[] alts) {
        for (int i = 0; i < alts.length; i++) {
            switch (alts[i].type()) {
            case MethodType(Symbol[] vparams, _):
                if (vparams.length == 0)
                    return alts[i];
            }
        }
        throw Debug.abort("not found:" + BOX_N + "()");
    }

    private Symbol getBoxValue(Symbol[] alts, Type type) {
        for (int i = 0; i < alts.length; i++) {
            switch (alts[i].type()) {
            case MethodType(Symbol[] vparams, _):
                if (vparams.length == 1 && vparams[0].type().equals(type))
                    return alts[i];
            }
        }
        throw Debug.abort("not found:" + BOX_N + "(" + Debug.show(type) + ")");
    }

    private Symbol getBoxArray(Symbol[] alts, Type type) {
        return getBoxValue(alts, definitions.arrayType(type));
    }

    private Symbol getUniqueTerm(Symbol owner, Name name) {
        Symbol symbol = getTerm(owner, name);
        assert !symbol.isOverloaded() :
            Debug.show(owner) + "." + name + " -> " + Debug.show(symbol);
        return symbol;
    }

    private Symbol getTerm(Symbol owner, Name name) {
        Symbol symbol = owner.lookup(name);
        assert symbol != Symbol.NONE : Debug.show(owner) + "." + name;
        return symbol;
    }

    private void initPrimitives() {
        Definitions defs = definitions;

        // scala.Any
        addPrimitive(defs.IS, Primitive.IS);
        addPrimitive(defs.AS, Primitive.AS);
        addPrimitive(defs.EQEQ, Primitive.EQ);
        addPrimitive(defs.BANGEQ, Primitive.NE);
        // !!! addPrimitive(defs.EQUALS, Primitive.EQUALS);
        addPrimitive(defs.HASHCODE, Primitive.HASHCODE);
        addPrimitive(defs.TOSTRING, Primitive.TOSTRING);

        // scala.Unit
        // !!! addAll(defs.UNIT_CLASS, Names.EQ, Primitive.EQ, 1);
        // !!! addAll(defs.UNIT_CLASS, Names.NE, Primitive.NE, 1);
        addAll(defs.UNIT_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.UNIT_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.UNIT_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addPrimitive(AS_UVALUE, Primitive.AS_UVALUE);
        // !!! addAll(defs.UNIT_CLASS, Names.ADD, Primitive.CONCAT, 1);

        // scala.Boolean
        addAll(defs.BOOLEAN_CLASS, Names.EQ, Primitive.EQ, 1);
        addAll(defs.BOOLEAN_CLASS, Names.NE, Primitive.NE, 1);
        addAll(defs.BOOLEAN_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.BOOLEAN_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.BOOLEAN_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addPrimitive(AS_ZVALUE, Primitive.AS_ZVALUE);
        addAll(defs.BOOLEAN_CLASS, Names.ZNOT, Primitive.ZNOT, 1);
        addAll(defs.BOOLEAN_CLASS, Names.OR, Primitive.OR, 1);
        addAll(defs.BOOLEAN_CLASS, Names.XOR, Primitive.XOR, 1);
        addAll(defs.BOOLEAN_CLASS, Names.AND, Primitive.AND, 1);
        addAll(defs.BOOLEAN_CLASS, Names.ZOR, Primitive.ZOR, 1);
        addAll(defs.BOOLEAN_CLASS, Names.ZAND, Primitive.ZAND, 1);
        // !!! addAll(defs.BOOLEAN_CLASS, Names.ADD, Primitive.CONCAT, 1);

        // scala.Byte
        addAll(defs.BYTE_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.BYTE_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.BYTE_CLASS, Names.toString, Primitive.TOSTRING, 1);

        // scala.Short
        addAll(defs.SHORT_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.SHORT_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.SHORT_CLASS, Names.toString, Primitive.TOSTRING, 1);

        // scala.Char
        addAll(defs.CHAR_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.CHAR_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.CHAR_CLASS, Names.toString, Primitive.TOSTRING, 1);

        // scala.Int
        addAll(defs.INT_CLASS, Names.EQ, Primitive.EQ, 4);
        addAll(defs.INT_CLASS, Names.NE, Primitive.NE, 4);
        addAll(defs.INT_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.INT_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.INT_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAll(defs.INT_CLASS, Names.NOT, Primitive.NOT, 2);
        addAdd(defs.INT_CLASS, 4);
        addSub(defs.INT_CLASS, 4);
        addAll(defs.INT_CLASS, Names.MUL, Primitive.MUL, 4);
        addAll(defs.INT_CLASS, Names.DIV, Primitive.DIV, 4);
        addAll(defs.INT_CLASS, Names.MOD, Primitive.MOD, 4);
        addAll(defs.INT_CLASS, Names.LT, Primitive.LT, 4);
        addAll(defs.INT_CLASS, Names.LE, Primitive.LE, 4);
        addAll(defs.INT_CLASS, Names.GT, Primitive.GT, 4);
        addAll(defs.INT_CLASS, Names.GE, Primitive.GE, 4);
        addAll(defs.INT_CLASS, Names.OR, Primitive.OR, 2);
        addAll(defs.INT_CLASS, Names.XOR, Primitive.XOR, 2);
        addAll(defs.INT_CLASS, Names.AND, Primitive.AND, 2);
        addAll(defs.INT_CLASS, Names.LSL, Primitive.LSL, 2);
        addAll(defs.INT_CLASS, Names.LSR, Primitive.LSR, 2);
        addAll(defs.INT_CLASS, Names.ASR, Primitive.ASR, 2);

        // scala.Long
        addAll(defs.LONG_CLASS, Names.EQ, Primitive.EQ, 3);
        addAll(defs.LONG_CLASS, Names.NE, Primitive.NE, 3);
        addAll(defs.LONG_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.LONG_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.LONG_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAll(defs.LONG_CLASS, Names.NOT, Primitive.NOT, 1);
        addAdd(defs.LONG_CLASS, 3);
        addSub(defs.LONG_CLASS, 3);
        addAll(defs.LONG_CLASS, Names.MUL, Primitive.MUL, 3);
        addAll(defs.LONG_CLASS, Names.DIV, Primitive.DIV, 3);
        addAll(defs.LONG_CLASS, Names.MOD, Primitive.MOD, 3);
        addAll(defs.LONG_CLASS, Names.LT, Primitive.LT, 3);
        addAll(defs.LONG_CLASS, Names.LE, Primitive.LE, 3);
        addAll(defs.LONG_CLASS, Names.GT, Primitive.GT, 3);
        addAll(defs.LONG_CLASS, Names.GE, Primitive.GE, 3);
        addAll(defs.LONG_CLASS, Names.OR, Primitive.OR, 1);
        addAll(defs.LONG_CLASS, Names.XOR, Primitive.XOR, 1);
        addAll(defs.LONG_CLASS, Names.AND, Primitive.AND, 1);
        addAll(defs.LONG_CLASS, Names.LSL, Primitive.LSL, 1);
        addAll(defs.LONG_CLASS, Names.LSR, Primitive.LSR, 1);
        addAll(defs.LONG_CLASS, Names.ASR, Primitive.ASR, 1);

        // scala.Float
        addAll(defs.FLOAT_CLASS, Names.EQ, Primitive.EQ, 2);
        addAll(defs.FLOAT_CLASS, Names.NE, Primitive.NE, 2);
        addAll(defs.FLOAT_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.FLOAT_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.FLOAT_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAdd(defs.FLOAT_CLASS, 2);
        addSub(defs.FLOAT_CLASS, 2);
        addAll(defs.FLOAT_CLASS, Names.MUL, Primitive.MUL, 2);
        addAll(defs.FLOAT_CLASS, Names.DIV, Primitive.DIV, 2);
        addAll(defs.FLOAT_CLASS, Names.MOD, Primitive.MOD, 2);
        addAll(defs.FLOAT_CLASS, Names.LT, Primitive.LT, 2);
        addAll(defs.FLOAT_CLASS, Names.LE, Primitive.LE, 2);
        addAll(defs.FLOAT_CLASS, Names.GT, Primitive.GT, 2);
        addAll(defs.FLOAT_CLASS, Names.GE, Primitive.GE, 2);

        // scala.Double
        addAll(defs.DOUBLE_CLASS, Names.EQ, Primitive.EQ, 1);
        addAll(defs.DOUBLE_CLASS, Names.NE, Primitive.NE, 1);
        addAll(defs.DOUBLE_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.DOUBLE_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.DOUBLE_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addPrimitive(AS_BVALUE, Primitive.AS_BVALUE);
        addPrimitive(AS_SVALUE, Primitive.AS_SVALUE);
        addPrimitive(AS_CVALUE, Primitive.AS_CVALUE);
        addPrimitive(AS_IVALUE, Primitive.AS_IVALUE);
        addPrimitive(AS_LVALUE, Primitive.AS_LVALUE);
        addPrimitive(AS_FVALUE, Primitive.AS_FVALUE);
        addPrimitive(AS_DVALUE, Primitive.AS_DVALUE);
        addAdd(defs.DOUBLE_CLASS, 1);
        addSub(defs.DOUBLE_CLASS, 1);
        addAll(defs.DOUBLE_CLASS, Names.MUL, Primitive.MUL, 1);
        addAll(defs.DOUBLE_CLASS, Names.DIV, Primitive.DIV, 1);
        addAll(defs.DOUBLE_CLASS, Names.MOD, Primitive.MOD, 1);
        addAll(defs.DOUBLE_CLASS, Names.LT, Primitive.LT, 1);
        addAll(defs.DOUBLE_CLASS, Names.LE, Primitive.LE, 1);
        addAll(defs.DOUBLE_CLASS, Names.GT, Primitive.GT, 1);
        addAll(defs.DOUBLE_CLASS, Names.GE, Primitive.GE, 1);

        // scala.Array
        // !!! addAll(defs.ARRAY_CLASS, defs.EQEQ_N, Primitive.EQ, 1);
        // !!! addAll(defs.ARRAY_CLASS, defs.BANGEQ_N, Primitive.NE, 1);
        // !!! addAll(defs.ARRAY_CLASS, Names.equals, Primitive.EQUALS, 1);
        // !!! addAll(defs.ARRAY_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        // !!! addAll(defs.ARRAY_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addPrimitive(AS_ZARRAY, Primitive.AS_ZARRAY);
        addPrimitive(AS_BARRAY, Primitive.AS_BARRAY);
        addPrimitive(AS_SARRAY, Primitive.AS_SARRAY);
        addPrimitive(AS_CARRAY, Primitive.AS_CARRAY);
        addPrimitive(AS_IARRAY, Primitive.AS_IARRAY);
        addPrimitive(AS_LARRAY, Primitive.AS_LARRAY);
        addPrimitive(AS_FARRAY, Primitive.AS_FARRAY);
        addPrimitive(AS_DARRAY, Primitive.AS_DARRAY);
        addPrimitive(AS_OARRAY, Primitive.AS_OARRAY);
        addAll(defs.ARRAY_CLASS, Names.length, Primitive.LENGTH, 1);
        addAll(defs.ARRAY_CLASS, Names.apply, Primitive.APPLY, 2);
        addAll(defs.ARRAY_CLASS, Names.update, Primitive.UPDATE, 1);

        // scala.String
        addPrimitive(defs.STRING_PLUS_ANY, Primitive.CONCAT);

        // java.lang.Throwable
        addPrimitive(defs.THROW, Primitive.THROW);

        // scala.runtime.RunTime
        addPrimitive(BOX_UVALUE, Primitive.BOX);
        addPrimitive(BOX_ZVALUE, Primitive.BOX);
        addPrimitive(BOX_BVALUE, Primitive.BOX);
        addPrimitive(BOX_SVALUE, Primitive.BOX);
        addPrimitive(BOX_CVALUE, Primitive.BOX);
        addPrimitive(BOX_IVALUE, Primitive.BOX);
        addPrimitive(BOX_LVALUE, Primitive.BOX);
        addPrimitive(BOX_FVALUE, Primitive.BOX);
        addPrimitive(BOX_DVALUE, Primitive.BOX);
        addPrimitive(BOX_ZARRAY, Primitive.BOX);
        addPrimitive(BOX_BARRAY, Primitive.BOX);
        addPrimitive(BOX_SARRAY, Primitive.BOX);
        addPrimitive(BOX_CARRAY, Primitive.BOX);
        addPrimitive(BOX_IARRAY, Primitive.BOX);
        addPrimitive(BOX_LARRAY, Primitive.BOX);
        addPrimitive(BOX_FARRAY, Primitive.BOX);
        addPrimitive(BOX_DARRAY, Primitive.BOX);
        addPrimitive(BOX_OARRAY, Primitive.BOX);
        addPrimitive(NEW_ZARRAY, Primitive.NEW_ZARRAY);
        addPrimitive(NEW_BARRAY, Primitive.NEW_BARRAY);
        addPrimitive(NEW_SARRAY, Primitive.NEW_SARRAY);
        addPrimitive(NEW_CARRAY, Primitive.NEW_CARRAY);
        addPrimitive(NEW_IARRAY, Primitive.NEW_IARRAY);
        addPrimitive(NEW_LARRAY, Primitive.NEW_LARRAY);
        addPrimitive(NEW_FARRAY, Primitive.NEW_FARRAY);
        addPrimitive(NEW_DARRAY, Primitive.NEW_DARRAY);
        addPrimitive(NEW_OARRAY, Primitive.NEW_OARRAY);
        addPrimitive(ZARRAY_GET, Primitive.ZARRAY_GET);
        addPrimitive(BARRAY_GET, Primitive.BARRAY_GET);
        addPrimitive(SARRAY_GET, Primitive.SARRAY_GET);
        addPrimitive(CARRAY_GET, Primitive.CARRAY_GET);
        addPrimitive(IARRAY_GET, Primitive.IARRAY_GET);
        addPrimitive(LARRAY_GET, Primitive.LARRAY_GET);
        addPrimitive(FARRAY_GET, Primitive.FARRAY_GET);
        addPrimitive(DARRAY_GET, Primitive.DARRAY_GET);
        addPrimitive(OARRAY_GET, Primitive.OARRAY_GET);
        addPrimitive(ZARRAY_SET, Primitive.ZARRAY_SET);
        addPrimitive(BARRAY_SET, Primitive.BARRAY_SET);
        addPrimitive(SARRAY_SET, Primitive.SARRAY_SET);
        addPrimitive(CARRAY_SET, Primitive.CARRAY_SET);
        addPrimitive(IARRAY_SET, Primitive.IARRAY_SET);
        addPrimitive(LARRAY_SET, Primitive.LARRAY_SET);
        addPrimitive(FARRAY_SET, Primitive.FARRAY_SET);
        addPrimitive(DARRAY_SET, Primitive.DARRAY_SET);
        addPrimitive(OARRAY_SET, Primitive.OARRAY_SET);

        // !!! Boolean.True
        // !!! Boolean.False
    }

    private void addAdd(Symbol clasz, int count) {
        Name name = Names.ADD;
        Symbol symbol = clasz.lookup(name);
        assert symbol != Symbol.NONE : Debug.show(clasz) + "." + name;
        Symbol[] alts = symbol.alternatives();
        // !!! assert count + 2 == alts.length : (count + 2) + " != " + alts.length;
        boolean pos = false;
        boolean concat = false;
        for (int i = 0; i < alts.length; i++) {
            switch (alts[i].type()) {
            case MethodType(Symbol[] vparams, _):
                if (vparams.length == 0) {
                    // !!! System.out.println("!!! Ignoring pico bridge method " + Debug.show(clasz) + "." + name);
                    break;
                }
                if (vparams[0].type().equals(definitions.JAVA_STRING_TYPE)) {
                    addPrimitive(alts[i], Primitive.CONCAT);
                    assert !concat;
                    concat = true;
                } else {
                    addPrimitive(alts[i], Primitive.ADD);
                    count--;
                }
                break;
            case PolyType(Symbol[] tparams, _):
                addPrimitive(alts[i], Primitive.POS);
                assert !pos;
                pos = true;
                break;
            default:
                System.out.println("!!! symbol = " + Debug.show(alts[i]));
                throw Debug.abort("illegal case" , alts[i].type());
            }
        }
        assert pos && concat : pos + "," + concat;
    }

    private void addSub(Symbol clasz, int count) {
        Name name = Names.SUB;
        Symbol symbol = clasz.lookup(name);
        assert symbol != Symbol.NONE : Debug.show(clasz) + "." + name;
        Symbol[] alts = symbol.alternatives();
        // !!! assert count + 1 == alts.length : (count + 1) + " != " + alts.length;
        boolean pos = false;
        for (int i = 0; i < alts.length; i++) {
            switch (alts[i].type()) {
            case MethodType(Symbol[] vparams, _):
                if (vparams.length == 0) {
                    // !!! System.out.println("!!! Ignoring pico bridge method " + Debug.show(clasz) + "." + name);
                    break;
                }
                addPrimitive(alts[i], Primitive.SUB);
                count--;
                break;
            case PolyType(Symbol[] tparams, _):
                addPrimitive(alts[i], Primitive.NEG);
                assert !pos;
                pos = true;
                break;
            default:
                System.out.println("!!! symbol = " + Debug.show(alts[i]));
                throw Debug.abort("illegal case" , alts[i].type());
            }
        }
        assert pos : pos;
    }

    private void addAll(Symbol clasz,Name name,Primitive primitive,int count) {
        Symbol symbol = clasz.lookup(name);
        assert symbol != Symbol.NONE : Debug.show(clasz) + "." + name;
        Symbol[] alts = symbol.alternatives();
        assert count == alts.length : count + " != " + alts.length;
        for (int i = 0; i < alts.length; i++) addPrimitive(alts[i], primitive);
    }

    private void addPrimitive(Symbol symbol, Primitive primitive) {
        assert !primitives.containsKey(symbol) : Debug.show(symbol);
        primitives.put(symbol, primitive);
    }

    //########################################################################
    // Primitives interface - general primitives

    /** Return true iff the given symbol refers to a primitive. */
    public boolean isPrimitive(Symbol symbol) {
        return primitives.containsKey(symbol);
    }

    /** Return primitive identified by the symbol. */
    public Primitive getPrimitive(Symbol symbol) {
        Object value = primitives.get(symbol);
        return value == null ? Primitive.NOT_A_PRIMITIVE : (Primitive)value;
    }

    /** Return index of primitive identified by the symbol. */
    public int getPrimitiveIndex(Symbol symbol) {
        return getPrimitive(symbol).tag;
    }

    //########################################################################
    // Primitives interface - array creation primitives

    /** Return array creation method for elements of the given kind. */
    public Symbol getNewArraySymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return NEW_ZARRAY;
        case TypeTags.BYTE   : return NEW_BARRAY;
        case TypeTags.SHORT  : return NEW_SARRAY;
        case TypeTags.CHAR   : return NEW_CARRAY;
        case TypeTags.INT    : return NEW_IARRAY;
        case TypeTags.LONG   : return NEW_LARRAY;
        case TypeTags.FLOAT  : return NEW_FARRAY;
        case TypeTags.DOUBLE : return NEW_DARRAY;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
    // Primitives interface - boxing primitives

    /** Return box method for values of the given type. */
    public Symbol getBoxValueSymbol(Type type) {
        switch (type) {
        case UnboxedType(int kind):
            return getBoxValueSymbol(kind);
        case UnboxedArrayType(Type elemtp):
            return getBoxArraySymbol(elemtp);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Return box method for values of the given kind. */
    public Symbol getBoxValueSymbol(int kind) {
        switch (kind) {
        case TypeTags.UNIT   : return BOX_UVALUE;
        case TypeTags.BOOLEAN: return BOX_ZVALUE;
        case TypeTags.BYTE   : return BOX_BVALUE;
        case TypeTags.SHORT  : return BOX_SVALUE;
        case TypeTags.CHAR   : return BOX_CVALUE;
        case TypeTags.INT    : return BOX_IVALUE;
        case TypeTags.LONG   : return BOX_LVALUE;
        case TypeTags.FLOAT  : return BOX_FVALUE;
        case TypeTags.DOUBLE : return BOX_DVALUE;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    /** Return box method for arrays of elements of the given type. */
    public Symbol getBoxArraySymbol(Type type) {
        switch (type) {
        case UnboxedType(int kind):
            return getBoxArraySymbol(kind);
        default:
            return BOX_OARRAY;
        }
    }

    /** Return box method for arrays of elements of the given kind. */
    public Symbol getBoxArraySymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return BOX_ZARRAY;
        case TypeTags.BYTE   : return BOX_BARRAY;
        case TypeTags.SHORT  : return BOX_SARRAY;
        case TypeTags.CHAR   : return BOX_CARRAY;
        case TypeTags.INT    : return BOX_IARRAY;
        case TypeTags.LONG   : return BOX_LARRAY;
        case TypeTags.FLOAT  : return BOX_FARRAY;
        case TypeTags.DOUBLE : return BOX_DARRAY;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
    // Primitives interface - unboxing primitives

    /** Return unbox method returning values of the given type. */
    public Symbol getUnboxValueSymbol(Type type) {
        switch (type) {
        case UnboxedType(int kind):
            return getUnboxValueSymbol(kind);
        case UnboxedArrayType(Type elemtp):
            return getUnboxArraySymbol(elemtp);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Return unbox method returning values of the given kind. */
    public Symbol getUnboxValueSymbol(int kind) {
        switch (kind) {
        case TypeTags.UNIT   : return AS_UVALUE;
        case TypeTags.BOOLEAN: return AS_ZVALUE;
        case TypeTags.BYTE   : return AS_BVALUE;
        case TypeTags.SHORT  : return AS_SVALUE;
        case TypeTags.CHAR   : return AS_CVALUE;
        case TypeTags.INT    : return AS_IVALUE;
        case TypeTags.LONG   : return AS_LVALUE;
        case TypeTags.FLOAT  : return AS_FVALUE;
        case TypeTags.DOUBLE : return AS_DVALUE;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    /** Return unbox method returning arrays of elements of the given type. */
    public Symbol getUnboxArraySymbol(Type type) {
        switch (type) {
        case UnboxedType(int kind):
            return getUnboxArraySymbol(kind);
        default:
            return AS_OARRAY;
        }
    }

    /** Return unbox method returning arrays of elements of the given kind. */
    public Symbol getUnboxArraySymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return AS_ZARRAY;
        case TypeTags.BYTE   : return AS_BARRAY;
        case TypeTags.SHORT  : return AS_SARRAY;
        case TypeTags.CHAR   : return AS_CARRAY;
        case TypeTags.INT    : return AS_IARRAY;
        case TypeTags.LONG   : return AS_LARRAY;
        case TypeTags.FLOAT  : return AS_FARRAY;
        case TypeTags.DOUBLE : return AS_DARRAY;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    /** Return instance testing symbol for given (unboxed) type. */
    public Symbol getInstanceTestSymbol(Type type) {
        switch (type) {
        case UnboxedType(int kind):
            return getInstanceTestSymbol(kind);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    public Symbol getInstanceTestSymbol(int kind) {
        switch(kind) {
        case TypeTags.BYTE   : return IS_BVALUE;
        case TypeTags.SHORT  : return IS_SVALUE;
        case TypeTags.CHAR   : return IS_CVALUE;
        case TypeTags.INT    : return IS_IVALUE;
        case TypeTags.LONG   : return IS_LVALUE;
        case TypeTags.FLOAT  : return IS_FVALUE;
        case TypeTags.DOUBLE : return IS_DVALUE;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
    // Primitives interface - array get and set primitives

    /** Return get method for arrays of the given type. */
    public Symbol getArrayGetSymbol(Type type) {
        switch (type) {
        case UnboxedArrayType(UnboxedType(int kind)):
            return getArrayGetSymbol(kind);
        case UnboxedArrayType(_):
            return OARRAY_GET;
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Return get method for arrays of elements of the given kind. */
    public Symbol getArrayGetSymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return ZARRAY_GET;
        case TypeTags.BYTE   : return BARRAY_GET;
        case TypeTags.SHORT  : return SARRAY_GET;
        case TypeTags.CHAR   : return CARRAY_GET;
        case TypeTags.INT    : return IARRAY_GET;
        case TypeTags.LONG   : return LARRAY_GET;
        case TypeTags.FLOAT  : return FARRAY_GET;
        case TypeTags.DOUBLE : return DARRAY_GET;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    /** Return set method for arrays of the given type. */
    public Symbol getArraySetSymbol(Type type) {
        switch (type) {
        case UnboxedArrayType(UnboxedType(int kind)):
            return getArraySetSymbol(kind);
        case UnboxedArrayType(_):
            return OARRAY_SET;
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Return set method for arrays of elements of the given kind. */
    public Symbol getArraySetSymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return ZARRAY_SET;
        case TypeTags.BYTE   : return BARRAY_SET;
        case TypeTags.SHORT  : return SARRAY_SET;
        case TypeTags.CHAR   : return CARRAY_SET;
        case TypeTags.INT    : return IARRAY_SET;
        case TypeTags.LONG   : return LARRAY_SET;
        case TypeTags.FLOAT  : return FARRAY_SET;
        case TypeTags.DOUBLE : return DARRAY_SET;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
    // Primitives interface - class names

    /* Return name to use in "Class.forName(<name>)" for the given type. */
    public String getNameForClassForName(Type type) {
        switch (type) {
        case TypeRef(_, Symbol symbol, _):
            return getNameForClassForName(symbol);
        case UnboxedType(int kind):
            return getNameForClassForName(kind);
        case UnboxedArrayType(TypeRef(_, Symbol symbol, _)):
            return "[L" + getNameForClassForName(symbol) + ";";
        case UnboxedArrayType(Type elemtp):
            return "[" + getNameForClassForName(elemtp);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /* Return name to use in "Class.forName(<name>)" for the given symbol. */
    public String getNameForClassForName(Symbol symbol) {
        if (symbol == definitions.ANY_CLASS ||
            symbol == definitions.ANYREF_CLASS)
            symbol = definitions.JAVA_OBJECT_CLASS;
        StringBuffer buffer = new StringBuffer(symbol.name.toString());
        for (symbol = symbol.owner();
             // !!! isPackage was isJavaPackageClass
             symbol != definitions.ROOT_CLASS && !symbol.isPackage();
             symbol = symbol.owner()) {
            buffer.insert(0, '$');
            buffer.insert(0, symbol.name);
        }
        if (symbol != definitions.ROOT_CLASS) {
            buffer.insert(0, '.');
            buffer.insert(0, symbol.fullName());
        }
        return buffer.toString();
    }

    /* Return name to use in "Class.forName(<name>)" for the given kind. */
    public String getNameForClassForName(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return "Z";
        case TypeTags.BYTE   : return "B";
        case TypeTags.SHORT  : return "S";
        case TypeTags.CHAR   : return "C";
        case TypeTags.INT    : return "I";
        case TypeTags.LONG   : return "J";
        case TypeTags.FLOAT  : return "F";
        case TypeTags.DOUBLE : return "D";
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
}
