/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// Old$Id$
// $Id$

package scalac.backend;

import scalac.util.Debug;

/**
 * Primitive functions.
 *
 * @author Michel Schinz, Philippe Altherr
 * @version 1.0
 */

public class Primitive {
    public int tag;

    // Non-primitive operations
    public case NOT_A_PRIMITIVE { tag =  0; } // not a primitive

    // Arithmetic unary operations
    public case POS             { tag =  1; } // +x
    public case NEG             { tag =  2; } // -x
    public case NOT             { tag =  3; } // ~x

    // Arithmetic binary operations
    public case ADD             { tag =  4; } // x + y
    public case SUB             { tag =  5; } // x - y
    public case MUL             { tag =  6; } // x * y
    public case DIV             { tag =  7; } // x / y
    public case MOD             { tag =  8; } // x % y

    // Bitwise operations
    public case OR              { tag =  9; } // x | y
    public case XOR             { tag = 10; } // x ^ y
    public case AND             { tag = 11; } // x & y

    // Shift operations
    public case LSL             { tag = 12; } // x << y
    public case LSR             { tag = 13; } // x >>> y
    public case ASR             { tag = 14; } // x >> y

    // Comparison operations
    public case EQ              { tag = 15; } // x == y
    public case NE              { tag = 16; } // x != y
    public case LT              { tag = 17; } // x < y
    public case LE              { tag = 18; } // x <= y
    public case GE              { tag = 19; } // x > y
    public case GT              { tag = 20; } // x >= y

    // Boolean unary operations
    public case ZNOT            { tag = 21; } // !x

    // Boolean binary operations
    public case ZOR             { tag = 22; } // x || y
    public case ZAND            { tag = 23; } // x && y

    // Array operations
    public case LENGTH          { tag = 24; } // x.length
    public case APPLY           { tag = 25; } // x(y)
    public case UPDATE          { tag = 26; } // x(y) = z

    // Conversion operations
    public case AS_UVALUE       { tag = 27; } // x.asUnit()
    public case AS_ZVALUE       { tag = 28; } // x.asBoolean()
    public case AS_BVALUE       { tag = 29; } // x.asByte()
    public case AS_SVALUE       { tag = 30; } // x.asShort()
    public case AS_CVALUE       { tag = 31; } // x.asChar()
    public case AS_IVALUE       { tag = 32; } // x.asInt()
    public case AS_LVALUE       { tag = 33; } // x.asLong()
    public case AS_FVALUE       { tag = 34; } // x.asFloat()
    public case AS_DVALUE       { tag = 35; } // x.asDouble()
    public case AS_ZARRAY       { tag = 36; } // x.asBooleanArray()
    public case AS_BARRAY       { tag = 37; } // x.asByteArray()
    public case AS_SARRAY       { tag = 38; } // x.asShortArray()
    public case AS_CARRAY       { tag = 39; } // x.asCharArray()
    public case AS_IARRAY       { tag = 40; } // x.asIntArray()
    public case AS_LARRAY       { tag = 41; } // x.asLongArray()
    public case AS_FARRAY       { tag = 42; } // x.asFloatArray()
    public case AS_DARRAY       { tag = 43; } // x.asDoubleArray()
    public case AS_OARRAY       { tag = 44; } // x.asObjectArray()

    // Any operations
    public case IS              { tag = 45; } // x.is[y]
    public case AS              { tag = 46; } // x.as[y]
    public case EQUALS          { tag = 47; } // x.equals(y)
    public case HASHCODE        { tag = 48; } // x.hashcode()
    public case TOSTRING        { tag = 49; } // x.toString()

    // String operations
    public case CONCAT          { tag = 50; } // String.valueOf(x)+String.valueOf(y)

    // Throwable operations
    public case THROW           { tag = 51; } // throw x

    // RunTime operations
    public case BOX             { tag = 52; } // RunTime.box(x)
    public case NEW_ZARRAY      { tag = 53; } // RunTime.zarray(x)
    public case NEW_BARRAY      { tag = 54; } // RunTime.barray(x)
    public case NEW_SARRAY      { tag = 55; } // RunTime.sarray(x)
    public case NEW_CARRAY      { tag = 56; } // RunTime.carray(x)
    public case NEW_IARRAY      { tag = 57; } // RunTime.iarray(x)
    public case NEW_LARRAY      { tag = 58; } // RunTime.larray(x)
    public case NEW_FARRAY      { tag = 59; } // RunTime.farray(x)
    public case NEW_DARRAY      { tag = 60; } // RunTime.darray(x)
    public case NEW_OARRAY      { tag = 61; } // RunTime.oarray(x)
    public case ZARRAY_LENGTH   { tag = 62; } // RunTime.zarray_length(x)
    public case BARRAY_LENGTH   { tag = 63; } // RunTime.barray_length(x)
    public case SARRAY_LENGTH   { tag = 64; } // RunTime.sarray_length(x)
    public case CARRAY_LENGTH   { tag = 65; } // RunTime.carray_length(x)
    public case IARRAY_LENGTH   { tag = 66; } // RunTime.iarray_length(x)
    public case LARRAY_LENGTH   { tag = 67; } // RunTime.larray_length(x)
    public case FARRAY_LENGTH   { tag = 68; } // RunTime.farray_length(x)
    public case DARRAY_LENGTH   { tag = 69; } // RunTime.darray_length(x)
    public case OARRAY_LENGTH   { tag = 70; } // RunTime.oarray_length(x)
    public case ZARRAY_GET      { tag = 71; } // RunTime.zarray_get(x,y)
    public case BARRAY_GET      { tag = 72; } // RunTime.barray_get(x,y)
    public case SARRAY_GET      { tag = 73; } // RunTime.sarray_get(x,y)
    public case CARRAY_GET      { tag = 74; } // RunTime.carray_get(x,y)
    public case IARRAY_GET      { tag = 75; } // RunTime.iarray_get(x,y)
    public case LARRAY_GET      { tag = 76; } // RunTime.larray_get(x,y)
    public case FARRAY_GET      { tag = 77; } // RunTime.farray_get(x,y)
    public case DARRAY_GET      { tag = 78; } // RunTime.darray_get(x,y)
    public case OARRAY_GET      { tag = 79; } // RunTime.oarray_get(x,y)
    public case ZARRAY_SET      { tag = 80; } // RunTime.zarray(x,y,z)
    public case BARRAY_SET      { tag = 81; } // RunTime.barray(x,y,z)
    public case SARRAY_SET      { tag = 82; } // RunTime.sarray(x,y,z)
    public case CARRAY_SET      { tag = 83; } // RunTime.carray(x,y,z)
    public case IARRAY_SET      { tag = 84; } // RunTime.iarray(x,y,z)
    public case LARRAY_SET      { tag = 85; } // RunTime.larray(x,y,z)
    public case FARRAY_SET      { tag = 86; } // RunTime.farray(x,y,z)
    public case DARRAY_SET      { tag = 87; } // RunTime.darray(x,y,z)
    public case OARRAY_SET      { tag = 88; } // RunTime.oarray(x,y,z)

    /** Return negated version of comparison primitive. */
    public Primitive negate() {
        switch (this) {
        case LT: return Primitive.GE;
        case LE: return Primitive.GT;
        case EQ: return Primitive.NE;
        case NE: return Primitive.EQ;
        case GE: return Primitive.LT;
        case GT: return Primitive.LE;
        default: throw Debug.abort("unknown primitive", this);
        }
    }

    /** Return primitive with arguments swapped (e.g. <= is turned
     ** into =>). */
    public Primitive swap() {
        switch (this) {
        case LT: return Primitive.GT;
        case LE: return Primitive.GE;
        case EQ: return Primitive.EQ;
        case NE: return Primitive.NE;
        case GE: return Primitive.LE;
        case GT: return Primitive.LT;
        default: throw Debug.abort("unknown primitive", this);
        }
    }
}
