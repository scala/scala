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
    public case ID              { tag = 90; } // x eq y
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
    public case BOX             { tag = 52; } // RunTime.box_<X>(x)
    public case UNBOX           { tag = 53; } // RunTime.unbox_<X>(x)
    public case NEW_ZARRAY      { tag = 54; } // RunTime.zarray(x)
    public case NEW_BARRAY      { tag = 55; } // RunTime.barray(x)
    public case NEW_SARRAY      { tag = 56; } // RunTime.sarray(x)
    public case NEW_CARRAY      { tag = 57; } // RunTime.carray(x)
    public case NEW_IARRAY      { tag = 58; } // RunTime.iarray(x)
    public case NEW_LARRAY      { tag = 59; } // RunTime.larray(x)
    public case NEW_FARRAY      { tag = 60; } // RunTime.farray(x)
    public case NEW_DARRAY      { tag = 61; } // RunTime.darray(x)
    public case NEW_OARRAY      { tag = 62; } // RunTime.oarray(x)
    public case ZARRAY_LENGTH   { tag = 63; } // RunTime.zarray_length(x)
    public case BARRAY_LENGTH   { tag = 64; } // RunTime.barray_length(x)
    public case SARRAY_LENGTH   { tag = 65; } // RunTime.sarray_length(x)
    public case CARRAY_LENGTH   { tag = 66; } // RunTime.carray_length(x)
    public case IARRAY_LENGTH   { tag = 67; } // RunTime.iarray_length(x)
    public case LARRAY_LENGTH   { tag = 68; } // RunTime.larray_length(x)
    public case FARRAY_LENGTH   { tag = 69; } // RunTime.farray_length(x)
    public case DARRAY_LENGTH   { tag = 70; } // RunTime.darray_length(x)
    public case OARRAY_LENGTH   { tag = 71; } // RunTime.oarray_length(x)
    public case ZARRAY_GET      { tag = 72; } // RunTime.zarray_get(x,y)
    public case BARRAY_GET      { tag = 73; } // RunTime.barray_get(x,y)
    public case SARRAY_GET      { tag = 74; } // RunTime.sarray_get(x,y)
    public case CARRAY_GET      { tag = 75; } // RunTime.carray_get(x,y)
    public case IARRAY_GET      { tag = 76; } // RunTime.iarray_get(x,y)
    public case LARRAY_GET      { tag = 77; } // RunTime.larray_get(x,y)
    public case FARRAY_GET      { tag = 78; } // RunTime.farray_get(x,y)
    public case DARRAY_GET      { tag = 79; } // RunTime.darray_get(x,y)
    public case OARRAY_GET      { tag = 80; } // RunTime.oarray_get(x,y)
    public case ZARRAY_SET      { tag = 81; } // RunTime.zarray(x,y,z)
    public case BARRAY_SET      { tag = 82; } // RunTime.barray(x,y,z)
    public case SARRAY_SET      { tag = 83; } // RunTime.sarray(x,y,z)
    public case CARRAY_SET      { tag = 84; } // RunTime.carray(x,y,z)
    public case IARRAY_SET      { tag = 85; } // RunTime.iarray(x,y,z)
    public case LARRAY_SET      { tag = 86; } // RunTime.larray(x,y,z)
    public case FARRAY_SET      { tag = 87; } // RunTime.farray(x,y,z)
    public case DARRAY_SET      { tag = 88; } // RunTime.darray(x,y,z)
    public case OARRAY_SET      { tag = 89; } // RunTime.oarray(x,y,z)

    public case B2B             { tag =100; } // RunTime.b2b(x)
    public case B2S             { tag =101; } // RunTime.b2s(x)
    public case B2C             { tag =102; } // RunTime.b2c(x)
    public case B2I             { tag =103; } // RunTime.b2i(x)
    public case B2L             { tag =104; } // RunTime.b2l(x)
    public case B2F             { tag =105; } // RunTime.b2f(x)
    public case B2D             { tag =106; } // RunTime.b2d(x)
    public case S2B             { tag =107; } // RunTime.s2b(x)
    public case S2S             { tag =108; } // RunTime.s2s(x)
    public case S2C             { tag =109; } // RunTime.s2c(x)
    public case S2I             { tag =110; } // RunTime.s2i(x)
    public case S2L             { tag =111; } // RunTime.s2l(x)
    public case S2F             { tag =112; } // RunTime.s2f(x)
    public case S2D             { tag =113; } // RunTime.s2d(x)
    public case C2B             { tag =114; } // RunTime.c2b(x)
    public case C2S             { tag =115; } // RunTime.c2s(x)
    public case C2C             { tag =116; } // RunTime.c2c(x)
    public case C2I             { tag =117; } // RunTime.c2i(x)
    public case C2L             { tag =118; } // RunTime.c2l(x)
    public case C2F             { tag =119; } // RunTime.c2f(x)
    public case C2D             { tag =120; } // RunTime.c2d(x)
    public case I2B             { tag =121; } // RunTime.i2b(x)
    public case I2S             { tag =122; } // RunTime.i2s(x)
    public case I2C             { tag =123; } // RunTime.i2c(x)
    public case I2I             { tag =124; } // RunTime.i2i(x)
    public case I2L             { tag =125; } // RunTime.i2l(x)
    public case I2F             { tag =126; } // RunTime.i2f(x)
    public case I2D             { tag =127; } // RunTime.i2d(x)
    public case L2B             { tag =128; } // RunTime.l2b(x)
    public case L2S             { tag =129; } // RunTime.l2s(x)
    public case L2C             { tag =130; } // RunTime.l2c(x)
    public case L2I             { tag =131; } // RunTime.l2i(x)
    public case L2L             { tag =132; } // RunTime.l2l(x)
    public case L2F             { tag =133; } // RunTime.l2f(x)
    public case L2D             { tag =134; } // RunTime.l2d(x)
    public case F2B             { tag =135; } // RunTime.f2b(x)
    public case F2S             { tag =136; } // RunTime.f2s(x)
    public case F2C             { tag =137; } // RunTime.f2c(x)
    public case F2I             { tag =138; } // RunTime.f2i(x)
    public case F2L             { tag =139; } // RunTime.f2l(x)
    public case F2F             { tag =140; } // RunTime.f2f(x)
    public case F2D             { tag =141; } // RunTime.f2d(x)
    public case D2B             { tag =142; } // RunTime.d2b(x)
    public case D2S             { tag =143; } // RunTime.d2s(x)
    public case D2C             { tag =144; } // RunTime.d2c(x)
    public case D2I             { tag =145; } // RunTime.d2i(x)
    public case D2L             { tag =146; } // RunTime.d2l(x)
    public case D2F             { tag =147; } // RunTime.d2f(x)
    public case D2D             { tag =148; } // RunTime.d2d(x)

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
