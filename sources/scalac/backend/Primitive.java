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

    // Non-primitive operations
    public case NOT_A_PRIMITIVE;              // not a primitive

    // Arithmetic unary operations
    public case POS;                          // +x
    public case NEG;                          // -x
    public case NOT;                          // ~x

    // Arithmetic binary operations
    public case ADD;                          // x + y
    public case SUB;                          // x - y
    public case MUL;                          // x * y
    public case DIV;                          // x / y
    public case MOD;                          // x % y

    // Bitwise operations
    public case OR;                           // x | y
    public case XOR;                          // x ^ y
    public case AND;                          // x & y

    // Shift operations
    public case LSL;                          // x << y
    public case LSR;                          // x >>> y
    public case ASR;                          // x >> y

    // Comparison operations
    public case ID;                           // x eq y
    public case NI;                           // x ne y
    public case EQ;                           // x == y
    public case NE;                           // x != y
    public case LT;                           // x < y
    public case LE;                           // x <= y
    public case GE;                           // x > y
    public case GT;                           // x >= y

    // Boolean unary operations
    public case ZNOT;                         // !x

    // Boolean binary operations
    public case ZOR;                          // x || y
    public case ZAND;                         // x && y

    // Array operations
    public case LENGTH;                       // x.length
    public case APPLY;                        // x(y)
    public case UPDATE;                       // x(y) = z

    // Any operations
    public case IS;                           // x.is[y]
    public case AS;                           // x.as[y]
    public case EQUALS;                       // x.equals(y)
    public case HASHCODE;                     // x.hashcode()
    public case TOSTRING;                     // x.toString()

    // AnyRef operations
    public case SYNCHRONIZED;                 // x.synchronized(y)

    // String operations
    public case CONCAT;                       // String.valueOf(x)+String.valueOf(y)

    // Throwable operations
    public case THROW;                        // throw x

    // Value types conversions
    public case COERCE;                       // x.coerce()

    // RunTime operations
    public case BOX;                          // RunTime.box_<X>(x)
    public case UNBOX;                        // RunTime.unbox_<X>(x)
    public case NEW_ZARRAY;                   // RunTime.zarray(x)
    public case NEW_BARRAY;                   // RunTime.barray(x)
    public case NEW_SARRAY;                   // RunTime.sarray(x)
    public case NEW_CARRAY;                   // RunTime.carray(x)
    public case NEW_IARRAY;                   // RunTime.iarray(x)
    public case NEW_LARRAY;                   // RunTime.larray(x)
    public case NEW_FARRAY;                   // RunTime.farray(x)
    public case NEW_DARRAY;                   // RunTime.darray(x)
    public case NEW_OARRAY;                   // RunTime.oarray(x)
    public case ZARRAY_LENGTH;                // RunTime.zarray_length(x)
    public case BARRAY_LENGTH;                // RunTime.barray_length(x)
    public case SARRAY_LENGTH;                // RunTime.sarray_length(x)
    public case CARRAY_LENGTH;                // RunTime.carray_length(x)
    public case IARRAY_LENGTH;                // RunTime.iarray_length(x)
    public case LARRAY_LENGTH;                // RunTime.larray_length(x)
    public case FARRAY_LENGTH;                // RunTime.farray_length(x)
    public case DARRAY_LENGTH;                // RunTime.darray_length(x)
    public case OARRAY_LENGTH;                // RunTime.oarray_length(x)
    public case ZARRAY_GET;                   // RunTime.zarray_get(x,y)
    public case BARRAY_GET;                   // RunTime.barray_get(x,y)
    public case SARRAY_GET;                   // RunTime.sarray_get(x,y)
    public case CARRAY_GET;                   // RunTime.carray_get(x,y)
    public case IARRAY_GET;                   // RunTime.iarray_get(x,y)
    public case LARRAY_GET;                   // RunTime.larray_get(x,y)
    public case FARRAY_GET;                   // RunTime.farray_get(x,y)
    public case DARRAY_GET;                   // RunTime.darray_get(x,y)
    public case OARRAY_GET;                   // RunTime.oarray_get(x,y)
    public case ZARRAY_SET;                   // RunTime.zarray(x,y,z)
    public case BARRAY_SET;                   // RunTime.barray(x,y,z)
    public case SARRAY_SET;                   // RunTime.sarray(x,y,z)
    public case CARRAY_SET;                   // RunTime.carray(x,y,z)
    public case IARRAY_SET;                   // RunTime.iarray(x,y,z)
    public case LARRAY_SET;                   // RunTime.larray(x,y,z)
    public case FARRAY_SET;                   // RunTime.farray(x,y,z)
    public case DARRAY_SET;                   // RunTime.darray(x,y,z)
    public case OARRAY_SET;                   // RunTime.oarray(x,y,z)

    public case B2B;                          // RunTime.b2b(x)
    public case B2S;                          // RunTime.b2s(x)
    public case B2C;                          // RunTime.b2c(x)
    public case B2I;                          // RunTime.b2i(x)
    public case B2L;                          // RunTime.b2l(x)
    public case B2F;                          // RunTime.b2f(x)
    public case B2D;                          // RunTime.b2d(x)
    public case S2B;                          // RunTime.s2b(x)
    public case S2S;                          // RunTime.s2s(x)
    public case S2C;                          // RunTime.s2c(x)
    public case S2I;                          // RunTime.s2i(x)
    public case S2L;                          // RunTime.s2l(x)
    public case S2F;                          // RunTime.s2f(x)
    public case S2D;                          // RunTime.s2d(x)
    public case C2B;                          // RunTime.c2b(x)
    public case C2S;                          // RunTime.c2s(x)
    public case C2C;                          // RunTime.c2c(x)
    public case C2I;                          // RunTime.c2i(x)
    public case C2L;                          // RunTime.c2l(x)
    public case C2F;                          // RunTime.c2f(x)
    public case C2D;                          // RunTime.c2d(x)
    public case I2B;                          // RunTime.i2b(x)
    public case I2S;                          // RunTime.i2s(x)
    public case I2C;                          // RunTime.i2c(x)
    public case I2I;                          // RunTime.i2i(x)
    public case I2L;                          // RunTime.i2l(x)
    public case I2F;                          // RunTime.i2f(x)
    public case I2D;                          // RunTime.i2d(x)
    public case L2B;                          // RunTime.l2b(x)
    public case L2S;                          // RunTime.l2s(x)
    public case L2C;                          // RunTime.l2c(x)
    public case L2I;                          // RunTime.l2i(x)
    public case L2L;                          // RunTime.l2l(x)
    public case L2F;                          // RunTime.l2f(x)
    public case L2D;                          // RunTime.l2d(x)
    public case F2B;                          // RunTime.f2b(x)
    public case F2S;                          // RunTime.f2s(x)
    public case F2C;                          // RunTime.f2c(x)
    public case F2I;                          // RunTime.f2i(x)
    public case F2L;                          // RunTime.f2l(x)
    public case F2F;                          // RunTime.f2f(x)
    public case F2D;                          // RunTime.f2d(x)
    public case D2B;                          // RunTime.d2b(x)
    public case D2S;                          // RunTime.d2s(x)
    public case D2C;                          // RunTime.d2c(x)
    public case D2I;                          // RunTime.d2i(x)
    public case D2L;                          // RunTime.d2l(x)
    public case D2F;                          // RunTime.d2f(x)
    public case D2D;                          // RunTime.d2d(x)

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
