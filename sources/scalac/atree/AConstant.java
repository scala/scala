/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import scalac.util.Debug;

/** This class represents a constant. */
public class AConstant {

    //########################################################################
    // Public Cases

    public case UNIT;
    public case BOOLEAN(boolean value);
    public case BYTE(byte value);
    public case SHORT(short value);
    public case CHAR(char value);
    public case INT(int value);
    public case LONG(long value);
    public case FLOAT(float value);
    public case DOUBLE(double value);
    public case STRING(String value);
    public case NULL;
    public case ZERO;

    //########################################################################
    // Public Methods

    /** Returns the type kind of this constant. */
    public ATypeKind kind() {
        switch (this) {
        case UNIT:
            return ATypeKind.UNIT;
        case BOOLEAN(_):
            return ATypeKind.BOOL;
        case BYTE(_):
            return ATypeKind.I1;
        case SHORT(_):
            return ATypeKind.I2;
        case CHAR(_):
            return ATypeKind.U2;
        case INT(_):
            return ATypeKind.I4;
        case LONG(_):
            return ATypeKind.I8;
        case FLOAT(_):
            return ATypeKind.R4;
        case DOUBLE(_):
            return ATypeKind.R8;
        case STRING(_):
            return ATypeKind.STR;
        case NULL:
            return ATypeKind.NULL;
        case ZERO:
            return ATypeKind.ZERO;
        default:
            throw Debug.abort("unknown case", this);
        }
    }


    /** Converts this constant to a boolean value. */
    public boolean booleanValue() {
        switch (this) {
        case BOOLEAN(boolean value):
            return value;
        default:
            throw Debug.abort("not convertible to boolean", this);
        }
    }

    /** Converts this constant to a byte value. */
    public byte byteValue() {
        switch (this) {
        case BYTE(byte value):
            return (byte)value;
        case SHORT(short value):
            return (byte)value;
        case CHAR(char value):
            return (byte)value;
        case INT(int value):
            return (byte)value;
        case LONG(long value):
            return (byte)value;
        case FLOAT(float value):
            return (byte)value;
        case DOUBLE(double value):
            return (byte)value;
        default:
            throw Debug.abort("not convertible to byte", this);
        }
    }

    /** Converts this constant to a short value. */
    public short shortValue() {
        switch (this) {
        case BYTE(byte value):
            return (short)value;
        case SHORT(short value):
            return (short)value;
        case CHAR(char value):
            return (short)value;
        case INT(int value):
            return (short)value;
        case LONG(long value):
            return (short)value;
        case FLOAT(float value):
            return (short)value;
        case DOUBLE(double value):
            return (short)value;
        default:
            throw Debug.abort("not convertible to short", this);
        }
    }

    /** Converts this constant to a char value. */
    public char charValue() {
        switch (this) {
        case BYTE(byte value):
            return (char)value;
        case SHORT(short value):
            return (char)value;
        case CHAR(char value):
            return (char)value;
        case INT(int value):
            return (char)value;
        case LONG(long value):
            return (char)value;
        case FLOAT(float value):
            return (char)value;
        case DOUBLE(double value):
            return (char)value;
        default:
            throw Debug.abort("not convertible to char", this);
        }
    }

    /** Converts this constant to a int value. */
    public int intValue() {
        switch (this) {
        case BYTE(byte value):
            return (int)value;
        case SHORT(short value):
            return (int)value;
        case CHAR(char value):
            return (int)value;
        case INT(int value):
            return (int)value;
        case LONG(long value):
            return (int)value;
        case FLOAT(float value):
            return (int)value;
        case DOUBLE(double value):
            return (int)value;
        default:
            throw Debug.abort("not convertible to int", this);
        }
    }

    /** Converts this constant to a long value. */
    public long longValue() {
        switch (this) {
        case BYTE(byte value):
            return (long)value;
        case SHORT(short value):
            return (long)value;
        case CHAR(char value):
            return (long)value;
        case INT(int value):
            return (long)value;
        case LONG(long value):
            return (long)value;
        case FLOAT(float value):
            return (long)value;
        case DOUBLE(double value):
            return (long)value;
        default:
            throw Debug.abort("not convertible to long", this);
        }
    }

    /** Converts this constant to a float value. */
    public float floatValue() {
        switch (this) {
        case BYTE(byte value):
            return (float)value;
        case SHORT(short value):
            return (float)value;
        case CHAR(char value):
            return (float)value;
        case INT(int value):
            return (float)value;
        case LONG(long value):
            return (float)value;
        case FLOAT(float value):
            return (float)value;
        case DOUBLE(double value):
            return (float)value;
        default:
            throw Debug.abort("not convertible to float", this);
        }
    }

    /** Converts this constant to a double value. */
    public double doubleValue() {
        switch (this) {
        case BYTE(byte value):
            return (double)value;
        case SHORT(short value):
            return (double)value;
        case CHAR(char value):
            return (double)value;
        case INT(int value):
            return (double)value;
        case LONG(long value):
            return (double)value;
        case FLOAT(float value):
            return (double)value;
        case DOUBLE(double value):
            return (double)value;
        default:
            throw Debug.abort("not convertible to double", this);
        }
    }

    /** Converts this constant to a String value. */
    public String stringValue() {
        switch (this) {
        case UNIT:
            return "()";
        case BOOLEAN(boolean value):
            return String.valueOf(value);
        case BYTE(byte value):
            return String.valueOf(value);
        case SHORT(short value):
            return String.valueOf(value);
        case CHAR(char value):
            return String.valueOf(value);
        case INT(int value):
            return String.valueOf(value);
        case LONG(long value):
            return String.valueOf(value);
        case FLOAT(float value):
            return String.valueOf(value);
        case DOUBLE(double value):
            return String.valueOf(value);
        case STRING(String value):
            return value;
        case NULL:
            return String.valueOf(null);
        default:
            throw Debug.abort("not convertible to String", this);
        }
    }

    /** Tests whether this constant equals given Object. */
    public boolean equals(Object that) {
        if (this == that) return true;
        return (that instanceof AConstant) && this.equals((AConstant)that);
    }

    /** Tests whether this constant equals given one. */
    public boolean equals(AConstant that) {
        if (this == that) return true;
        if (null == that) return false;
        switch (this) {
        case UNIT:
            return false;
        case BOOLEAN(boolean ivalue):
            switch (that) {
            case BOOLEAN(boolean avalue):
                return ivalue == avalue;
            default:
                return false;
            }
        case BYTE(byte ivalue):
            switch (that) {
            case BYTE(byte avalue):
                return ivalue == avalue;
            default:
                return false;
            }
        case SHORT(short ivalue):
            switch (that) {
            case SHORT(short avalue):
                return ivalue == avalue;
            default:
                return false;
            }
        case CHAR(char ivalue):
            switch (that) {
            case CHAR(char avalue):
                return ivalue == avalue;
            default:
                return false;
            }
        case INT(int ivalue):
            switch (that) {
            case INT(int avalue):
                return ivalue == avalue;
            default:
                return false;
            }
        case LONG(long ivalue):
            switch (that) {
            case LONG(long avalue):
                return ivalue == avalue;
            default:
                return false;
            }
        case FLOAT(float ivalue):
            switch (that) {
            case FLOAT(float avalue):
                return ivalue == avalue;
            default:
                return false;
            }
        case DOUBLE(double ivalue):
            switch (that) {
            case DOUBLE(double avalue):
                return ivalue == avalue;
            default:
                return false;
            }
        case STRING(String ivalue):
            switch (that) {
            case STRING(String avalue):
                return ivalue.equals(avalue);
            default:
                return false;
            }
        case NULL:
            return false;
        case ZERO:
            return false;
        default:
            throw Debug.abort("unknown case", this);
        }
    }

    /** Returns the hash code of this constant. */
    public int hashCode() {
        switch (this) {
        case UNIT:
            return 4041;
        case BOOLEAN(boolean value):
            return value ? 1231 : 1237;
        case BYTE(byte value):
            return value;
        case SHORT(short value):
            return value;
        case CHAR(char value):
            return value;
        case INT(int value):
            return value;
        case LONG(long value):
            long bits = value;
            return (int)(bits ^ (bits >>> 32));
        case FLOAT(float value):
            int bits = java.lang.Float.floatToIntBits(value);
            return bits;
        case DOUBLE(double value):
            long bits = java.lang.Double.doubleToLongBits(value);
            return (int)(bits ^ (bits >>> 32));
        case STRING(String value):
            return value.hashCode();
        case NULL:
            return 0;
        case ZERO:
            return 0;
        default:
            throw Debug.abort("unknown case", this);
        }
    }

    /** Returns a string representation of this constant. */
    public String toString() {
        return new ATreePrinter().printConstant(this).toString();
    }

    //########################################################################
}
