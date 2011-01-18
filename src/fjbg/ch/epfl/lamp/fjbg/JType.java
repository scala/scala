/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;

/**
 * Representation of Java types.
 *
 * @version 1.0
 * @author Michel Schinz
 */

abstract public class JType {
    abstract public int getSize();
    abstract public String getSignature();
    abstract public int getTag();
    abstract public String toString();
    abstract public boolean isCompatibleWith(JType other);

    public boolean isValueType() { return false; }
    public boolean isObjectType() { return false; }
    public boolean isArrayType() { return false; }
    public boolean isReferenceType() { return false; }

    // Tags for types. Taken from BCEL.
    public static final int T_BOOLEAN = 4;
    public static final int T_CHAR    = 5;
    public static final int T_FLOAT   = 6;
    public static final int T_DOUBLE  = 7;
    public static final int T_BYTE    = 8;
    public static final int T_SHORT   = 9;
    public static final int T_INT     = 10;
    public static final int T_LONG    = 11;
    public static final int T_VOID    = 12; // Non-standard
    public static final int T_ARRAY   = 13;
    public static final int T_OBJECT  = 14;
    public static final int T_UNKNOWN = 15;
    public static final int T_ADDRESS = 16;

    public static final int T_REFERENCE = 17; // type compatible with references

    public static final JType[] EMPTY_ARRAY = new JType[0];

    protected static JType parseSig(StringReader s) throws IOException {
        int nextChar = s.read();
        if (nextChar == -1) throw new IllegalArgumentException();

        switch ((char)nextChar) {
        case 'V' : return VOID;
        case 'Z' : return BOOLEAN;
        case 'B' : return BYTE;
        case 'C' : return CHAR;
        case 'S' : return SHORT;
        case 'I' : return INT;
        case 'F' : return FLOAT;
        case 'J' : return LONG;
        case 'D' : return DOUBLE;
        case 'L': {
            StringBuffer className = new StringBuffer();
            for (;;) {
                nextChar = s.read();
                if (nextChar == -1 || nextChar == ';') break;
                className.append(nextChar == '/' ? ':' : ((char)nextChar));
            }
            if (nextChar != ';') throw new IllegalArgumentException();
            return new JObjectType(className.toString());
        }
        case '[': {
            JType elemType = parseSig(s);
            return new JArrayType(elemType);
        }
        case '(': {
            ArrayList argTps = new ArrayList();
            for (;;) {
                s.mark(1);
                nextChar = s.read();
                if (nextChar == -1 || nextChar == ')') break;
                s.reset();
                argTps.add(parseSig(s));
            }
            if (nextChar != ')') throw new IllegalArgumentException("a");
            JType[] argTpsA = (JType[])argTps.toArray(new JType[argTps.size()]);
            JType returnType = parseSig(s);
            return new JMethodType(returnType, argTpsA);
        }
        default:
            throw new IllegalArgumentException();
        }
    }

    /**
     * A signature is a string representing the generic type of a field or
     * method, or generic type information for a class declaration.
     * See section 4.4.4 of the JVM specification.
     */
    public static JType parseSignature(String signature) {
        try {
            StringReader sigReader = new StringReader(signature);
            JType parsed = parseSig(sigReader);
            if (sigReader.read() != -1)
                throw new IllegalArgumentException();
            return parsed;
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("invalid signature " + signature);
        } catch (IOException e) {
            throw new Error(e);
        }
    }

    public static int getTotalSize(JType[] types) {
        int size = 0;
        for (int i = 0; i < types.length; ++i)
            size += types[i].getSize();
        return size;
    }

    protected JType() {}

    public static JType VOID = new JType() {
        public int getSize() { return 0; }
        public String getSignature() { return "V"; }
        public int getTag() { return T_VOID; }
        public String toString() { return "void"; }
        public boolean isCompatibleWith(JType other) {
            throw new UnsupportedOperationException("type VOID is no real "
                                                    + "data type therefore "
                                                    + "cannot be assigned to "
                                                    + other.toString());
        }
    };

    public static JType BOOLEAN = new JType() {
        public int getSize() { return 1; }
        public String getSignature() { return "Z"; }
        public int getTag() { return T_BOOLEAN; }
        public String toString() { return "boolean"; }
        public boolean isValueType() { return true; }
        public boolean isCompatibleWith(JType other) {
            return other == BOOLEAN
                || other == INT
                || other == BYTE
                || other == CHAR
                || other == SHORT;
        }
    };

    public static JType BYTE = new JType() {
        public int getSize() { return 1; }
        public String getSignature() { return "B"; }
        public int getTag() { return T_BYTE; }
        public String toString() { return "byte"; }
        public boolean isValueType() { return true; }
        public boolean isCompatibleWith(JType other) {
            return other == BOOLEAN
                || other == INT
                || other == BYTE
                || other == CHAR
                || other == SHORT;
        }
    };

    public static JType CHAR = new JType() {
        public int getSize() { return 1; }
        public String getSignature() { return "C"; }
        public int getTag() { return T_CHAR; }
        public String toString() { return "char"; }
        public boolean isValueType() { return true; }
        public boolean isCompatibleWith(JType other) {
            return other == BOOLEAN
                || other == INT
                || other == BYTE
                || other == CHAR
                || other == SHORT;
        }
    };

    public static JType SHORT = new JType() {
        public int getSize() { return 1; }
        public String getSignature() { return "S"; }
        public int getTag() { return T_SHORT; }
        public String toString() { return "short"; }
        public boolean isValueType() { return true; }
        public boolean isCompatibleWith(JType other) {
            return other == BOOLEAN
                || other == INT
                || other == BYTE
                || other == CHAR
                || other == SHORT;
        }
    };

    public static JType INT = new JType() {
        public int getSize() { return 1; }
        public String getSignature() { return "I"; }
        public int getTag() { return T_INT; }
        public String toString() { return "int"; }
        public boolean isValueType() { return true; }
        public boolean isCompatibleWith(JType other) {
            return other == BOOLEAN
                || other == INT
                || other == BYTE
                || other == CHAR
                || other == SHORT;
        }
    };

    public static JType FLOAT = new JType() {
        public int getSize() { return 1; }
        public String getSignature() { return "F"; }
        public int getTag() { return T_FLOAT; }
        public String toString() { return "float"; }
        public boolean isValueType() { return true; }
        public boolean isCompatibleWith(JType other) {
            return other == FLOAT;
        }
    };

    public static JType LONG = new JType() {
            public int getSize() { return 2; }
            public String getSignature() { return "J"; }
            public int getTag() { return T_LONG; }
            public String toString() { return "long"; }
            public boolean isValueType() { return true; }
            public boolean isCompatibleWith(JType other) {
                return other == LONG;
            }
        };

    public static JType DOUBLE = new JType() {
            public int getSize() { return 2; }
            public String getSignature() { return "D"; }
            public int getTag() { return T_DOUBLE; }
            public String toString() { return "double"; }
            public boolean isValueType() { return true; }
            public boolean isCompatibleWith(JType other) {
                return other == DOUBLE;
            }
        };

    public static JType REFERENCE = new JType() {
            public int getSize() { return 1; }
            public String getSignature() {
                throw new UnsupportedOperationException("type REFERENCE is no real "
                                                        + "data type and therefore "
                                                        + "has no signature");
            }
            public int getTag() { return T_REFERENCE; }
            public String toString() { return "<reference>"; }
            public boolean isCompatibleWith(JType other) {
                throw new UnsupportedOperationException("type REFERENCE is no real "
                                                        + "data type and therefore "
                                                        + "cannot be assigned to "
                                                        + other.toString());
            }
        };

    public static JType ADDRESS = new JType() {
        public int getSize() { return 1; }
        public String getSignature() {
            throw new UnsupportedOperationException("type ADDRESS is no usable "
                                                    + "data type and therefore "
                                                    + "has no signature");
        }
        public int getTag() { return T_ADDRESS; }
        public String toString() { return "<address>"; }
        public boolean isCompatibleWith(JType other) {
            return other == ADDRESS;
        }
    };

    public static JType UNKNOWN = new JType() {
        public int getSize() {
            throw new UnsupportedOperationException("type UNKNOWN is no real "
                                                    + "data type and therefore "
                                                    + "has no size");
        }
        public String getSignature() {
            throw new UnsupportedOperationException("type UNKNOWN is no real "
                                                    + "data type and therefore "
                                                    + "has no signature");
        }
        public int getTag() { return T_UNKNOWN; }
        public String toString() { return "<unknown>"; }
        public boolean isCompatibleWith(JType other) {
            throw new UnsupportedOperationException("type UNKNOWN is no real "
                                                    + "data type and therefore "
                                                    + "cannot be assigned to "
                                                    + other.toString());
        }
    };

    protected static String tagToString(int tag) {
        switch (tag) {
        case T_BOOLEAN : return "boolean";
        case T_CHAR    : return "char";
        case T_FLOAT   : return "float";
        case T_DOUBLE  : return "double";
        case T_BYTE    : return "byte";
        case T_SHORT   : return "short";
        case T_INT     : return "int";
        case T_LONG    : return "long";
        case T_VOID    : return "void"; // Non-standard
        case T_ARRAY   : return "[]";
        case T_OBJECT  : return "Object";
        case T_UNKNOWN : return "<unknown>";
        case T_ADDRESS : return "<address>";
        default:         return String.valueOf(tag);
        }
    }
}
