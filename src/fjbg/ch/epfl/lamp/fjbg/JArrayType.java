/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

/**
 * Types for Java arrays.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JArrayType extends JReferenceType {
    protected final JType elementType;
    protected String signature = null;

    public JArrayType(JType elementType) {
        this.elementType = elementType;
    }

    public int getSize() { return 1; }

    public String getSignature() {
        if (signature == null)
            signature = "[" + elementType.getSignature();
        return signature;
    }

    public String getDescriptor() {
        return getSignature();
    }

    public int getTag() { return T_ARRAY; }

    public JType getElementType() { return elementType; }

    public String toString() {
        return elementType.toString() + "[]";
    }

    public boolean isArrayType() { return true; }

    public boolean isCompatibleWith(JType other) {
        if (other instanceof JObjectType)
            return (JObjectType)other == JObjectType.JAVA_LANG_OBJECT;
        else if (other instanceof JArrayType)
            return elementType.isCompatibleWith(((JArrayType)other).elementType);
        else return other == JType.REFERENCE;
    }

    public static JArrayType BOOLEAN   = new JArrayType(JType.BOOLEAN);
    public static JArrayType BYTE      = new JArrayType(JType.BYTE);
    public static JArrayType CHAR      = new JArrayType(JType.CHAR);
    public static JArrayType SHORT     = new JArrayType(JType.SHORT);
    public static JArrayType INT       = new JArrayType(JType.INT);
    public static JArrayType FLOAT     = new JArrayType(JType.FLOAT);
    public static JArrayType LONG      = new JArrayType(JType.LONG);
    public static JArrayType DOUBLE    = new JArrayType(JType.DOUBLE);
    public static JArrayType REFERENCE = new JArrayType(JType.REFERENCE);
}
