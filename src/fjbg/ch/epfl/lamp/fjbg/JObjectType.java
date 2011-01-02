/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

/**
 * Types for Java objects.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JObjectType extends JReferenceType {
    protected final String name;
    protected String signature = null;

    public final static JObjectType JAVA_LANG_OBJECT =
        new JObjectType("java.lang.Object");
    public final static JObjectType JAVA_LANG_STRING =
        new JObjectType("java.lang.String");
    public final static JObjectType CLONEABLE =
        new JObjectType("Cloneable");
    public final static JObjectType JAVA_IO_SERIALIZABLE =
        new JObjectType("java.io.Serializable");

    public JObjectType(String name) {
        this.name = name;
    }

    public int getSize() { return 1; }

    public String getName() { return name; }

    public String getSignature() {
        if (signature == null)
            signature = "L" + name.replace('.','/') + ";";
        return signature;
    }

    public String getDescriptor() {
        return name.replace('.','/');
    }

    public int getTag() { return T_OBJECT; }

    public String toString() { return name; }

    public boolean isObjectType() { return true; }

    public boolean isCompatibleWith(JType other) {
        return other instanceof JObjectType
            || other == JType.REFERENCE;
    }
    public boolean equals(Object o) {
        if (o instanceof JObjectType)
            return ((JObjectType)o).getSignature().equals(this.getSignature());
        else
            return false;
    }
    public int hashCode() {
        return name.hashCode();
    }
}
