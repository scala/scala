/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

/**
 * Type for Java methods. These types do not really exist in Java, but
 * are provided here because they are useful in several places.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JMethodType extends JType {
    protected final JType returnType;
    protected final JType[] argTypes;
    protected String signature = null;

    public final static JMethodType ARGLESS_VOID_FUNCTION =
        new JMethodType(JType.VOID, JType.EMPTY_ARRAY);

    public JMethodType(JType returnType, JType[] argTypes) {
        this.returnType = returnType;
        this.argTypes = argTypes;
    }

    public JType getReturnType() { return returnType; }
    public JType[] getArgumentTypes() { return argTypes; }

    public int getSize() {
        throw new UnsupportedOperationException();
    }

    public String getSignature() {
        if (signature == null) {
            StringBuffer buf = new StringBuffer();
            buf.append('(');
            for (int i = 0; i < argTypes.length; ++i)
                buf.append(argTypes[i].getSignature());
            buf.append(')');
            buf.append(returnType.getSignature());
            signature = buf.toString();
        }
        return signature;
    }

    public int getTag() { return T_UNKNOWN; }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append('(');
        for (int i = 0; i < argTypes.length; ++i)
            buf.append(argTypes[i].toString());
        buf.append(')');
        buf.append(returnType.toString());
        return buf.toString();
    }

    public int getArgsSize() {
        int size = 0;
        for (int i = 0; i < argTypes.length; ++i)
            size += argTypes[i].getSize();
        return size;
    }

    public int getProducedStack() {
        return returnType.getSize() - getArgsSize();
    }

    public boolean isCompatibleWith(JType other) {
        return false;
    }
    public boolean equals(Object o) {
        if (o instanceof JMethodType)
            return ((JMethodType)o).getSignature().equals(this.getSignature());
        else
            return false;
    }
    public int hashCode() {
        if (signature == null)
            return 0;
        else
            return signature.hashCode();
    }
}
