/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * EclosingMethod attribute

 * A class must have an EnclosingMethod attribute if and only if it is a
 * local class or an anonymous class. A class may have no more than one
 * EnclosingMethod attribute. See section 4.8.6 of the JVM specification.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JEnclosingMethodAttribute extends JAttribute {
    /** Constant pool of the current classfile. */
    private JConstantPool pool;

    protected final int classIdx;
    protected final int nameAndTypeIdx;

    public JEnclosingMethodAttribute(FJBGContext context,
                                     JClass clazz,
                                     String className,
                                     String methodName,
                                     JType methodType) {
        super(context, clazz);
        this.pool = clazz.pool;

        this.classIdx = pool.addClass(className);
        this.nameAndTypeIdx = pool.addNameAndType(methodName, methodType.getSignature());
    }

    public JEnclosingMethodAttribute(FJBGContext context,
                                     JClass clazz,
                                     Object owner,
                                     String name,
                                     int size,
                                     DataInputStream stream)
        throws IOException {
        super(context, clazz, name);
        this.pool = clazz.pool;

        this.classIdx = stream.readShort();
        this.nameAndTypeIdx = stream.readShort();

        assert name.equals(getName());
    }

    public String getName() { return "EnclosingMethod"; }

    // Follows javap output format for EnclosingMethod attribute.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer("  EnclosingMethod:");
        buf.append("\n   #");
        buf.append(classIdx);
        if (nameAndTypeIdx != 0) {
            buf.append(" of #");
            buf.append(nameAndTypeIdx);
        }
        buf.append(";\t//  ");
        buf.append(pool.lookupEntry(classIdx));
        buf.append("\n");
        return buf.toString();
    }

    protected int getSize() {
        return 4; // 2 * Short.SIZE
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(classIdx);
        stream.writeShort(nameAndTypeIdx);
    }
}
