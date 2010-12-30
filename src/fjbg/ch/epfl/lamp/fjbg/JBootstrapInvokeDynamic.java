/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Iterator;

/**
 * BootstrapInvokeDynamic entry, as described by JSR 292 (invoke dynamic)
 *
 * @author Iulian Dragos
 * @version 1.0
 *
 */
public class JBootstrapInvokeDynamic extends JAttribute {
    /** Constant pool of the current classfile. */
    private JConstantPool pool;

    protected final int classIdx;

    public JBootstrapInvokeDynamic(FJBGContext context,
                                   JClass clazz,
                                   String className) {
        super(context, clazz);
        this.pool = clazz.pool;

        this.classIdx = pool.addClass(className);
    }

    public JBootstrapInvokeDynamic(FJBGContext context,
                                   JClass clazz,
                                   Object owner,
                                   String name,
                                   int size,
                                   DataInputStream stream)
        throws IOException {
        super(context, clazz, name);

        this.classIdx = stream.readShort();

        assert name.equals(getName());
    }

    public String getName() { return "BootstrapInvokeDynamic"; }

    // Follows javap output format for BootstrapInvokeDynamic attribute.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer("  BootstrapInvokeDynamic:");
        buf.append("\n   #");
        buf.append(classIdx);
        buf.append("; // class ");
        buf.append(pool.lookupClass(classIdx));
        buf.append("\n");
        return buf.toString();
    }

    protected int getSize() {
        return 2;  // Short.SIZE
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(classIdx);
    }
}
