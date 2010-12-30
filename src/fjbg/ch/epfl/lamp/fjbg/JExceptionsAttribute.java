/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * Exceptions attribute

 * This table is used by compilers to indicate which Exceptions a method
 * is declared to throw. See section 2.6.4 of the JVM specification.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */

public class JExceptionsAttribute extends JAttribute {
    /** Constant pool of the current classfile. */
    private JConstantPool pool;

    protected int[] indexTable;
    protected int count;

    public JExceptionsAttribute(FJBGContext context,
                                JClass clazz,
                                JMethod owner) {
        super(context, clazz);
        this.pool = clazz.pool;

        this.count = 0;
        this.indexTable = new int[8]; // some size > count

        assert clazz == owner.getOwner();
    }

    public JExceptionsAttribute(FJBGContext context,
                                JClass clazz,
                                Object owner, //JMethod
                                String name,
                                int size,
                                DataInputStream stream)
        throws IOException {
        super(context, clazz, name);
        this.pool = clazz.pool;

        this.count = stream.readShort();
        this.indexTable = new int[count];
        for (int i = 0; i < count; ++i)
            indexTable[i] = stream.readShort();

        assert name.equals(getName());
    }

    public void addEntry(int classIndex) {
        if (count >= indexTable.length) {
            int[] newIT = new int[indexTable.length * 2];
            System.arraycopy(indexTable, 0, newIT, 0, indexTable.length);
            indexTable = newIT;
        }
        indexTable[count++] = classIndex;
    }

    public String getName() { return "Exceptions"; }

    // Follows javap output format for Exceptions attribute.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer("  Exceptions: ");
        for (int i = 0; i < indexTable.length; ++i) {
            buf.append("\n   throws ");
            buf.append(JClass.toExternalName(pool.lookupClass(indexTable[i])));
        }
        buf.append("\n");
        return buf.toString();
    }

    protected int getSize() {
        return 2 + indexTable.length * 2;
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(count);
        for (int i = 0; i < count; ++i)
            stream.writeShort(indexTable[i]);
    }
}
