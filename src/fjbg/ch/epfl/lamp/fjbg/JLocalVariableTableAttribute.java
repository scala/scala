/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;

import ch.epfl.lamp.fjbg.JConstantPool.*;

/**
 * Attribute storing local variables.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */

public class JLocalVariableTableAttribute extends JAttribute {
    /** Constant pool of the current classfile. */
    private JConstantPool pool;

    protected final LinkedList/*<Entry>*/ entries = new LinkedList();
    protected int localVariableIndex = 0;

    public JLocalVariableTableAttribute(FJBGContext context,
                                        JClass clazz,
                                        JCode code) {
        super(context, clazz);
        this.pool = clazz.pool;

        assert code.getOwner().getOwner() == clazz;
    }

    public JLocalVariableTableAttribute(FJBGContext context,
                                        JClass clazz,
                                        Object owner,
                                        String name,
                                        int size,
                                        DataInputStream stream)
        throws IOException {
        super(context, clazz, name);
        this.pool = clazz.pool;

        int count = stream.readShort();
        for (int i = 0; i < count; ++i) {
            int startPc = stream.readShort();
            int length = stream.readShort();
            int nameIndex = stream.readShort();
            int descIndex = stream.readShort();
            int index = stream.readShort();
            addEntry(startPc, length, nameIndex, descIndex, index);
        }

        assert name.equals(getName());
    }

    public void addEntry(int startPc, int length, int nameIndex,
                         int descIndex, int index) {
        entries.add(new Entry(startPc, length, nameIndex, descIndex, index));
    }

    public void addEntry(int startPc, int length, String name,
                         String desc, int index) {
        Entry e = new Entry(startPc, length, name, desc, index);
        Entry other = getEntry(index);
        if (other != null) {
            assert other.nameIndex == e.nameIndex && other.descIndex == e.descIndex
                : e + " already declared as " + other;
        } else
            entries.add(e);
    }

    public void addEntry(int startPc, int length, String name, String desc) {
        entries.add(new Entry(startPc, length, name, desc));
    }

    public String getName() { return "LocalVariableTable"; }

    // Follows javap output format for LocalVariableTable attribute.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer("  LocalVariableTable: ");
        buf.append("\n   Start  Length  Slot  Name   Signature");
        for (Iterator it = entries.iterator(); it.hasNext(); ) {
            buf.append("\n   ");
            Entry e = (Entry)it.next();
            Utf8Entry name = (Utf8Entry)pool.lookupEntry(e.nameIndex);
            Utf8Entry sig = (Utf8Entry)pool.lookupEntry(e.descIndex);
            buf.append(e.startPc);
            buf.append("      ");
            buf.append(e.length);
            buf.append("      ");
            buf.append(e.index);
            buf.append("    ");
            buf.append(name.getValue());
            buf.append("       ");
            buf.append(sig.getValue());
        }
        buf.append("\n");
        return buf.toString();
    }

    public int getMaxLocals() {
        return localVariableIndex;
    }

    public int getSize() {
        return 2 + entries.size() * 10;
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(entries.size());
        for (Iterator it = entries.iterator(); it.hasNext(); ) {
            Entry e = (Entry)it.next();
            stream.writeShort(e.startPc);
            stream.writeShort(e.length);
            stream.writeShort(e.nameIndex);
            stream.writeShort(e.descIndex);
            stream.writeShort(e.index);
        }
    }

    private Entry getEntry(int index) {
        Entry e = null;
        try { e = (Entry)entries.get(index); } catch (Exception ex) {}
        return e;
    }

    private class Entry {
        int startPc;
        int length;
        int nameIndex;
        int descIndex;
        int index;

        public Entry(int startPc, int length, int nameIndex, int descIndex, int index) {
            this.startPc = startPc;
            this.length = length;
            this.nameIndex = nameIndex;
            this.descIndex = descIndex;
            this.index = index;
            localVariableIndex += length;
        }

        public Entry(int startPc, int length, String name, String desc, int index) {
            this(startPc, length, pool.addUtf8(name), pool.addUtf8(desc), index);
        }

        public Entry(int startPc, int length, String name, String desc) {
            this(startPc, length, pool.addUtf8(name), pool.addUtf8(desc), localVariableIndex);
        }

        /** Two entries are equal if they refer to the same index.
         */
        public boolean equals(Object other) {
            if (other instanceof Entry) {
                Entry otherEntry = (Entry) other;
                return otherEntry.index == this.index;
            }
            return false;
        }
    }
}
