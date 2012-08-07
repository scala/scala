/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2012 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * InnerClasses attribute.
 *
 * The ClassFile structure of a class/interface C must have exactly one
 * InnerClasses attribute in its attributes table if the constant pool of C
 * contains a CONSTANT_Class_info entry which represents a class or interface
 * that is not a member of a package. See section 4.8.5 of the JVM Specification.
 *
 * @author Iulian Dragos, Stephane Micheloud
 * @version 1.1
 */
public class JInnerClassesAttribute extends JAttribute {
    /** Constant pool of the current classfile. */
    private JConstantPool pool;

    /** InnerClass entries */
    private Map/*<String, Entry>*/ entries = new LinkedHashMap();

    public JInnerClassesAttribute(FJBGContext context, JClass clazz) {
        super(context, clazz);
        this.pool = clazz.pool;
    }

    public JInnerClassesAttribute(FJBGContext context,
                                  JClass clazz,
                                  Object owner,
                                  String name,
                                  int size,
                                  DataInputStream stream)
        throws IOException {
        super(context, clazz, name);
        this.pool = clazz.pool;

        String inner = null;
        int count = stream.readShort();
        for (int i = 0; i < count; ++i) {
            int innerIdx = stream.readShort();
            int outerIdx = stream.readShort();
            int nameIdx = stream.readShort();
            int flags = stream.readShort();
            inner = pool.lookupClass(innerIdx);
            entries.put(inner, new Entry(innerIdx, outerIdx, nameIdx, flags));
        }

        assert name.equals(getName());
    }

    public void addEntry(String inner, String outer, String name, int flags) {
        int innerIdx = pool.addClass(inner);
        int outerIdx = 0;
        if (outer != null) outerIdx = pool.addClass(outer);
        int nameIdx = 0;
        if (name != null) nameIdx = pool.addUtf8(name);

        Entry e = new Entry(innerIdx, outerIdx, nameIdx, flags);

        if (entries.containsKey(inner)) {
            Entry other = (Entry) entries.get(inner);
            assert other.outerInfo == e.outerInfo && other.originalName == e.originalName && other.innerFlags == e.innerFlags
                : inner + " already declared as " + other;
        } else
            entries.put(inner, e);
    }

    public String getName() { return "InnerClasses"; }

    // Follows javap output format for the InnerClass attribute.
    /*@Override*/ public String toString() {
        // Here we intentionally use "InnerClass" as javap :-(
        StringBuffer buf = new StringBuffer("  InnerClass: ");
        for (Iterator it = entries.values().iterator(); it.hasNext(); ) {
            Entry e = (Entry)it.next();
            buf.append("\n   ");
            buf.append(e.innerFlagsToString());
            buf.append("#");
            if (e.originalName != 0) {
                buf.append(e.originalName);
                buf.append("= #");
            }
            buf.append(e.innerInfo);
            if (e.outerInfo != 0) {
                buf.append(" of #");
                buf.append(e.outerInfo);
            }
            buf.append("; //");
            if (e.originalName != 0) {
                buf.append(pool.lookupUtf8(e.originalName));
                buf.append("=");
            }
            buf.append("class ");
            buf.append(pool.lookupClass(e.innerInfo));
            if (e.outerInfo != 0) {
                buf.append(" of class ");
                buf.append(pool.lookupClass(e.outerInfo));
            }
        }
        buf.append("\n");
        return buf.toString();
    }

    protected int getSize() {
        return 2 + entries.size() * 8;
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(entries.size());
        for (Iterator it = entries.values().iterator(); it.hasNext(); ) {
            Entry e = (Entry)it.next();
            stream.writeShort(e.innerInfo);
            stream.writeShort(e.outerInfo);
            stream.writeShort(e.originalName);
            stream.writeShort(e.innerFlags);
        }
    }

    /** An entry in the InnerClasses attribute, as defined by the JVM Spec. */
    private class Entry {
        /** CONSTANT_Class_info index in the pool for the inner class (mangled). */
        int innerInfo;

        /** CONSTANT_Class_info index in the pool for the outer class (mangled). */
        int outerInfo;

        /** CONSTANT_Utf8_info index in the pool for the original name of the inner class. */
        int originalName;

        /** Short int for modifier flags. */
        int innerFlags;

        public Entry(int iI, int oI, int oN, int f) {
            this.innerInfo = iI;
            this.outerInfo = oI;
            this.originalName = oN;
            this.innerFlags = f;
        }

        public Entry(String innerClass, String outerClass, String name, int flags) {
            this(pool.addClass(innerClass), pool.addClass(outerClass), pool.addUtf8(name), flags);
        }

        /** Two entries are equal if they refer to the same inner class.
         *  innerInfo represents a unique name (mangled).
         */
        public boolean equals(Object other) {
            if (other instanceof Entry) {
                Entry otherEntry = (Entry) other;
                return otherEntry.innerInfo == this.innerInfo;
            }
            return false;
        }

        public String innerFlagsToString() {
            StringBuffer buf = new StringBuffer();
            if (isPublic()) buf.append("public ");
            else if (isProtected()) buf.append("protected ");
            else if (isPrivate()) buf.append("private ");
            //if (isStatic()) buf.append("static "); // as javap
            if (isAbstract()) buf.append("abstract ");
            else if (isFinal()) buf.append("final ");
            return buf.toString();
        }

        private boolean isPublic() {
            return (innerFlags & JAccessFlags.ACC_PUBLIC) != 0;
        }

        private boolean isPrivate() {
            return (innerFlags & JAccessFlags.ACC_PRIVATE) != 0;
        }

        private boolean isProtected() {
            return (innerFlags & JAccessFlags.ACC_PROTECTED) != 0;
        }

        private boolean isStatic() {
            return (innerFlags & JAccessFlags.ACC_STATIC) != 0;
        }

        private boolean isFinal() {
            return (innerFlags & JAccessFlags.ACC_FINAL) != 0;
        }

        private boolean isAbstract() {
            return (innerFlags & JAccessFlags.ACC_ABSTRACT) != 0;
        }
    }
}
