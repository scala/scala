/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * Attributes which are unknown to the JVM (or at least to this library).
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JOtherAttribute extends JAttribute {
    protected final String name;
    protected final byte[] contents;
    protected final int length;

    public JOtherAttribute(FJBGContext context,
                           JClass clazz,
                           Object owner,
                           String name,
                           byte[] contents,
                           int length) {
        super(context, clazz, name);
        this.name = name;
        this.contents = contents;
        this.length = length;
    }

    public JOtherAttribute(FJBGContext context,
                           JClass clazz,
                           Object owner,
                           String name,
                           int size,
                           DataInputStream stream)
        throws IOException {
        super(context, clazz, name);
        this.name = name;
        this.contents = new byte[size];
        this.length = size;

        stream.read(contents, 0, length);
    }

    public String getName() { return name; }

    // Follows javap output format for user-defined attributes.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer("  ");
        buf.append(name);
        buf.append(": length = 0x");
        buf.append(Integer.toHexString(length).toUpperCase());
        for (int i = 0; i < length; ++i) {
            if (i % 16 == 0) buf.append("\n   ");
            buf.append(hexString(contents[i]));
            buf.append(" ");
        }
        buf.append("\n");
        return buf.toString();
    }

    protected int getSize() { return length; }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.write(contents, 0, length);
    }

    private static final String hexString(int i) {
        return ((0 <= i && i < 16) ? "0" : "")+Integer.toHexString(i).toUpperCase();
    }
}
