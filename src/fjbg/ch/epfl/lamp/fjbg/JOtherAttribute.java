
package ch.epfl.lamp.fjbg;

import java.io.*;

/**
 * Attributes which are unknown to the JVM (or at least to this
 * library).
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

    protected int getSize() { return length; }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.write(contents, 0, length);
    }
}
