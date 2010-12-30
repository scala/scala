/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * Sourcefile attribute, which can be attached to class files to
 * associate them with their source file.
 *
 * There can be no more than one SourceFile attribute in the attributes table
 * of a given ClassFile structure. See section 4.8.9 of the JVM specification.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JSourceFileAttribute extends JAttribute {
    protected final String sourceFileName;
    protected final int sourceFileIndex;

    public JSourceFileAttribute(FJBGContext context,
                                JClass clazz,
                                String sourceFileName) {
        super(context, clazz);
        this.sourceFileName = sourceFileName;
        this.sourceFileIndex = clazz.getConstantPool().addUtf8(sourceFileName);
    }

    public JSourceFileAttribute(FJBGContext context,
                                JClass clazz,
                                Object owner,
                                String name,
                                int size,
                                DataInputStream stream)
        throws IOException {
        super(context, clazz, name);

        this.sourceFileIndex = stream.readShort();
        this.sourceFileName = clazz.getConstantPool().lookupUtf8(sourceFileIndex);

        assert name.equals(getName());
    }

    public String getName() { return "SourceFile"; }

    public String getFileName() { return sourceFileName; }

    // Follows javap output format for SourceFile attribute.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer("  SourceFile: \"");
        buf.append(sourceFileName);
        buf.append("\"\n");
        return buf.toString();
    }

    protected int getSize() {
        return 2; // Short.SIZE
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(sourceFileIndex);
    }
}
