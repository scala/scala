
package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * Sourcefile attribute, which can be attached to class files to
 * associate them with their source file.
 *
 * @version 1.0
 * @author Michel Schinz
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
        super(context, clazz);
        stream.readInt();       // ignore size
        this.sourceFileIndex = stream.readShort();
        this.sourceFileName = clazz.getConstantPool().lookupUtf8(sourceFileIndex);

        assert name.equals(getName());
    }

    public String getName() { return "SourceFile"; }

    protected int getSize() {
        return 2;
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(sourceFileIndex);
    }
}
