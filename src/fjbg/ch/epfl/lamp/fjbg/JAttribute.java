// $Id$

package ch.epfl.lamp.fjbg;

import java.io.*;
import java.util.*;

/**
 * Abstract superclass for attributes which can be attached to various
 * parts of a class file.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public abstract class JAttribute {
    protected final int nameIdx;

    static public void writeTo(List/*<JAttribute>*/ attrs, DataOutputStream stream)
        throws IOException {
        stream.writeShort(attrs.size());
        Iterator attrsIt = attrs.iterator();
        while (attrsIt.hasNext()) {
            JAttribute attr = (JAttribute)attrsIt.next();
            attr.writeTo(stream);
        }
    }

    static public List/*<JAttribute>*/ readFrom(FJBGContext context,
                                                JClass clazz,
                                                Object owner,
                                                DataInputStream stream)
        throws IOException {
        JAttributeFactory factory = context.getJAttributeFactory();
        int count = stream.readShort();
        ArrayList list = new ArrayList(count);
        for (int i = 0; i < count; ++i)
            list.add(factory.newInstance(clazz, owner, stream));
        return list;
    }

    public JAttribute(FJBGContext context, JClass clazz) {
        this.nameIdx = clazz.getConstantPool().addUtf8(getName());
    }

    public JAttribute(FJBGContext context, JClass clazz, String name) {
        this.nameIdx = clazz.getConstantPool().addUtf8(name);
    }

    abstract public String getName();

    /**
     * Write the attribute to a stream.
     */
    public void writeTo(DataOutputStream stream) throws IOException {
        int contentsSize = getSize();

        stream.writeShort(nameIdx);
        stream.writeInt(contentsSize);
        int streamSizeBefore = stream.size();
        writeContentsTo(stream);
        int streamSizeDiff = stream.size() - streamSizeBefore;

        assert contentsSize == streamSizeDiff
            : "invalid size for attribute " + getName()
            + " given: " + contentsSize
            + " actual: " + streamSizeDiff;
    }

    // Note: it is not legal to add data to the constant pool during
    // the execution of any of the following two methods.
    protected abstract int getSize();
    protected abstract void writeContentsTo(DataOutputStream stream)
        throws IOException;
}
