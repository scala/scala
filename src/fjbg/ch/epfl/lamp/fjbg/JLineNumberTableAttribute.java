/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * Attribute storing correspondance between instructions and source
 * line numbers.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JLineNumberTableAttribute extends JAttribute {
    protected final JCode code;

    public JLineNumberTableAttribute(FJBGContext context,
                                     JClass clazz,
                                     JCode owner) {
        super(context, clazz);
        this.code = owner;

        assert owner.getOwner().getOwner() == clazz;
    }

    public JLineNumberTableAttribute(FJBGContext context,
                                     JClass clazz,
                                     Object owner,
                                     String name,
                                     int size,
                                     DataInputStream stream)
        throws IOException {
        super(context, clazz, name);
        this.code = (JCode)owner;

        int[] mapping = new int[code.getSize()];

        int count = stream.readShort();
        for (int i = 0; i < count; ++i) {
            int startPC = stream.readShort();
            int lineNum = stream.readShort();
            mapping[startPC] = lineNum;
        }

        // Avoids duplication of LineNumberTable attribute
        // (see method ensureLineNumberCapacity in class JCode).
        assert code.lineNumbers == null;
        code.lineNumbers = new int[0];

        int lineNum = 0;
        for (int pc = 0; pc < mapping.length; ++pc) {
            if (mapping[pc] != 0) lineNum = mapping[pc];
            if (lineNum != 0) code.setLineNumber(pc, lineNum);
        }

        assert name.equals(getName());
    }

    public String getName() { return "LineNumberTable"; }

    // Follows javap output format for LineNumberTable attribute.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer("  LineNumberTable: ");
        int[] encoding = encode();
        for (int i = 0; i < encoding.length/2; ++i) {
            buf.append("\n   line ");
            buf.append(encoding[i * 2 + 1]);
            buf.append(": ");
            buf.append(encoding[i * 2]);
        }
        buf.append("\n");
        return buf.toString();
    }

    protected int[] encoding;
    protected int[] encode() {
        if (encoding == null) {
            int[] lineNumbers = code.getLineNumbers();
            int[] preEncoding = new int[lineNumbers.length * 2];
            int prevLineNum = 0;

            int i = 0;
            for (int pc = 0; pc < lineNumbers.length; ++pc) {
                int lineNum = lineNumbers[pc];
                if (lineNum != 0 & lineNum != prevLineNum) {
                    preEncoding[i++] = pc;
                    preEncoding[i++] = lineNum;
                    prevLineNum = lineNum;
                }
            }
            if (i == preEncoding.length)
                encoding = preEncoding;
            else {
                encoding = new int[i];
                System.arraycopy(preEncoding, 0, encoding, 0, i);
            }
        }
        return encoding;
    }

    protected int getSize() {
        int[] encoding = encode();
        return 2 + encoding.length * 2;
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        int[] encoding = encode();
        int entries = encoding.length / 2;
        stream.writeShort(entries);
        for (int i = 0; i < entries; ++i) {
            stream.writeShort(encoding[i * 2]);
            stream.writeShort(encoding[i * 2 + 1]);
        }
    }
}
