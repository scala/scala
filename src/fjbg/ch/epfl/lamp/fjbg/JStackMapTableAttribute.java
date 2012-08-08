/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2012 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author Stephane Micheloud
 * @version 1.0
 */

public class JStackMapTableAttribute extends JAttribute {
    /** Constant pool of the current classfile. */
    private JConstantPool pool;

    /** StackMapTable entries */
    protected final List/*<Frame>*/ entries = new ArrayList();
    protected int entriesSize = 0;
    protected boolean usesU2;

    public JStackMapTableAttribute(FJBGContext context,
                                   JClass clazz,
                                   JCode code) {
        super(context, clazz);
        this.pool = clazz.pool;

        assert code.getOwner().getOwner() == clazz;
    }

    public JStackMapTableAttribute(FJBGContext context,
                                   JClass clazz,
                                   Object owner,
                                   String name,
                                   int size,
                                   DataInputStream stream)
        throws IOException {
        super(context, clazz, name);
        this.pool = clazz.pool;

        int count = stream.readShort();
        this.usesU2 = count < 65536;
        for (int i = 0; i < count; ++i)
           this.entries.add(new Frame(stream));
        this.entriesSize = computeSize();

        assert name.equals(getName());
    }

    public String getName() { return "StackMapTable"; }

    // Follows javap output format for StackMapTable attribute.
    /*@Override*/ public String toString() {
        Frame frame = null;
        StringBuffer buf = new StringBuffer("  StackMapTable: number_of_entries = ");
        buf.append(entries.size());
        Iterator it = entries.iterator();
        while (it.hasNext()) {
            frame = (Frame)it.next();
            buf.append("\n   frame_type = ");
            buf.append(frame.tag);
            buf.append(" /* ");
            buf.append(getFrameType(frame.tag));
            buf.append(" */");
            if (frame.offsetDelta != -1)
                buf.append("\n     offset_delta = "+frame.offsetDelta);
            if (frame.locals != null)
                appendTypeInfoArray(buf, "locals", frame.locals);
            if (frame.stackItems != null)
                appendTypeInfoArray(buf, "stack", frame.stackItems);
        }
        buf.append("\n");
        return buf.toString();
    }

    protected int getSize() {
        return entriesSize;
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(entriesSize);
        Iterator it = entries.iterator();
        while (it.hasNext()) {
            Frame frame = (Frame)it.next();
            frame.writeContentsTo(stream);
        }
    }

    private class TypeInfo {
        final int tag;
        final int poolIndexOrOffset; // tag == 7 => poolIndex, tag = 8 => offset
        private int bytes;
        TypeInfo(DataInputStream stream) throws IOException {
            int size = 1;
            this.tag = stream.readByte();
            if (tag == 7) { // ITEM_Object; // 7
                poolIndexOrOffset = stream.readShort();
                size += 2;
            } else if (tag == 8) { // ITEM_Uninitialized // 8
                poolIndexOrOffset = (usesU2) ? stream.readShort() : stream.readInt();
                size += (usesU2) ? 2 : 4;
            } else
                poolIndexOrOffset = -1;
            this.bytes += size;
        }
        int getSize() { return bytes; }
        void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeByte(tag);
            if (tag == 7) { // ITEM_Object; // 7
                stream.writeShort(poolIndexOrOffset);
            } else if (tag == 8) { // ITEM_Uninitialized // 8
                if (usesU2) stream.writeShort(poolIndexOrOffset);
                else stream.writeInt(poolIndexOrOffset);
            }
        }
        /*@Override*/ public String toString() {
            switch (tag) {
            case 0: // ITEM_Top
                return "<top>";
            case 1: // ITEM_Integer
                return "int";
            case 2: // ITEM_Float
                return "float";
            case 3: // ITEM_Double
                return "double";
            case 4: // ITEM_Long
                return "long";
            case 5: // ITEM_Null
                return "null";
            case 6: // ITEM_UninializedThis
                return "this";
            case 7: // ITEM_Object
                String name = pool.lookupClass(poolIndexOrOffset);
                if (name.startsWith("[")) name = "\""+name+"\"";
                return "class "+name;
            case 8: // ITEM_Uninitialized
                return "<uninitialized>";
            default:
                return String.valueOf(tag);
            }
        }
    }

    private class Frame {
        final int tag;
        int offsetDelta = -1;
        TypeInfo[] stackItems = null;
        TypeInfo[] locals = null;
        private int bytes;
        Frame(DataInputStream stream) throws IOException {
            // The stack_map_frame structure consists of a one-byte tag
            // followed by zero or more bytes.
            this.tag = stream.readUnsignedByte();
            if (tag < 64) { // SAME;  // 0-63
                //done
            } else if (tag < 128) { // SAME_LOCALS_1_STACK_ITEM;  // 64-127
                this.offsetDelta = tag - 64;
                readStackItems(stream, 1);
            } else if (tag < 248) { // reserved for future use.
                assert false : "Tags in the range [128-247] are reserved for future use.";
            } else if (tag < 251) { // CHOP;  // 248-250
                int k = 251 - tag;
                readOffsetDelta(stream);
            } else if (tag == 251) { // SAME_FRAME_EXTENDED
                readOffsetDelta(stream);
            } else if (tag < 255) { // APPEND;  // 252-254
                readOffsetDelta(stream);
                readLocals(stream, tag - 251);
            } else {               // FULL_FRAME;  // 255
                readOffsetDelta(stream);
                readLocals(stream);
                readStackItems(stream);
            }
        }
        int getSize() { return bytes; }
        void readOffsetDelta(DataInputStream stream) throws IOException {
            this.offsetDelta = (usesU2) ? stream.readShort() : stream.readInt();
            this.bytes += (usesU2) ? 2 : 4;
        }
        int getOffsetDelta() { return offsetDelta; }
        void readStackItems(DataInputStream stream, int k) throws IOException {
            this.stackItems = new TypeInfo[k];
            for (int i = 0; i < k; ++i) {
                stackItems[i] = new TypeInfo(stream);
                this.bytes += stackItems[i].getSize();
            }
        }
        void readStackItems(DataInputStream stream) throws IOException {
            int k = (usesU2) ? stream.readShort() : stream.readInt();
            this.bytes += (usesU2) ? 2 : 4;
            readStackItems(stream, k);
        }
        void readLocals(DataInputStream stream, int k) throws IOException {
            this.locals = new TypeInfo[k];
            for (int i = 0; i < k; ++i) {
                locals[i] = new TypeInfo(stream);
                this.bytes += locals[i].getSize();
            }
        }
        void readLocals(DataInputStream stream) throws IOException {
            int k = (usesU2) ? stream.readShort() : stream.readInt();
            this.bytes += (usesU2) ? 2 : 4;
            readLocals(stream, k);
        }
        void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeByte(tag);
            if (tag < 64) {
                //done
            } else if (tag < 128) { // SAME;  // 0-63
                assert stackItems.length == 1;
                stackItems[0].writeContentsTo(stream);
            } else if (tag < 248) {
                assert false : "Tags in the range [128-247] are reserved for future use.";
            } else if (tag < 251) {
                if (usesU2) stream.writeShort(offsetDelta);
                else stream.writeInt(offsetDelta);
            } else if (tag == 251) {
                if (usesU2) stream.writeShort(offsetDelta);
                else stream.writeInt(offsetDelta);
            } else if (tag < 255) { // APPEND;  // 252-254
                if (usesU2) stream.writeShort(offsetDelta);
                else stream.writeInt(offsetDelta);
                for (int i = 0; i < locals.length; ++i)
                    locals[i].writeContentsTo(stream);
            } else {
                if (usesU2) stream.writeShort(offsetDelta);
                else stream.writeInt(offsetDelta);
                for (int i = 0; i < locals.length; ++i)
                    locals[i].writeContentsTo(stream);
                for (int i = 0; i < stackItems.length; ++i)
                    stackItems[i].writeContentsTo(stream);
            }
        }
    }

    private int computeSize() {
        int size = (usesU2) ? 2 : 4; // number of frames
        Iterator it = entries.iterator();
        while (it.hasNext()) {
            Frame frame = (Frame)it.next();
            size += frame.getSize();
        }
        return size;
    }

    private static final String getFrameType(int tag) {
        if (tag < 64) return "same";
        else if (tag < 128) return "same locals 1 stack item";
        else if (tag < 248) return "<reserved>";
        else if (tag < 251) return "chop";
        else if (tag == 251) return "same frame extended";
        else if (tag < 255) return "append";
        else return "full frame";
    }

    private static StringBuffer appendTypeInfoArray(StringBuffer buf,
                                                    String s, TypeInfo[] a) {
        buf.append("\n     ");
        buf.append(s);
        buf.append(" = ");
        if (a.length > 0) {
            buf.append("[ ");
            for (int i = 0; i < a.length; ++i) {
                if (i > 0) buf.append(", ");
                buf.append(a[i]);
            }
            buf.append(" ]");
        }
        else
            buf.append("[]");
        return buf;
    }

}
