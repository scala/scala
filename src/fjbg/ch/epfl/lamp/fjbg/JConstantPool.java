
package ch.epfl.lamp.fjbg;

import java.util.*;
import java.io.*;

/**
 * Constant pool, holding constants for a Java class file.
 *
 * @author Michel Schinz
 * @version 2.0
 */

public class JConstantPool {
    protected boolean frozen = false;

    protected HashMap/*<Entry,Integer>*/ entryToIndex = new HashMap();
    protected Entry[] indexToEntry;
    protected int currIndex;

    public static final short CONSTANT_Utf8               = 1;
    public static final short CONSTANT_Integer            = 3;
    public static final short CONSTANT_Float              = 4;
    public static final short CONSTANT_Long               = 5;
    public static final short CONSTANT_Double             = 6;
    public static final short CONSTANT_Class              = 7;
    public static final short CONSTANT_String             = 8;
    public static final short CONSTANT_Fieldref           = 9;
    public static final short CONSTANT_Methodref          = 10;
    public static final short CONSTANT_InterfaceMethodref = 11;
    public static final short CONSTANT_NameAndType        = 12;

    protected JConstantPool(FJBGContext context) {
        indexToEntry = new Entry[8];
        currIndex = 1;
    }

    protected JConstantPool(FJBGContext context, DataInputStream stream)
        throws IOException {
        int count = stream.readShort();
        indexToEntry = new EntryIndex[count];

        currIndex = 1;
        while (currIndex < count) {
            EntryIndex e;
            int tag = stream.readByte();

            switch (tag) {
            case CONSTANT_Utf8:
                e = new Utf8Entry(stream);
                break;
            case CONSTANT_Integer:
                e = new IntegerEntry(stream);
                break;
            case CONSTANT_Float:
                e = new FloatEntry(stream);
                break;
            case CONSTANT_Long:
                e = new LongEntry(stream);
                break;
            case CONSTANT_Double:
                e = new DoubleEntry(stream);
                break;
            case CONSTANT_Class:
                e = new DescriptorEntryIndex(stream);
                break;
            case CONSTANT_String:
                e = new StringEntryIndex(stream);
                break;
            case CONSTANT_Fieldref:
            case CONSTANT_Methodref:
            case CONSTANT_InterfaceMethodref:
                e = new FieldOrMethodRefEntryIndex(tag, stream);
                break;
            case CONSTANT_NameAndType:
                e = new NameAndTypeEntryIndex(stream);
                break;
            default:
                throw new IllegalArgumentException("unknown entry in pool: " + tag);
            }
            indexToEntry[currIndex] = e;
            currIndex += e.getSize();
        }
    }

    public void freeze() { frozen = true; }

    /**
     * Returns a string representing the type of an entry
     * knowing its tag
     * @param tag The tag representing the type of the
     * constant pool entry
     */
    public String getEntryType(int tag) {
        switch (tag) {
        case 4 : return "Utf8";
        case 5 : return "Integer";
        case 6 : return "Float";
        case 7 : return "Long";
        case 8 : return "Double";
        case 9 : return "Class";
        case 10 : return "String";
        case 11 : return "Fieldref";
        case 12 : return "Methodref";
        case 13 : return "InterfaceMethodref";
        case 14 : return "NameAndType";
        default : throw new Error("invalid constant pool tag : " + tag);
        }
    }

    public int addClass(String className) {
        return addDescriptor(className.replace('.', '/'));
    }

    public String lookupClass(int index) {
        DescriptorEntry entry = (DescriptorEntry)lookupEntry(index);
        return entry.getValue().replace('/', '.');
    }

    public int addDescriptor(JReferenceType type) {
        return addDescriptor(type.getDescriptor());
    }

    protected int addDescriptor(String name) {
        return addEntry(new DescriptorEntryValue(name));
    }

    public int addClassMethodRef(String className,
                                 String methodName,
                                 String signature) {
        return addMethodRef(true, className, methodName, signature);
    }

    public int addInterfaceMethodRef(String className,
                                     String methodName,
                                     String signature) {
        return addMethodRef(false, className, methodName, signature);
    }

    public int addMethodRef(boolean isClass,
			    String className,
			    String methodName,
			    String signature) {
        return addEntry(new FieldOrMethodRefEntryValue(isClass
                                                       ? CONSTANT_Methodref
                                                       : CONSTANT_InterfaceMethodref,
                                                       className,
                                                       methodName,
                                                       signature));
    }

    public int addFieldRef(String className,
                           String fieldName,
                           String signature) {
        return addEntry(new FieldOrMethodRefEntryValue(CONSTANT_Fieldref,
                                                       className,
                                                       fieldName,
                                                       signature));
    }

    public int addInteger(int value) {
        return addEntry(new IntegerEntry(value));
    }

    public int addFloat(float value) {
        return addEntry(new FloatEntry(value));
    }

    public int addLong(long value) {
        return addEntry(new LongEntry(value));
    }

    public int addDouble(double value) {
        return addEntry(new DoubleEntry(value));
    }

    public int addString(String value) {
        return addEntry(new StringEntryValue(value));
    }

    public int addNameAndType(String name, String descriptor) {
        return addEntry(new NameAndTypeEntryValue(name, descriptor));
    }

    public int addUtf8(String value) {
        return addEntry(new Utf8Entry(value));
    }

    public int addUtf8(byte[] value) {
        return addEntry(new Utf8Entry(value));
    }

    public String lookupUtf8(int index) {
        Utf8Entry entry = (Utf8Entry)lookupEntry(index);
        return entry.getValue();
    }

    protected int addEntry(EntryValue e) {
        assert !frozen;

        Integer idx = (Integer)entryToIndex.get(e);
        if (idx != null)
            return idx.intValue();

        e.addChildren();

        int index = currIndex;
        currIndex += e.getSize();

        entryToIndex.put(e, new Integer(index));
        if (index >= indexToEntry.length) {
            Entry[] newI2E = new Entry[indexToEntry.length * 2];
            System.arraycopy(indexToEntry, 0, newI2E, 0, indexToEntry.length);
            indexToEntry = newI2E;
        }
        indexToEntry[index] = e;
        return index;
    }

    public Entry lookupEntry(int index) {
        assert index > 0 && index < currIndex
            : "invalid index: " + index;
        assert indexToEntry[index] != null
            : "invalid index (null contents): " + index;
        return indexToEntry[index];
    }

    public void writeTo(DataOutputStream stream) throws IOException {
        if (! frozen) freeze();

	stream.writeShort(currIndex);
        for (int i = 0; i < currIndex; ++i) {
            Entry entry = indexToEntry[i];
            if (entry != null) {
                stream.writeByte(entry.getTag());
                entry.writeContentsTo(stream);
            }
        }
    }

    /// Classes for the various kinds of entries
    //////////////////////////////////////////////////////////////////////

    public interface Entry {
        public int getTag();

        int getSize();
        void writeContentsTo(DataOutputStream stream) throws IOException;
    }

    protected interface EntryValue extends Entry {
        abstract void addChildren();
    }

    protected interface EntryIndex extends Entry {
        abstract void fetchChildren();
    }

    abstract protected class ChildlessEntry implements EntryValue, EntryIndex {
        public void addChildren() {}
        public void fetchChildren() {}
    }

    public class IntegerEntry extends ChildlessEntry implements Entry {
        private final int value;
        public IntegerEntry(int value) { this.value = value; }
        public IntegerEntry(DataInputStream stream) throws IOException {
            this(stream.readInt());
        }

        public int hashCode() { return value; }
        public boolean equals(Object o) {
            return o instanceof IntegerEntry && ((IntegerEntry)o).value == value;
        }

        public int getTag() { return CONSTANT_Integer; }
        public int getValue() { return value; }

        public int getSize() { return 1; }
        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeInt(value);
        }
    }

    public class FloatEntry extends ChildlessEntry implements Entry {
        private final float value;
        public FloatEntry(float value) { this.value = value; }
        public FloatEntry(DataInputStream stream) throws IOException {
            this(stream.readFloat());
        }

        public int hashCode() { return (int)value; }
        public boolean equals(Object o) {
            return o instanceof FloatEntry && ((FloatEntry)o).value == value;
        }

        public int getTag() { return CONSTANT_Float; }
        public float getValue() { return value; }

        public int getSize() { return 1; }
        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeFloat(value);
        }
    }

    public class LongEntry extends ChildlessEntry implements Entry {
        private final long value;
        public LongEntry(long value) { this.value = value; }
        public LongEntry(DataInputStream stream) throws IOException {
            this(stream.readLong());
        }

        public int hashCode() { return (int)value; }
        public boolean equals(Object o) {
            return o instanceof LongEntry && ((LongEntry)o).value == value;
        }

        public int getTag() { return CONSTANT_Long; }
        public long getValue() { return value; }

        public int getSize() { return 2; }
        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeLong(value);
        }
    }

    public class DoubleEntry extends ChildlessEntry implements Entry {
        private final double value;
        public DoubleEntry(double value) { this.value = value; }
        public DoubleEntry(DataInputStream stream) throws IOException {
            this(stream.readDouble());
        }

        public int hashCode() { return (int)value; }
        public boolean equals(Object o) {
            return o instanceof DoubleEntry && ((DoubleEntry)o).value == value;
        }

        public int getTag() { return CONSTANT_Double; }
        public double getValue() { return value; }

        public int getSize() { return 2; }
        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeDouble(value);
        }
    }

    public class Utf8Entry extends ChildlessEntry implements Entry {
        private final String value;
        private final byte[] bytes;
        public Utf8Entry(String value) {
            this.value = value.intern();
            this.bytes = null;
        }
        public Utf8Entry(DataInputStream stream) throws IOException {
            this(stream.readUTF());
        }
        public Utf8Entry(byte[] bytes) {
            this.bytes = bytes;
            this.value = null;
        }

        public int hashCode() {
            if (bytes != null) return bytes.hashCode();
            return value.hashCode();
        }
        public boolean equals(Object o) {
            boolean isEqual = o instanceof Utf8Entry;
            if (bytes != null) {
                isEqual = isEqual && ((Utf8Entry)o).bytes == bytes;
            }
            else {
                isEqual = isEqual && ((Utf8Entry)o).value == value;
            }
            return isEqual;
        }

        public int getTag() { return CONSTANT_Utf8; }
        public String getValue() { return value; }
        public byte[] getBytes() { return bytes; }

        public int getSize() { return 1; }
        public void writeContentsTo(DataOutputStream stream) throws IOException {
            if (bytes != null) {
                if (bytes.length > 65535) {
                    throw new IOException("String literal of length " + bytes.length + " does not fit in Classfile");
                }
                stream.writeShort(bytes.length);
                stream.write(bytes);
            }
            else
                stream.writeUTF(value);
        }
    }

    abstract public class StringEntry implements Entry {
        protected String value;
        protected int valueIndex;

        public int hashCode() {
            assert value != null;
            return value.hashCode();
        }
        public boolean equals(Object o) {
            return o instanceof StringEntry && ((StringEntry)o).value == value;
        }

        public int getTag() { return CONSTANT_String; }
        public String getValue() { return value; }

        public int getSize() { return 1; }
        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeShort(valueIndex);
        }
    }

    public class StringEntryValue extends StringEntry implements EntryValue {
        public StringEntryValue(String value) {
            this.value = value.intern();
        }
        public void addChildren() {
            valueIndex = addUtf8(value);
        }
    }

    public class StringEntryIndex extends StringEntry implements EntryIndex {
        public StringEntryIndex(int valueIndex) {
            this.valueIndex = valueIndex;
        }
        public StringEntryIndex(DataInputStream stream) throws IOException {
            this(stream.readShort());
        }
        public String getValue() {
            if (value == null) fetchChildren();
            return super.getValue();
        }
        public void fetchChildren() {
            value = lookupUtf8(valueIndex);
        }
    }

    abstract public class DescriptorEntry implements Entry {
        protected String name;
        protected int nameIndex;

        public int hashCode() {
            assert name != null;
            return name.hashCode();
        }
        public boolean equals(Object o) {
            return o instanceof DescriptorEntry && ((DescriptorEntry)o).name == name;
        }

        public int getTag() { return CONSTANT_Class; }
        public String getValue() { return name; }

        public int getSize() { return 1; }
        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeShort(nameIndex);
        }
    }

    protected class DescriptorEntryValue
        extends DescriptorEntry
        implements EntryValue {
        public DescriptorEntryValue(String name) { this.name = name.intern(); }
        public void addChildren() {
            nameIndex = addUtf8(name);
        }
    }

    protected class DescriptorEntryIndex
        extends DescriptorEntry
        implements EntryIndex {
        public DescriptorEntryIndex(int nameIndex) { this.nameIndex = nameIndex; }
        public DescriptorEntryIndex(DataInputStream stream) throws IOException {
            this(stream.readShort());
        }
        public String getValue() {
            if (name == null) fetchChildren();
            return super.getValue();
        }
        public void fetchChildren() {
            name = lookupUtf8(nameIndex);
        }
    }

    abstract public class FieldOrMethodRefEntry implements Entry {
        private final int tag;
        protected String className, thingName, signature;
        protected int classIndex, nameAndTypeIndex;

        public FieldOrMethodRefEntry(int tag) {
            assert tag == CONSTANT_Fieldref
                || tag == CONSTANT_Methodref
                || tag == CONSTANT_InterfaceMethodref;

            this.tag = tag;
        }

        public int hashCode() {
            return tag
                + className.hashCode()
                + thingName.hashCode()
                + signature.hashCode();
        }
        public boolean equals(Object o) {
            return o instanceof FieldOrMethodRefEntry
                && ((FieldOrMethodRefEntry)o).tag == tag
                && ((FieldOrMethodRefEntry)o).className == className
                && ((FieldOrMethodRefEntry)o).thingName == thingName
                && ((FieldOrMethodRefEntry)o).signature == signature;
        }

        public int getTag() { return tag; }
        public String getClassName() { return className; }
        public String getFieldOrMethodName() { return thingName; }
        public String getSignature() { return signature; }

        public int getSize() { return 1; }
        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeShort(classIndex);
            stream.writeShort(nameAndTypeIndex);
        }
    }

    protected class FieldOrMethodRefEntryValue
        extends FieldOrMethodRefEntry
        implements EntryValue {
        public FieldOrMethodRefEntryValue(int tag,
                                          String className,
                                          String thingName,
                                          String signature) {
            super(tag);
            this.className = className.intern();
            this.thingName = thingName.intern();
            this.signature = signature.intern();
        }

        public void addChildren() {
            classIndex = addClass(className);
            nameAndTypeIndex = addNameAndType(thingName, signature);
        }
    }

    protected class FieldOrMethodRefEntryIndex
        extends FieldOrMethodRefEntry
        implements EntryIndex {
        public FieldOrMethodRefEntryIndex(int tag,
                                          int classIndex,
                                          int nameAndTypeIndex) {
            super(tag);
            this.classIndex = classIndex;
            this.nameAndTypeIndex = nameAndTypeIndex;
        }
        public FieldOrMethodRefEntryIndex(int tag, DataInputStream stream)
            throws IOException {
            this(tag, stream.readShort(), stream.readShort());
        }
        public String getClassName() {
            if (className == null) fetchChildren();
            return super.getClassName();
        }
        public String getFieldOrMethodName() {
            if (thingName == null) fetchChildren();
            return super.getFieldOrMethodName();
        }
        public String getSignature() {
            if (signature == null) fetchChildren();
            return super.getSignature();
        }
        public void fetchChildren() {
            className = lookupClass(classIndex);
            NameAndTypeEntry nat = (NameAndTypeEntry)lookupEntry(nameAndTypeIndex);
            thingName = nat.getName();
            signature = nat.getDescriptor();
        }
    }

    abstract public class NameAndTypeEntry implements Entry {
        protected String name, descriptor;
        protected int nameIndex, descriptorIndex;

        public int hashCode() { return name.hashCode() + descriptor.hashCode(); }
        public boolean equals(Object o) {
            return o instanceof NameAndTypeEntry
                && ((NameAndTypeEntry)o).name == name
                && ((NameAndTypeEntry)o).descriptor == descriptor;
        }

        public int getTag() { return CONSTANT_NameAndType; }
        public String getName() { return name; }
        public String getDescriptor() { return descriptor; }

        public int getSize() { return 1; }
        public void writeContentsTo(DataOutputStream stream) throws IOException {
            stream.writeShort(nameIndex);
            stream.writeShort(descriptorIndex);
        }
    }

    protected class NameAndTypeEntryValue
        extends NameAndTypeEntry
        implements EntryValue {
        public NameAndTypeEntryValue(String name, String descriptor) {
            this.name = name.intern();
            this.descriptor = descriptor.intern();
        }
        public void addChildren() {
            nameIndex = addUtf8(name);
            descriptorIndex = addUtf8(descriptor);
        }
    }

    protected class NameAndTypeEntryIndex
        extends NameAndTypeEntry
        implements EntryIndex {
        public NameAndTypeEntryIndex(int nameIndex, int descriptorIndex) {
            this.nameIndex = nameIndex;
            this.descriptorIndex = descriptorIndex;
        }
        public NameAndTypeEntryIndex(DataInputStream stream) throws IOException {
            this(stream.readShort(), stream.readShort());
        }
        public String getName() {
            if (name == null) fetchChildren();
            return super.getName();
        }
        public String getDescriptor() {
            if (descriptor == null) fetchChildren();
            return super.getDescriptor();
        }
        public void fetchChildren() {
            name = lookupUtf8(nameIndex);
            descriptor = lookupUtf8(descriptorIndex);
        }
    }
}
