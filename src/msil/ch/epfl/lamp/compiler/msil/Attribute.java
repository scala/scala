/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package ch.epfl.lamp.compiler.msil;

import ch.epfl.lamp.compiler.msil.util.Signature;

import java.util.Map;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Iterator;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.io.UnsupportedEncodingException;

/**
 * Describes custom attribute instances.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class Attribute {

    //##########################################################################

    private final ConstructorInfo constr;

    private final byte[] value;

    Attribute(ConstructorInfo constr, byte[] value) {
        assert constr != null;
	this.constr = constr;
        assert value != null : constr.toString();
	this.value = value;
    }

    //##########################################################################
    // public interface

    /** @return the type (class) of the attribute. */
    public Type GetType() { return constr.DeclaringType; }

    /** @return the constructor of this attribute. */
    public ConstructorInfo getConstructor() {
	return constr;
    }

    /** @return the Blob with serialized constructor & named arguments. */
    public byte[] getValue() {
	byte[] value = new byte[this.value.length];
	System.arraycopy(this.value, 0, value, 0, value.length);
	return value;
    }

    /**@return an array with the arguments to the attribute's constructor. */
    public Object[] getConstructorArguments() {
        parseBlob();
        Object[] cas = new Object[constrArgs.length];
        System.arraycopy(constrArgs, 0, cas, 0, cas.length);
        return cas;
    }

    /** @return the named argument with the given name. */
    public NamedArgument getNamedArgument(String name) {
        return (NamedArgument)namedArgs.get(name);
    }

    /** @return an array of all named arguments for this attribute. */
    public NamedArgument[] getNamedArguments() {
        NamedArgument[] nargs =
            (NamedArgument[])namedArgs.values().toArray(NamedArgument.EMPTY);
        return nargs;
    }

    /** @return a string representation of this attribute. */
    public String toString() {
        parseBlob();
	ParameterInfo[] params = constr.GetParameters();
	assert params.length == constrArgs.length : this.constr;
        StringBuffer str = new StringBuffer();
	str.append('[');
	str.append(constr.DeclaringType.FullName);
        str.append('(');
        for (int i = 0; i < constrArgs.length; i++) {
            if (i > 0)
                str.append(", ");
	    Type t = params[i].ParameterType;
	    if (t.IsEnum()) {
		str.append('(');
		str.append(t.FullName);
		str.append(')');
	    }
            formatValue(str, constrArgs[i]);
        }
        NamedArgument[] nargs = getNamedArguments();
        for (int i = 0; i < nargs.length; i++) {
            str.append(", ").append(nargs[i]);
            }
        str.append(")]");
	return str.toString();
    }

    //#########################################################################

    private static final Map type2id = new HashMap();
    private static final Map id2type = new HashMap();
    static {
        map("Boolean", Signature.ELEMENT_TYPE_BOOLEAN);
        map("Char",    Signature.ELEMENT_TYPE_CHAR);
        map("SByte",   Signature.ELEMENT_TYPE_I1);
        map("Byte",    Signature.ELEMENT_TYPE_U1);
        map("Int16",   Signature.ELEMENT_TYPE_I2);
        map("UInt16",  Signature.ELEMENT_TYPE_U2);
        map("Int32",   Signature.ELEMENT_TYPE_I4);
        map("UInt32",  Signature.ELEMENT_TYPE_U4);
        map("Int64",   Signature.ELEMENT_TYPE_I8);
        map("UInt64",  Signature.ELEMENT_TYPE_U8);
        map("Single",  Signature.ELEMENT_TYPE_R4);
        map("Double",  Signature.ELEMENT_TYPE_R8);
        map("String",  Signature.ELEMENT_TYPE_STRING);
        map("Type",    Signature.X_ELEMENT_TYPE_TYPE);
        map("Object",  Signature.ELEMENT_TYPE_OBJECT);
    }
    private static void map(String type, int id) {
        Type t = Type.GetType("System." + type);
        assert type != null : type + " -> " + id;
        Integer i = new Integer(id);
        type2id.put(t, i);
        id2type.put(i, t);
    }
    private static int getTypeId(Type type) {
        Integer id = (Integer)type2id.get(type);
        assert id != null : type;
        return id.intValue();
    }

    private Object[] constrArgs;
    private Map namedArgs;
    private ByteBuffer buf;

    private void parseBlob() {
        try { parseBlob0(); }
        catch (RuntimeException e) {
            throw new RuntimeException(PEFile.bytes2hex(value), e);
        }
    }

    private void parseBlob0() {
        if (buf != null)
            return;
        buf = ByteBuffer.wrap(value);                                   // Sec. 23.3 in Partition II of CLR Spec.
        buf.order(ByteOrder.LITTLE_ENDIAN);

        short sig = buf.getShort();                                     // Prolog
        assert sig == 1 : PEFile.bytes2hex(value);
        ParameterInfo[] params = constr.GetParameters();
        constrArgs = new Object[params.length];
        for (int i = 0; i < params.length; i++) {
            constrArgs[i] = parseFixedArg(params[i].ParameterType);     // FixedArg
        }

        int ncount = buf.getShort();                                   // NumNamed
        namedArgs = new LinkedHashMap();
        for (int i = 0; i < ncount; i++) {
            int designator = buf.get();                                // designator one of 0x53 (FIELD) or 0x54 (PROPERTY)
            assert designator == Signature.X_ELEMENT_KIND_FIELD
                || designator == Signature.X_ELEMENT_KIND_PROPERTY
                : "0x" + PEFile.byte2hex(designator);
            Type type = parseFieldOrPropTypeInNamedArg();              // FieldOrPropType
            String name = parseString();                               // FieldOrPropName
            Object value = parseFixedArg(type);                        // FixedArg
            NamedArgument narg =
                new NamedArgument(designator, name, type, value);
            namedArgs.put(name, narg);
        }
    }

    private Object parseFixedArg(Type type) {
      if (type.IsArray())
	    return parseArray(type.GetElementType());
	  else
        return parseElem(type);
    }

    /* indicates whether the "simple" case (the other is "enum") of the first row
       in the Elem production should be taken. */
    private boolean isSimpleElem(Type type) {
        if(!type2id.containsKey(type)) return false;
        int id = getTypeId(type);
        switch(id){
            case Signature.ELEMENT_TYPE_STRING:
            case Signature.X_ELEMENT_TYPE_TYPE:
            case Signature.ELEMENT_TYPE_OBJECT:
                return false;
            default:
                return true;
        }
    }

    /* indicates whether the second row in the Elem production
       should be taken (and more specifically, "string" case within that row). */
    private boolean isStringElem(Type type) {
        if(!type2id.containsKey(type)) return false;
        int id = getTypeId(type);
        return id == Signature.ELEMENT_TYPE_STRING;
    }

    /* indicates whether the second row in the Elem production
       should be taken (and more specifically, "type" case within that row). */
    private boolean isTypeElem(Type type) {
        if(!type2id.containsKey(type)) return false;
        int id = getTypeId(type);
        return id == Signature.X_ELEMENT_TYPE_TYPE;
    }

    /* indicates whether the third row in the Elem production
       should be taken (and more specifically, "boxed" case within that row). */
    private boolean isSystemObject(Type type) {
        if(!type2id.containsKey(type)) return false;
        int id = getTypeId(type);
        return id == Signature.ELEMENT_TYPE_OBJECT;
    }

    private Object parseElem(Type type) {
       // simple or enum
       if (isSimpleElem(type)) return parseVal(getTypeId(type));
       if (type.IsEnum())      return parseVal(getTypeId(type.getUnderlyingType()));
       // string or type
       if (isStringElem(type)) return parseString();
       if (isTypeElem(type))   return getTypeFromSerString();
       // boxed valuetype, please notice that a "simple" boxed valuetype is preceded by 0x51
       if (isSystemObject(type)) {
           Type boxedT = parse0x51();
           if(boxedT.IsEnum()) {
               return new BoxedArgument(boxedT, parseVal(getTypeId(boxedT.getUnderlyingType())));
           } else {
               return new BoxedArgument(boxedT, parseVal(getTypeId(boxedT))); // TODO dead code?
           }
       } else {
           Type boxedT = parseType();
           return parseVal(getTypeId(boxedT));
       }
    }

    /* this does not parse an Elem, but a made-up production (Element). Don't read too much into this method name! */
    private Object parseVal(int id) {
        switch (id) {
        case Signature.ELEMENT_TYPE_BOOLEAN:
            return new Boolean(buf.get() == 0 ? false : true);
        case Signature.ELEMENT_TYPE_CHAR:
            return new Character(buf.getChar());
        case Signature.ELEMENT_TYPE_I1:
        case Signature.ELEMENT_TYPE_U1:
            return new Byte(buf.get());       // TODO U1 not the same as I1
        case Signature.ELEMENT_TYPE_I2:
        case Signature.ELEMENT_TYPE_U2:
            return new Short(buf.getShort()); // TODO U2 not the same as I2
        case Signature.ELEMENT_TYPE_I4:
        case Signature.ELEMENT_TYPE_U4:
            return new Integer(buf.getInt()); // TODO U4 not the same as I4
        case Signature.ELEMENT_TYPE_I8:
        case Signature.ELEMENT_TYPE_U8:
            return new Long(buf.getLong());   // TODO U8 not the same as I8
        case Signature.ELEMENT_TYPE_R4:
            return new Float(buf.getFloat());
        case Signature.ELEMENT_TYPE_R8:
            return new Double(buf.getDouble());
        case Signature.X_ELEMENT_TYPE_TYPE:
            return getTypeFromSerString();
        case Signature.ELEMENT_TYPE_STRING:
            return parseString();
        default:
            throw new RuntimeException("Shouldn't have called parseVal with: " + id);
        }
    }

    private Object parseArray(Type type) {
	if (type.IsEnum())
	    return parseArray(type.getUnderlyingType());
	return parseArray(getTypeId(type));
    }

    private Object parseArray(int id) {
        switch (id) {
        case Signature.ELEMENT_TYPE_BOOLEAN:
            return parseBooleanArray();
        case Signature.ELEMENT_TYPE_CHAR:
            return parseCharArray();
        case Signature.ELEMENT_TYPE_I1:
        case Signature.ELEMENT_TYPE_U1:    // TODO U1 not the same as I1
            return parseByteArray();
        case Signature.ELEMENT_TYPE_I2:
        case Signature.ELEMENT_TYPE_U2:
            return parseShortArray();
        case Signature.ELEMENT_TYPE_I4:
        case Signature.ELEMENT_TYPE_U4:
            return parseIntArray();
        case Signature.ELEMENT_TYPE_I8:
        case Signature.ELEMENT_TYPE_U8:
            return parseLongArray();
        case Signature.ELEMENT_TYPE_R4:
            return parseFloatArray();
        case Signature.ELEMENT_TYPE_R8:
            return parseDoubleArray();
        case Signature.ELEMENT_TYPE_STRING:
            return parseStringArray();
        case Signature.X_ELEMENT_TYPE_ENUM:
	    return parseArray(getTypeFromSerString());
        default:
            throw new RuntimeException("Unknown type id: " + id);
        }
    }

    private Type parseType() { // FieldOrPropType, Sec. 23.3 in Partition II of CLR Spec.
        int id = buf.get();
        switch (id) {
        case Signature.ELEMENT_TYPE_SZARRAY:
            Type arrT = Type.mkArray(parseType(), 1);
            return arrT;
        case Signature.X_ELEMENT_TYPE_ENUM:
            String enumName = parseString();
            Type enumT = Type.getType(enumName);
            return enumT;
        default:
            Type t = (Type)id2type.get(new Integer(id));
            assert t != null : PEFile.byte2hex(id);
            return t;
        }
    }

    private Type parse0x51() {
        int id = buf.get();
        switch (id) {
        case 0x51:
            return parse0x51();
        case Signature.ELEMENT_TYPE_SZARRAY:
            Type arrT = Type.mkArray(parseType(), 1);
            return arrT;
        case Signature.X_ELEMENT_TYPE_ENUM:
            String enumName = parseString();
            Type enumT = Type.getType(enumName);
            return enumT;
        default:
            Type t = (Type)id2type.get(new Integer(id));
            assert t != null : PEFile.byte2hex(id);
            return t;
        }
    }


    private Type parseFieldOrPropTypeInNamedArg() { // FieldOrPropType, Sec. 23.3 in Partition II of CLR Spec.
        int id = buf.get();
        switch (id) {
        case 0x51:
            return (Type)(id2type.get(new Integer(Signature.ELEMENT_TYPE_OBJECT)));
        // TODO remove case Signature.ELEMENT_TYPE_SZARRAY:
            // Type arrT = Type.mkArray(parseType(), 1);
            // return arrT;
        case Signature.X_ELEMENT_TYPE_ENUM:
            String enumName = parseString();
            Type enumT = Type.getType(enumName); // TODO this "lookup" only covers already-loaded assemblies.
            return enumT; // TODO null as return value (due to the above) spells trouble later.
        default:
            Type t = (Type)id2type.get(new Integer(id));
            assert t != null : PEFile.byte2hex(id);
            return t;
        }
    }

    private Type getTypeFromSerString() {
        String typename = parseString();
        int i = typename.indexOf(',');
        /* fully qualified assembly name follows. Just strip it on the assumption that
           the assembly is referenced in the externs and the type will be found. */
        String name = (i < 0) ? typename : typename.substring(0, i);
        Type t = Type.GetType(name);
        if (t == null && i > 0) {
            int j = typename.indexOf(',', i + 1);
            if (j > 0) {
                String assemName = typename.substring(i + 1, j);
                try {
                    Assembly.LoadFrom(assemName);
                } catch (Throwable e) {
                    throw new RuntimeException(typename, e);
                }
                t = Type.GetType(name);
            }
        }
        assert t != null : typename;
        return t;
    }

    private boolean[] parseBooleanArray() {
        boolean[] arr = new boolean[buf.getInt()];
        for (int i = 0; i < arr.length; i++)
            arr[i] = buf.get() == 0 ? false : true;
        return arr;
    }

    private char[] parseCharArray() {
        char[] arr = new char[buf.getInt()];
        for (int i = 0; i < arr.length; i++)
            arr[i] = buf.getChar();
        return arr;
    }

    private byte[] parseByteArray() {
        byte[] arr = new byte[buf.getInt()];
        for (int i = 0; i < arr.length; i++)
            arr[i] = buf.get();
        return arr;
    }

    private short[] parseShortArray() {
        short[] arr = new short[buf.getInt()];
        for (int i = 0; i < arr.length; i++)
            arr[i] = buf.getShort();
        return arr;
    }

    private int[] parseIntArray() {
        int[] arr = new int[buf.getInt()];
        for (int i = 0; i < arr.length; i++)
            arr[i] = buf.getInt();
        return arr;
    }

    private long[] parseLongArray() {
        long[] arr = new long[buf.getInt()];
        for (int i = 0; i < arr.length; i++)
            arr[i] = buf.getLong();
        return arr;
    }

    private float[] parseFloatArray() {
        float[] arr = new float[buf.getInt()];
        for (int i = 0; i < arr.length; i++)
            arr[i] = buf.getFloat();
        return arr;
    }

    private double[] parseDoubleArray() {
        double[] arr = new double[buf.getInt()];
        for (int i = 0; i < arr.length; i++)
            arr[i] = buf.getDouble();
        return arr;
    }

    private String[] parseStringArray() {
        String[] arr = new String[buf.getInt()];
        for (int i = 0; i < arr.length; i++)
            arr[i] = parseString();
        return arr;
    }

    private String parseString() { // SerString convention
        String str = null;
        int length = parseLength();
        if (length < 0)
            return null;
        try { str = new String(value, buf.position(), length, "UTF-8" ); }
        catch (UnsupportedEncodingException e) { throw new Error(e); }
        buf.position(buf.position() + length);
        return str;
    }

    private int getByte() {
        return (buf.get() + 0x0100) & 0xff;
    }

    public int parseLength() {
	int length = getByte();
        // check for invalid length format: the first, second or third
        // most significant bits should be 0; if all are 1 the length is invalid.
        if ((length & 0xe0) == 0xe0)
            return -1;
	if ((length & 0x80) != 0) {
	    length = ((length & 0x7f) << 8) | getByte();
	    if ((length & 0x4000) != 0)
		length = ((length & 0x3fff) << 16) | (getByte()<<8) | getByte();
	}
	return length;
    }

    //##########################################################################
    private static void formatValue(StringBuffer str, Object o) {
        Class c = (o == null) ? null : o.getClass();
        if (c == null) {
            str.append("<null>");
        } else if (c == String.class) {
            str.append('"');
            str.append(o);
            str.append('"');
        } else if (c == Character.class) {
            str.append('\'');
            str.append(o);
            str.append('\'');
        } else if (c == boolean[].class) {
            str.append("new boolean[] {");
            boolean[] arr = (boolean[])o;
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) str.append(", ");
                str.append(arr[i]);
            }
            str.append('}');
        } else if (c == char[].class) {
            str.append("new short[] {");
            short[] arr = (short[])o;
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) str.append(", ");
                str.append(arr[i]);
            }
            str.append('}');
        } else if (c == byte[].class) {
            str.append("new byte[] {");
            byte[] arr = (byte[])o;
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) str.append(", ");
                str.append(arr[i]);
            }
            str.append('}');
        } else if (c == short[].class) {
            str.append("new short[] {");
            short[] arr = (short[])o;
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) str.append(", ");
                str.append(arr[i]);
            }
            str.append('}');
        } else if (c == int[].class) {
            str.append("new int[] {");
            int[] arr = (int[])o;
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) str.append(", ");
                str.append(arr[i]);
            }
            str.append('}');
        } else if (c == long[].class) {
            str.append("new long[] {");
            long[] arr = (long[])o;
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) str.append(", ");
                str.append(arr[i]);
            }
            str.append('}');
        } else if (c == float[].class) {
            str.append("new float[] {");
            float[] arr = (float[])o;
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) str.append(", ");
                str.append(arr[i]);
            }
            str.append('}');
        } else if (c == double[].class) {
            str.append("new double[] {");
            double[] arr = (double[])o;
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) str.append(", ");
                str.append(arr[i]);
            }
            str.append('}');
        } else if (c == String[].class) {
            str.append("new String[] {");
            String[] arr = (String[])o;
            for (int i = 0; i < arr.length; i++) {
                if (i > 0) str.append(", ");
                formatValue(str, arr[i]);
            }
            str.append('}');
	} else if (o instanceof Type) {
	    str.append("typeof(");
	    str.append(o);
	    str.append(")");
        } else
            str.append(o);
    }

    //##########################################################################

    /** Represents named arguments (assigned outside of the constructor)
     *  of a custom attribute
     */
    public static class NamedArgument {

        /** Designates if the named argument corresponds to a field or property.
         *  Possible values:
         *      Signature.X_ELEMENT_KIND_FIELD = 0x53
         *      Signature.X_ELEMENT_KIND_PROPERTY = 0x54
         */
        public final int designator;

        /** The name of the field/property. */
        public final String name;

        /** Type of the field/property. */
        public final Type type;

        /** The value for the field/property. */
        public final Object value;

        /** An empty array NamedArgument. */
        public static final NamedArgument[] EMPTY = new NamedArgument[0];

        public NamedArgument(int designator, String name,Type type,Object value)
        {
            this.designator = designator;
            this.name = name;
            this.type = type;
            this.value = value;
        }

        /** @return <b>true</b> if the named argument specifies a field;
         *  <b>false<b> otherwise.
         */
        public boolean isField() {
            return designator == Signature.X_ELEMENT_KIND_FIELD;
        }

        /** @return <b>true</b> if the named argument specifies a property;
         *  <b>false<b> otherwise.
         */
        public boolean isProperty() {
            return designator == Signature.X_ELEMENT_KIND_PROPERTY;
        }

        /** @return a string representation of the named argument. */
        public String toString() {
            StringBuffer str = new StringBuffer(name);
            str.append(" = ");
            if (type.IsEnum())
                str.append('(').append(type.FullName).append(')');
            formatValue(str, value);
            return str.toString();
        }
    }

    //##########################################################################

    public static class BoxedArgument {
        public final Type type;
        public final Object value;
        public BoxedArgument(Type type, Object value) {
            this.type = type; this.value = value;
        }
        public String toString() {
            return "(" + type.FullName + ")" + value;
        }
    }

    //##########################################################################

}  // class Attribute
