/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import scalac.atree.AConstant;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.AbstractFileReader;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.SourceRepresentation;

/** This class implements the parsing of class file constant pools. */
public class ConstantPool implements ClassfileConstants {

    //########################################################################
    // Private Fields

    /** The input file */
    private final AbstractFileReader in;

    /** The signature parser */
    private final Signatures parser;

    /** The start addresses of all constants */
    private final int[] starts;

    /** The values of the constants (or null if not yet read) */
    private final Object[] values;

    //########################################################################
    // Public Constructors

    /** Initializes this instance by reading constant pool in file. */
    public ConstantPool(AbstractFileReader in, Signatures parser) {
        this.in = in;
        this.parser = parser;
        this.starts = new int[in.nextChar()];
        this.values = new Object[starts.length];
        for (int index = 1; index < starts.length; ) {
            starts[index++] = in.bp;
            switch (in.nextByte()) {
            case CONSTANT_UTF8:
            case CONSTANT_UNICODE:
                in.skip(in.nextChar());
                continue;
            case CONSTANT_CLASS:
            case CONSTANT_STRING:
                in.skip(2);
                continue;
            case CONSTANT_FIELDREF:
            case CONSTANT_METHODREF:
            case CONSTANT_INTFMETHODREF:
            case CONSTANT_NAMEANDTYPE:
            case CONSTANT_INTEGER:
            case CONSTANT_FLOAT:
                in.skip(4);
                continue;
            case CONSTANT_LONG:
            case CONSTANT_DOUBLE:
                in.skip(8);
                index++;
                continue;
            default:
                throw errorBadTag(in.bp - 1);
            }
        }
    }

    //########################################################################
    // Public Methods

    /** Returns the string at given index. */
    public String getString(int index) {
        if (index <= 0 || starts.length <= index) throw errorBadIndex(index);
        if (values[index] instanceof String) return (String)values[index];
        if (values[index] instanceof Name) return values[index].toString();
        String value = readString(starts[index]);
        values[index] = value;
        return value;
    }

    /** Returns the name at given index. */
    public Name getName(int index) {
        if (index <= 0 || starts.length <= index) throw errorBadIndex(index);
        if (values[index] instanceof Name) return (Name)values[index];
        Name value = readName(starts[index]);
        values[index] = value;
        return value;
    }

    /** Returns the class at given index. */
    public Symbol getClass(int index) {
        if (index <= 0 || starts.length <= index) throw errorBadIndex(index);
        if (values[index] instanceof Symbol) return (Symbol)values[index];
        Symbol value = readClass(starts[index]);
        values[index] = value;
        return value;
    }

    /** Returns the field type at given index. */
    public Type getFieldType(int index) {
        if (index <= 0 || starts.length <= index) throw errorBadIndex(index);
        if (values[index] instanceof Type) return (Type)values[index];
        Type value = readFieldType(starts[index]);
        values[index] = value;
        return value;
    }

    /** Returns the method type at given index. */
    public Type getMethodType(int index) {
        if (index <= 0 || starts.length <= index) throw errorBadIndex(index);
        if (values[index] instanceof Type) return clone((Type)values[index]);
        Type value = readMethodType(starts[index]);
        values[index] = value;
        return value;
    }

    /** Returns the constant value at given index. */
    public AConstant getConstantValue(int index) {
        if (index <= 0 || starts.length <= index) throw errorBadIndex(index);
        if (values[index] != null) return (AConstant)values[index];
        AConstant value = readConstantValue(starts[index]);
        values[index] = value;
        return value;
    }

    //########################################################################
    // Private Fields

    /** Reads the string at given address. */
    private String readString(int address) {
        if (in.byteAt(address) != CONSTANT_UTF8) throw errorBadTag(address);
        return parser.at(address).getSignature();
    }

    /** Reads the name at given address. */
    private Name readName(int address) {
        return Name.fromString(readString(address));
    }

    /** Reads the class at given address. */
    private Symbol readClass(int address) {
        if (in.byteAt(address) != CONSTANT_CLASS) throw errorBadTag(address);
        int index = in.getChar(address + 1);
        if (index <= 0 || starts.length <= index) throw errorBadIndex(index);
        address = starts[index];
        if (in.byteAt(address) != CONSTANT_UTF8) throw errorBadTag(address);
        return parser.at(address).readClassName();
    }

    /** Reads the field type at given address. */
    private Type readFieldType(int address) {
        if (in.byteAt(address) != CONSTANT_UTF8) throw errorBadTag(address);
        return parser.at(address).readValueType();
    }

    /** Reads the method type at given address. */
    private Type readMethodType(int address) {
        if (in.byteAt(address) != CONSTANT_UTF8) throw errorBadTag(address);
        return parser.at(address).readMethodType();
    }

    /** Reads the constant value at given address. */
    private AConstant readConstantValue(int address) {
        switch (in.byteAt(address)) {
        case CONSTANT_STRING:
            return AConstant.STRING(getString(in.getChar(address + 1)));
        case CONSTANT_INTEGER:
            return AConstant.INT(in.getInt(address + 1));
        case CONSTANT_FLOAT:
            return AConstant.FLOAT(in.getFloat(address + 1));
        case CONSTANT_LONG:
            	return AConstant.LONG(in.getLong(address + 1));
        case CONSTANT_DOUBLE:
            return AConstant.DOUBLE(in.getDouble(address + 1));
        default:
            throw errorBadTag(address);
        }
    }

    /** Returns the type with all its parameters symbols cloned. */
    private Type clone(Type type) {
        switch (type) {
        case MethodType(Symbol[] params, Type result):
            Symbol[] clones = new Symbol[params.length];
            for (int i = 0; i < clones.length; i++)
                clones[i] = params[i].cloneSymbol(Symbol.NONE);
            return Type.MethodType(clones, result);
        case ErrorType:
            return type;
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Throws an exception signaling a bad constant index. */
    private RuntimeException errorBadIndex(int index) {
        String error = "bad constant pool index: " + index;
        throw new RuntimeException(error);
    }

    /** Throws an exception signaling a bad tag at given address. */
    private RuntimeException errorBadTag(int address) {
        int tag = in.byteAt(address);
        String error = "bad constant pool tag " + tag + " at byte " + address;
        throw new RuntimeException(error);
    }

    //########################################################################
}
