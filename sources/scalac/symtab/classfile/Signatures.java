/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.symtab.classfile;

import scala.tools.util.Position;
import scalac.Global;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.AbstractFileReader;
import scalac.util.Name;
import scalac.util.SourceRepresentation;

/** This class implements the parsing of class file signatures. */
public class Signatures {

    //########################################################################
    // Private Fields

    /** The global environment */
    private final Global global;

    /** The Java type factory */
    private final JavaTypeFactory make;

    /** The input file */
    private final AbstractFileReader in;

    /** The address of the first byte of the current signature */
    private int first;

    /** The address of the last byte of the current signature */
    private int last;

    /** The current address (first <= current <= last) */
    private int current;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public Signatures(Global global, JavaTypeFactory make,
        AbstractFileReader in)
    {
        this.global = global;
        this.make = make;
        this.in = in;
    }

    //########################################################################
    // Public Methods

    /**
     * Sets the address of the next signature to read. The address
     * must point to the first byte of a CONSTANT_Utf8_info.
     */
    public Signatures at(int address) {
        first = address + 3;
        last = first + in.getChar(address + 1) - 1;
        current = first;
        return this;
    }

    /** Returns the current signature. */
    public String getSignature() {
        return SourceRepresentation.ascii2string(in.buf, first, last-first+1);
    }

    /** Reads the class signature at current address. */
    public Symbol readClassName() {
        Symbol owner = global.definitions.ROOT_CLASS;
        int start = current;
        for (; current <= last; current++) {
            int b = in.byteAt(current);
            if (b == ';') break;
            if (b != '/') continue;
            Name name = Name.fromAscii(in.buf, start, current - start);
            Symbol module = owner.members().lookup(name);
            if (!module.isModule()) {
                Symbol symbol = owner.newModule(Position.NOPOS, 0, name);
                symbol.moduleClass().setInfo(Type.ErrorType);
                error("could not find module " + symbol.staticType());
                if (module.isNone()) owner.members().enterNoHide(symbol);
                module = symbol;
            }
            owner = module.moduleClass();
            start = current + 1;
        }
        Name name = Name.fromAscii(in.buf, start, current-start).toTypeName();
        Symbol clasz = owner.members().lookup(name);
        if (!clasz.isClass()) {
            Symbol symbol = owner.newClass(Position.NOPOS, 0, name);
            symbol.setInfo(Type.ErrorType);
            symbol.allConstructors().setInfo(Type.ErrorType);
            error("could not find class " + symbol.staticType());
            if (clasz.isNone()) owner.members().enterNoHide(symbol);
            clasz = symbol;
        }
        current++;
        return clasz;
    }

    /** Reads the value type signature at current address. */
    public Type readValueType() {
        switch (in.byteAt(current++)) {
        case 'V': return make.voidType();
        case 'Z': return make.booleanType();
        case 'B': return make.byteType();
        case 'S': return make.shortType();
        case 'C': return make.charType();
        case 'I': return make.intType();
        case 'J': return make.longType();
        case 'F': return make.floatType();
        case 'D': return make.doubleType();
        case 'L': return make.classType(readClassName());
        case '[': return make.arrayType(readValueType());
        default : return errorBadTypeTag(current - 1);
        }
    }

    /** Reads the method type signature at current address. */
    public Type readMethodType() {
        if (in.byteAt(current++) != '(') return errorBadTypeTag(current - 1);
        Type[] parameters = readParamterTypes(0);
        Type result = readValueType();
        return make.methodType(parameters, result, Type.EMPTY_ARRAY);
    }

    //########################################################################
    // Private Methods

    /** Reads the parameter types at current address. */
    private Type[] readParamterTypes(int i) {
        if (in.byteAt(current) == ')') {
            current++;
            return new Type[i];
        } else {
            Type type = readValueType();
            Type[] types = readParamterTypes(i + 1);
            types[i] = type;
            return types;
        }
    }

    /** Signals a bad tag at given address. Return ErrorType. */
    private Type errorBadTypeTag(int address) {
        char tag = (char)in.byteAt(address);
        error("bad tag '" + tag + "' in signature '" + getSignature() + "'");
        return Type.ErrorType;
    }

    /** Signals the given error. */
    private void error(String error) {
        global.error("class file '" + in.path + "': " + error);
    }

    //########################################################################
}
