/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import ch.epfl.lamp.util.Position;

import scalac.Global;
import scalac.symtab.Definitions;
import scalac.symtab.Modifiers;
import scalac.symtab.Symbol;
import scalac.symtab.TermSymbol;
import scalac.symtab.Type;
import scalac.util.Debug;
import scalac.util.Name;

/** This class implements an attributed tree typer. */
public class ATreeTyper {

    //########################################################################
    // Private Fields

    /** The global environment */
    public final Global global;

    /** The global definitions */
    private final Definitions definitions;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ATreeTyper(Global global, Definitions definitions) {
        this.global = global;
        this.definitions = definitions;
    }

    //########################################################################
    // Public Methods - Typing code

    /** Returns the types of the given codes. */
    public Type[] type(ACode[] codes) {
        Type[] types = new Type[codes.length];
        for (int i = 0; i < types.length; i++) types[i] = type(codes[i]);
        return types;
    }

    /** Returns the type of the given code. */
    public Type type(ACode code) {
        switch (code) {
        case Void:
            return definitions.UNIT_TYPE();
        case This(Symbol clasz):
            return clasz.thisType();
        case Constant(AConstant constant):
            return type(constant);
        case Load(ALocation location):
            return type(location);
        case Store(_, _):
            return Type.NoType;
        case Apply(AFunction function, Type[] targs, _):
            return apply(type(function), targs).resultType();
        case IsAs(_, Type type, boolean cast):
            return cast ? type : definitions.BOOLEAN_TYPE();
        case If(_, ACode success, ACode failure):
            return Type.lub(new Type[]{type(success), type(failure)});
        case Switch(_, _, ACode[] bodies):
            return Type.lub(type(bodies));
        case Synchronized(_, ACode value):
            return type(value);
        case Block(_, _, ACode value):
            return type(value);
        case Label(Symbol label, _, _):
            return label.type().resultType();
        case Goto(_, _):
            return Type.NoType;
        case Return(_, _):
            return Type.NoType;
        case Throw(_):
            return Type.NoType;
        case Drop(_, _):
            return Type.NoType;
        default:
            throw Debug.abort("unknown case", code);
        }
    }

    //########################################################################
    // Public Methods - Typing value locations

    /** Returns the type of the given value location. */
    public Type type(ALocation location) {
        switch (location) {
        case Module(Symbol module):
            return module.thisType();
        case Field(Void, Symbol field, _):
            return field.owner().thisType().memberStabilizedType(field);
        case Field(ACode object, Symbol field, _):
            return type(object).memberStabilizedType(field);
        case Local(Symbol local, _):
            return local.type();
        case ArrayItem(ACode array, _):
            return getArrayElementType(type(array));
        default:
            throw Debug.abort("unknown case", location);
        }
    }

    //########################################################################
    // Public Methods - Typing function references

    /** Returns the type of the given function reference. */
    public Type type(AFunction function) {
        switch (function) {
        case Method(Void, Symbol method, AInvokeStyle style):
            Type type = method.owner().thisType().memberStabilizedType(method);
            if (style ==  AInvokeStyle.New) {
                assert method.isInitializer(): function;
                Symbol[] tparams = method.owner().typeParams();
                if (tparams.length != 0) type = Type.PolyType(tparams, type);
            }
            return type;
        case Method(ACode object, Symbol method, _):
            return type(object).memberStabilizedType(method);
        case Primitive(APrimitive primitive):
            return type(primitive);
        case NewArray(Type element):
            return definitions.ARRAY_TYPE(element);
        default:
            throw Debug.abort("unknown case", function);
        }
    }

    //########################################################################
    // Public Methods - Typing primitives

    /** Returns the type of the given primitive. */
    public Type type(APrimitive primitive) {
        switch (primitive) {
        case Negation(ATypeKind kind):
            Type type = type(kind);
            return getMethodType(type, type);
        case Test(_, ATypeKind kind, true):
            Type type = type(kind);
            return getMethodType(type, type(ATypeKind.BOOL));
        case Test(_, ATypeKind kind, false):
            Type type = type(kind);
            return getMethodType(type, type, type(ATypeKind.BOOL));
        case Comparison(_, ATypeKind kind):
            Type type = type(kind);
            return getMethodType(type, type, type(ATypeKind.I4));
        case Arithmetic(_, ATypeKind kind):
            Type type = type(kind);
            return getMethodType(type, type, type);
        case Logical(_, ATypeKind kind):
            Type type = type(kind);
            return getMethodType(type, type, type);
        case Shift(_, ATypeKind kind):
            Type type = type(kind);
            return getMethodType(type, type(ATypeKind.I4), type);
        case Conversion(ATypeKind src, ATypeKind dst):
            return getMethodType(type(src), type(dst));
        case ArrayLength(ATypeKind kind):
            Type type = definitions.ARRAY_TYPE(type(kind));
            return getMethodType(type, type(ATypeKind.I4));
        case StringConcat(ATypeKind lf, ATypeKind rg):
            return getMethodType(type(lf), type(rg), type(ATypeKind.STR));
        default:
            throw Debug.abort("unknown case", primitive);
        }
    }

    //########################################################################
    // Public Methods - Typing constants

    /** Returns the type of the given constant. */
    public Type type(AConstant constant) {
        Type base = basetype(constant);
        if (global.currentPhase.id > global.PHASE.ERASURE.id()) return base;
        return Type.ConstantType(base, constant);
    }

    /** Returns the base type of the given constant. */
    public Type basetype(AConstant constant) {
        switch (constant) {
        case UNIT      : return definitions.UNIT_TYPE();
        case BOOLEAN(_): return definitions.BOOLEAN_TYPE();
        case BYTE(_)   : return definitions.BYTE_TYPE();
        case SHORT(_)  : return definitions.SHORT_TYPE();
        case CHAR(_)   : return definitions.CHAR_TYPE();
        case INT(_)    : return definitions.INT_TYPE();
        case LONG(_)   : return definitions.LONG_TYPE();
        case FLOAT(_)  : return definitions.FLOAT_TYPE();
        case DOUBLE(_) : return definitions.DOUBLE_TYPE();
        case STRING(_) : return definitions.STRING_TYPE();
        case NULL      : return definitions.ALLREF_TYPE();
        case ZERO      : return definitions.ALL_TYPE();
        default        : throw Debug.abort("unknown case", constant);
        }
    }

    //########################################################################
    // Public Methods - Typing type kinds

    /** Returns the type of the given type kind. */
    public Type type(ATypeKind kind) {
        switch (kind) {
        case UNIT: return definitions.UNIT_TYPE();
        case BOOL: return definitions.BOOLEAN_TYPE();
 // !!! case U1  : return ?;
        case U2  : return definitions.CHAR_TYPE();
 // !!! case U4  : return ?;
 // !!! case U8  : return ?;
        case I1  : return definitions.BYTE_TYPE();
        case I2  : return definitions.SHORT_TYPE();
        case I4  : return definitions.INT_TYPE();
        case I8  : return definitions.LONG_TYPE();
        case R4  : return definitions.FLOAT_TYPE();
        case R8  : return definitions.DOUBLE_TYPE();
        case REF : return definitions.ANYREF_TYPE();
        case STR : return definitions.STRING_TYPE();
        case NULL: return definitions.ALLREF_TYPE();
        case ZERO: return definitions.ALL_TYPE();
        default  : throw Debug.abort("unknown case", kind);
        }
    }

    //########################################################################
    // Public Methods - Aliases for scala

    public Type[] computeType(ACode[] codes) {
	return type(codes);
    }

    public Type computeType(ACode code) {
	return type(code);
    }

    public Type computeType(ALocation location) {
	return type(location);
    }

    public Type computeType(AFunction function) {
	return type(function);
    }

    public Type computeType(APrimitive primitive) {
	return type(primitive);
    }

    public Type computeType(AConstant constant) {
	return type(constant);
    }

    public Type computeType(ATypeKind kind) {
	return type(kind);
    }

    //########################################################################
    // Private Methods

    /** Returns the application of given arguments to given type. */
    private Type apply(Type type, Type[] targs) {
        switch (type) {
        case PolyType(Symbol[] tparams, Type result):
            return result.subst(tparams, targs);
        default:
            assert targs.length == 0: type + " -- " + Debug.show(targs);
            return type;
        }
    }

    /** Returns the element type of the given array type. */
    public Type getArrayElementType(Type type) { // !!! public / private
        switch (type) {
        case TypeRef(_, Symbol symbol, Type[] args):
            assert symbol == definitions.ARRAY_CLASS && args.length == 1: type;
            return args[0];
        case UnboxedArrayType(Type element):
            return element;
        default:
            throw Debug.abort("non-array type", type);
        }
    }

    /** Returns a method type with given argument and result types. */
    private Type getMethodType(Type targ1, Type result) {
        return getMethodType(new Type[]{targ1}, result);
    }

    /** Returns a method type with given argument and result types. */
    private Type getMethodType(Type targ1, Type targ2, Type result) {
        return getMethodType(new Type[]{targ1, targ2}, result);
    }

    /** Returns a method type with given argument and result types. */
    private Type getMethodType(Type[] targs, Type result) {
        Symbol[] tparams = new Symbol[targs.length];
        for (int i = 0; i < tparams.length; i++) {
            Name name = Name.fromString("v" + i);
            tparams[i] = new TermSymbol(
                Position.NOPOS, name, Symbol.NONE, Modifiers.PARAM);
            tparams[i].setType(targs[i]);
        }
        return Type.MethodType(tparams, result);
    }

    //########################################################################
}
