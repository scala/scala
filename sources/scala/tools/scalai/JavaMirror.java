/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: JavaMirror.java,v 1.8 2002/10/01 16:14:07 paltherr Exp $
// $Id$

package scala.tools.scalai;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.util.Map;
import java.util.HashMap;

import scalac.symtab.Kinds;
import scalac.symtab.TypeTags;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.Definitions;
import scalac.util.Debug;

public class JavaMirror {

    //########################################################################
    // Private Constants

    private static final Class void_class    = Void.TYPE;
    private static final Class boolean_class = Boolean.TYPE;
    private static final Class byte_class    = Byte.TYPE;
    private static final Class short_class   = Short.TYPE;
    private static final Class char_class    = Character.TYPE;
    private static final Class int_class     = Integer.TYPE;
    private static final Class long_class    = Long.TYPE;
    private static final Class float_class   = Float.TYPE;
    private static final Class double_class  = Double.TYPE;
    private static final Class Object_class  = Object.class;

    //########################################################################
    // Private Fields

    private final ClassLoader loader;

    private final Map/*<Class ,Class      >*/ arrays;
    private final Map/*<Symbol,Class      >*/ classes;
    private final Map/*<Symbol,Field      >*/ fields;
    private final Map/*<Symbol,Method     >*/ methods;
    private final Map/*<Symbol,Constructor>*/ constructors;

    //########################################################################
    // Public Constructors

    public JavaMirror(Definitions definitions, ClassLoader loader) {
        this.loader = loader;
        this.arrays = new HashMap();
        this.classes = new HashMap();
        this.fields = new HashMap();
        this.methods = new HashMap();
        this.constructors = new HashMap();
        this.classes.put(definitions.ANY_CLASS, Object_class);
        this.classes.put(definitions.ANYREF_CLASS, Object_class);
    }

    //########################################################################
    // Public Methods - arrays

    public Class getArray(Class component) {
        Object value = arrays.get(component);
        if (value != null) return (Class)value;
        Class array = getArray0(component);
        arrays.put(component, array);
        return array;
    }

    private Class getArray0(Class component) {
        String classname = "[" + getArrayComponentName(component);
        try {
            return Class.forName(classname, false, loader);
        } catch (ClassNotFoundException exception) {
            throw Debug.abort("no such class", classname);
        }
    }

    private String getArrayComponentName(Class component) {
        if (component.isPrimitive()) {
            if (component == boolean_class) return "Z";
            if (component == byte_class) return "B";
            if (component == short_class) return "S";
            if (component == char_class) return "C";
            if (component == int_class) return "I";
            if (component == long_class) return "J";
            if (component == float_class) return "F";
            if (component == double_class) return "D";
            throw Debug.abort("unknown primitive class", component);
        }
        String classname = component.getName();
        return component.isArray() ? classname : "L" + classname + ";";
    }

    //########################################################################
    // Public Methods - classes

    public Class getClass(Type type) {
        switch (type) {

        case UnboxedType(int kind):
            return getClass(kind);

        case UnboxedArrayType(Type component):
            return getArray(getClass(component));

        case TypeRef(_, Symbol symbol, _):
            return getClass(symbol);

        default:
            throw Debug.abort("illegal type", type);
        }
    }

    public Class getClass(int kind) {
        switch (kind) {
        case TypeTags.UNIT   : return void_class;
        case TypeTags.BOOLEAN: return boolean_class;
        case TypeTags.BYTE   : return byte_class;
        case TypeTags.SHORT  : return short_class;
        case TypeTags.CHAR   : return char_class;
        case TypeTags.INT    : return int_class;
        case TypeTags.LONG   : return long_class;
        case TypeTags.FLOAT  : return float_class;
        case TypeTags.DOUBLE : return double_class;
        default              : throw Debug.abort("kind = " + kind);
        }
    }

    public Class getClass(Symbol symbol) {
        Object value = classes.get(symbol);
        if (value != null) return (Class)value;
        Class mirror = getClass0(symbol);
        assert Debug.log("java mirror: ", symbol, " -> ", mirror);
        classes.put(symbol, mirror);
        return mirror;
    }

    private Class getClass0(Symbol symbol) {
        String name = getClassName(symbol, false);
        try {
            return Class.forName(name, false, loader);
        } catch (ClassNotFoundException exception) {
            throw Debug.abort("no such class", Debug.show(symbol," - ",name));
        }
    }

    private String getClassName(Symbol symbol, boolean asPrefix) {
        assert symbol.kind == Kinds.CLASS : Debug.show(symbol);
        String name = getPrefix(symbol.owner()) + symbol.name;
        if (!asPrefix && !symbol.isJava() && symbol.isModuleClass())
            name = name + '$';
        return name;
    }

    private String getPrefix(Symbol symbol) {
        assert symbol.kind == Kinds.CLASS : Debug.show(symbol);
        if (symbol.isRoot()) return "";
        String prefix = getClassName(symbol, true);
        return prefix + (symbol.isClass() ? '$' : '.');
    }

    //########################################################################
    // Public Methods - fields

    public Field getField(Symbol symbol) {
        Object value = fields.get(symbol);
        if (value != null) return (Field)value;
        Field mirror = getField0(symbol);
        assert Debug.log("java mirror: ", symbol, " -> ", mirror);
        fields.put(symbol, mirror);
        return mirror;
    }

    private Field getField0(Symbol symbol) {
        if (symbol.isModule()) {
            assert !symbol.isJava() : Debug.show(symbol);
            Class owner = getClass0(symbol.moduleClass());
            return getField0(symbol, owner, "MODULE$");
        } else {
            Class owner = getClass(symbol.owner());
            return getField0(symbol, owner, symbol.name.toString());
        }
    }

    private Field getField0(Symbol symbol, Class owner, String name) {
        try {
            return owner.getField(name);
        } catch (NoSuchFieldException exception) {
            throw Debug.abort("no such field", symbol);
        }
    }

    //########################################################################
    // Public Methods - methods

    public Method getMethod(Symbol symbol) {
        Object value = methods.get(symbol);
        if (value != null) return (Method)value;
        Method mirror = getMethod0(symbol);
        assert Debug.log("java mirror: ", symbol, " -> ", mirror);
        methods.put(symbol, mirror);
        return mirror;
    }

    private Method getMethod0(Symbol symbol) {
        Class owner = getClass(symbol.owner());
        Class[] params = getVParamsOf(symbol.type());
        try {
            return owner.getMethod(symbol.name.toString(), params);
        } catch (NoSuchMethodException exception) {
            throw Debug.abort("no such method", symbol);
        }
    }

    //########################################################################
    // Public Methods - constructors

    public Constructor getConstructor(Symbol symbol) {
        Object value = constructors.get(symbol);
        if (value != null) return (Constructor)value;
        Constructor mirror = getConstructor0(symbol);
        assert Debug.log("java mirror: ", symbol, " -> ", mirror);
        constructors.put(symbol, mirror);
        return mirror;
    }

    private Constructor getConstructor0(Symbol symbol) {
        Class owner = getClass(symbol.owner());
        Class[] params = getVParamsOf(symbol.type());
        try {
            return owner.getConstructor(params);
        } catch (NoSuchMethodException exception) {
            throw Debug.abort("no such constructor", symbol);
        }
    }

    //########################################################################
    // Public Methods - value parameters

    public Class[] getVParamsOf(Symbol symbol) {
        return getVParamsOf(symbol.type());
    }

    public Class[] getVParamsOf(Type type) {
        switch (type) {

        case MethodType(Symbol[] vparams, _):
            return getVParams(vparams);

        default:
            throw Debug.abort("illegal type", type);
        }
    }

    public Class[] getVParams(Symbol[] symbols) {
        Class[] vparams = new Class[symbols.length];
        for (int i = 0; i < vparams.length; i++) {
            vparams[i] = getClass(symbols[i].type());
        }
        return vparams;
    }

    //########################################################################
}
