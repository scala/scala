/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scalac.util;

import java.util.HashMap;

import java.lang.reflect.Modifier;
import java.lang.reflect.Method;
import java.lang.reflect.Field;

import scalac.Global;
import scalac.ApplicationError;
import scalac.ast.Tree;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.Scope;

/**
 * Debugging class, used e.g. to obtain string representations of
 * compiler data structures that are not "pretty-printed" and thus
 * easier to relate to the source code.
 *
 * All methods are static to be easily useable in any context.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public abstract class Debug {

    //########################################################################
    // Debug interface - abort

    public static Error abort() {
        return new ApplicationError();
    }

    public static Error abort(String message) {
        return new ApplicationError(message);
    }

    public static Error abort(Object object) {
        return new ApplicationError(object);
    }

    public static Error abort(Throwable cause) {
        return new ApplicationError(cause);
    }

    public static Error abort(String message, Object object) {
        return new ApplicationError(message, object);
    }

    public static Error abort(String message, Throwable cause) {
        return new ApplicationError(message, cause);
    }

    public static Error abort(Object object, Throwable cause) {
        return new ApplicationError(object, cause);
    }

    public static Error abort(String message, Object object, Throwable cause) {
        return new ApplicationError(message, object, cause);
    }

    //########################################################################
    // Debug interface - log

    public static boolean log(Object a) {
        return logAll(new Object[] {a});
    }

    public static boolean log(Object a, Object b) {
        return logAll(new Object[] {a, b});
    }

    public static boolean log(Object a, Object b, Object c) {
        return logAll(new Object[] {a, b, c});
    }

    public static boolean log(Object a, Object b, Object c, Object d) {
        return logAll(new Object[] {a, b, c, d});
    }

    public static boolean logAll(Object[] args) {
        return Global.instance.log(showAll(args, true));
    }

    //########################################################################
    // Debug interface - handlers

    public static final HashMap/*<Class,DebugHandler>*/ handlers;

    static {
        handlers = new HashMap();
        handlers.put(String.class, DebugToStringHandler.instance);
        handlers.put(Tree.class, DebugToStringHandler.instance);
        handlers.put(Type.class, DebugToStringHandler.instance);
        handlers.put(Symbol.class, DebugSymbol.instance);
        handlers.put(Scope.class, DebugScope.instance);
    }

    public static DebugHandler getHandler(Object that) {
        if (that == null) return DebugDefaultHandler.instance;
        return getHandler(that.getClass());
    }

    public static DebugHandler getHandler(Class clasz) {
        if (clasz == null) return DebugDefaultHandler.instance;
        Object handler = handlers.get(clasz);
        if (handler != null) return (DebugHandler)handler;
        return getHandler(clasz.getSuperclass());
    }

    //########################################################################
    // Debug interface - toString

    public static String toString(Object that) {
        return show(that);
    }

    //########################################################################
    // Debug interface - show

    public static String show(Object a) {
        return showAll(new Object[] {a});
    }

    public static String show(Object a, Object b) {
        return showAll(new Object[] {a, b});
    }

    public static String show(Object a, Object b, Object c) {
        return showAll(new Object[] {a, b, c});
    }

    public static String show(Object a, Object b, Object c, Object d) {
        return showAll(new Object[] {a, b, c, d});
    }

    public static String show(Object a, Object b, Object c, Object d, Object e)
    {
        return showAll(new Object[] {a, b, c, d, e});
    }

    public static String show(Object a, Object b, Object c, Object d, Object e,
        Object f)
    {
        return showAll(new Object[] {a, b, c, d, e, f});
    }

    public static String show(Object a, Object b, Object c, Object d, Object e,
        Object f, Object g)
    {
        return showAll(new Object[] {a, b, c, d, e, f, g});
    }

    public static String show(Object a, Object b, Object c, Object d, Object e,
        Object f, Object g, Object h)
    {
        return showAll(new Object[] {a, b, c, d, e, f, g, h});
    }

    public static String show(Object a, Object b, Object c, Object d, Object e,
        Object f, Object g, Object h, Object i)
    {
        return showAll(new Object[] {a, b, c, d, e, f, g, h, i});
    }

    public static String show(Object a, Object b, Object c, Object d, Object e,
        Object f, Object g, Object h, Object i, Object j)
    {
        return showAll(new Object[] {a, b, c, d, e, f, g, h, i, j});
    }

    public static String show(Object a, Object b, Object c, Object d, Object e,
        Object f, Object g, Object h, Object i, Object j, Object k)
    {
        return showAll(new Object[] {a, b, c, d, e, f, g, h, i, j, k});
    }

    public static String show(Object a, Object b, Object c, Object d, Object e,
        Object f, Object g, Object h, Object i, Object j, Object k, Object l)
    {
        return showAll(new Object[] {a, b, c, d, e, f, g, h, i, j, k, l});
    }

    public static String showAll(Object[] args) {
        return showAll(args, false);
    }

    public static String showAll(Object[] args, boolean nosep) {
        if (args == null) return "null";
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < args.length; i++)
            append(nosep || i == 0 ? buffer : buffer.append(" - "), args[i]);
        return buffer.toString();
    }

    //########################################################################
    // Debug interface - append

    public static void append(StringBuffer buffer, Object that) {
        getHandler(that).append0(buffer, that);
    }

    public static void appendDefault(StringBuffer buffer, Object that) {
        if (that == null) { buffer.append("null"); return; }
        if (!that.getClass().isArray())
            appendObject(buffer, that);
        else if (that instanceof Object [])
            appendArray(buffer, (Object [])that);
        else if (that instanceof boolean[])
            appendArray(buffer, (boolean[])that);
        else if (that instanceof byte   [])
            appendArray(buffer, (byte   [])that);
        else if (that instanceof short  [])
            appendArray(buffer, (short  [])that);
        else if (that instanceof char   [])
            appendArray(buffer, (char   [])that);
        else if (that instanceof int    [])
            appendArray(buffer, (int    [])that);
        else if (that instanceof long   [])
            appendArray(buffer, (long   [])that);
        else if (that instanceof float  [])
            appendArray(buffer, (float  [])that);
        else if (that instanceof double [])
            appendArray(buffer, (double [])that);
        else
            appendObject(buffer, that);
    }

    public static void appendObject(StringBuffer buffer, Object that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append(getClassName(that));
        Class owner = null;
        if (hasToString(that.getClass())) {
            buffer.append('(');
            buffer.append(that);
            buffer.append(')');
        } else {
            int code = System.identityHashCode(that);
            buffer.append('@');
            buffer.append(Integer.toHexString(code));
        }
    }

    public static void appendArray(StringBuffer buffer, Object[] that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append('[');
        for (int i = 0; i < that.length; i++) {
            if (i > 0) buffer.append(',');
            append(buffer, that[i]);
        }
        buffer.append(']');
    }

    public static void appendArray(StringBuffer buffer, boolean[] that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append('[');
        for (int i = 0; i < that.length; i++) {
            if (i > 0) buffer.append(',');
            buffer.append(that[i]);
        }
        buffer.append(']');
    }

    public static void appendArray(StringBuffer buffer, byte[] that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append('[');
        for (int i = 0; i < that.length; i++) {
            if (i > 0) buffer.append(',');
            buffer.append(that[i]);
        }
        buffer.append(']');
    }

    public static void appendArray(StringBuffer buffer, short[] that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append('[');
        for (int i = 0; i < that.length; i++) {
            if (i > 0) buffer.append(',');
            buffer.append(that[i]);
        }
        buffer.append(']');
    }

    public static void appendArray(StringBuffer buffer, char[] that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append('[');
        for (int i = 0; i < that.length; i++) {
            if (i > 0) buffer.append(',');
            buffer.append(that[i]);
        }
        buffer.append(']');
    }

    public static void appendArray(StringBuffer buffer, int[] that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append('[');
        for (int i = 0; i < that.length; i++) {
            if (i > 0) buffer.append(',');
            buffer.append(that[i]);
        }
        buffer.append(']');
    }

    public static void appendArray(StringBuffer buffer, long[] that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append('[');
        for (int i = 0; i < that.length; i++) {
            if (i > 0) buffer.append(',');
            buffer.append(that[i]);
        }
        buffer.append(']');
    }

    public static void appendArray(StringBuffer buffer, float[] that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append('[');
        for (int i = 0; i < that.length; i++) {
            if (i > 0) buffer.append(',');
            buffer.append(that[i]);
        }
        buffer.append(']');
    }

    public static void appendArray(StringBuffer buffer, double[] that) {
        if (that == null) { buffer.append("null"); return; }
        buffer.append('[');
        for (int i = 0; i < that.length; i++) {
            if (i > 0) buffer.append(',');
            buffer.append(that[i]);
        }
        buffer.append(']');
    }

    //########################################################################
    // Debug interface - utils

    public static final Class OBJECT_CLASS = Object.class;

    public static boolean hasToString(Class clasz) {
        try {
            Method toString = clasz.getMethod("toString", new Class[0]);
            return toString.getDeclaringClass() != OBJECT_CLASS;
        } catch (NoSuchMethodException excpetion) {
            return false;
        } catch (SecurityException excpetion) {
            return false;
        }
    }

    public static String getClassName(Object object) {
        if (object == null) return "null";
        Class clasz = object.getClass();
        String name = clasz.getName();
        if (!name.endsWith("$$Var")) return name;
        Class superclass = clasz.getSuperclass();
        Field[] fields = superclass.getDeclaredFields();
        for (int i = 0; i < fields.length; i++) {
            try {
                Field field = fields[i];
                if (field.getType() != clasz) continue;
                if (!Modifier.isStatic(field.getModifiers())) continue;
                Object value = field.get(null);
                if (value != object) continue;
                return name + "[" + field.getName() + "]";
            } catch (IllegalAccessException exception) {
                // continue
            }
        }
        return name;
    }

    //########################################################################
}

public interface DebugHandler {

    //########################################################################
    // DebugHandler interface

    public void append0(StringBuffer buffer, Object that);

    //########################################################################
}

public abstract class DebugAbstractHandler implements DebugHandler {
}

public class DebugDefaultHandler extends DebugAbstractHandler {

    //########################################################################
    // DebugDefaultHandler interface

    public static DebugDefaultHandler instance = new DebugDefaultHandler();

    //########################################################################
    // DebugHandler interface

    public void append0(StringBuffer buffer, Object that) {
        Debug.appendDefault(buffer, that);
    }

    //########################################################################
}

public class DebugToStringHandler extends DebugAbstractHandler {

    //########################################################################
    // DebugToStringHandler interface

    public static DebugToStringHandler instance = new DebugToStringHandler();

    //########################################################################
    // DebugHandler interface

    public void append0(StringBuffer buffer, Object that) {
        buffer.append(that);
    }

    //########################################################################
}

public class DebugObject extends DebugAbstractHandler {

    //########################################################################
    // DebugObject interface

    public static DebugObject instance = new DebugObject();

    public void append1(StringBuffer buffer, Object that) {
        Debug.appendObject(buffer, that);
    }

    //########################################################################
    // DebugHandler interface

    public void append0(StringBuffer buffer, Object that) {
        append1(buffer, (Object)that);
    }

    //########################################################################
}

public class DebugArray extends DebugAbstractHandler {

    //########################################################################
    // DebugArray interface

    public static DebugArray instance = new DebugArray();

    public void append1(StringBuffer buffer, Object[] that) {
        Debug.appendArray(buffer, that);
    }

    //########################################################################
    // DebugHandler interface

    public void append0(StringBuffer buffer, Object that) {
        append1(buffer, (Object[])that);
    }

    //########################################################################
}

public class DebugSymbol extends DebugAbstractHandler {

    //########################################################################
    // DebugSymbol interface

    public static DebugSymbol instance = new DebugSymbol();

    public void append1(StringBuffer buffer, Symbol that) {
        if (!that.isNone() && !that.owner().isRoot() && !that.isRoot()) {
            Debug.append(buffer, that.owner());
            buffer.append(".");
        }
        buffer.append(that.name);
        if (Global.instance.uniqid) {
            buffer.append('#');
            buffer.append(Global.instance.uniqueID.id(that));
        }
    }

    //########################################################################
    // DebugHandler interface

    public void append0(StringBuffer buffer, Object that) {
        append1(buffer, (Symbol)that);
    }

    //########################################################################
}

public class DebugScope extends DebugAbstractHandler {

    //########################################################################
    // DebugScope interface

    public static DebugScope instance = new DebugScope();

    public void append1(StringBuffer buffer, Scope that) {
        buffer.append('{');
        for (Scope.SymbolIterator i = that.iterator(); i.hasNext();) {
            Debug.append(buffer, i.next());
            if (i.hasNext()) buffer.append(',');
        }
        buffer.append('}');
    }

    //########################################################################
    // DebugHandler interface

    public void append0(StringBuffer buffer, Object that) {
        append1(buffer, (Scope)that);
    }

    //########################################################################
}

