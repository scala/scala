/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util.debug;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;

/**
 * This class is intended to help debugging applications. It provides
 * functions to abort the application and to turn objects into
 * strings.
 *
 * In order to turn objects into strings, the class maintains a list
 * of debuggers. An application using this class should configure it
 * by adding debuggers for its data types (see method "addDebugger").
 *
 * Each time an object has to be transformed into a string, the list
 * of debuggers is searched from the last added one to the first one
 * for a debugger that is able to make that transformation. The first
 * that is found, is then used to make the transformation.
 */
public class Debug {

    //########################################################################
    // Private Variables

    /** The list of all available debuggers */
    private static final ArrayList/*<Debugger>*/ debuggers = new ArrayList();

    static {
        addDebugger(ObjectDebugger.object);
        addDebugger(ArrayDebugger.object);
        addDebugger(ThrowableDebugger.object);
        addDebugger(new ToStringDebugger(String.class));
    }

    //########################################################################
    // Public Methods - Configuring

    /** Adds the specified debugger to the list of debuggers. */
    public static void addDebugger(Debugger debugger) {
        debuggers.add(debugger);
    }

    //########################################################################
    // Public Methods - Aborting

    /** Aborts the application by throwing an AbortError. */
    public static Error abort() {
        throw new AbortError();
    }
    public static Error abort(Throwable cause) {
        throw new AbortError(cause);
    }
    public static Error abort(Object object) {
        return abort(show(object));
    }
    public static Error abort(Object object, Throwable cause) {
        return abort(show(object), cause);
    }
    public static Error abort(String message) {
        throw new AbortError(message);
    }
    public static Error abort(String message, Throwable cause) {
        throw new AbortError(message, cause);
    }
    public static Error abort(String message, Object object) {
        return abort(message + ": " + show(object));
    }
    public static Error abort(String message, Object object, Throwable cause) {
        return abort(message + ": " + show(object), cause);
    }

    /** Aborts the application by throwing an AbortError. */
    public static Error abortIllegalCase(int value) {
        return abort("illegal case: " + value);
    }
    public static Error abortIllegalCase(Object object) {
        return abort("illegal case", object);
    }

    //########################################################################
    // Public Methods - Showing

    /**
     * Makes a string out of the object(s) using for each the first
     * matching debugger and separating them with dash(es).
     */
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

    /**
     * Makes a string out of all objects using for each the first
     * matching debugger and separating them with dashes.
     */
    public static String showAll(Object[] objects) {
        return showAll(objects, " - ");
    }

    /**
     * Makes a string out of all objects using for each the first
     * matching debugger and separating them with the specified
     * separator if it is non-null and by nothing otherwise.
     */
    public static String showAll(Object[] objects, String separator) {
        StringBuffer buffer = new StringBuffer();
        appendAll(buffer, objects, separator);
        return buffer.toString();
    }

    //########################################################################
    // Public Methods - Appending

    /**
     * Appends the object to the buffer using the first matching
     * debugger.
     */
    public static void append(StringBuffer buffer, Object object) {
        if (object != null) {
            for (int i = debuggers.size() - 1; i >= 0; i--) {
                Debugger debugger = (Debugger)debuggers.get(i);
                if (!debugger.canAppend(object)) continue;
                debugger.append(buffer, object);
                return;
            }
        }
        buffer.append(object);
    }

    /**
     * Appends all the objects to the buffer using for each the first
     * matching debugger. The object are separated by the specified
     * separator if it is non-null and by nothing otherwise.
     */
    public static void appendAll(StringBuffer buffer, Object[] objects,
        String separator)
    {
        for (int i = 0; i < objects.length; i++) {
            if (i > 0 && separator != null) buffer.append(separator);
            append(buffer, objects[i]);
        }
    }

    //########################################################################
    // Public Methods - Miscellaneous

    /**
     * Returns the class name of the object. Does some pretty printing
     * for parameterless pico case classes.
     */
    public static String getClassNameOf(Object object) {
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

    /** Returns true iff the object overrides "Object.toString()". */
    public static boolean overridesToString(Object object) {
        try {
            Class clasz = object.getClass();
            Method toString = clasz.getMethod("toString", new Class[0]);
            return toString.getDeclaringClass() != Object.class;
        } catch (NoSuchMethodException exception) {
            return false;
        } catch (SecurityException exception) {
            return false;
        }
    }

    //########################################################################
}
