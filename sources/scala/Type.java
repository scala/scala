/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala;

import scala.runtime.RunTime;

/**
 * Run-time types for Scala.
 *
 * @author Michel Schinz
 * @version 1.0
 */

abstract public class Type {
    public static final Type[] EMPTY_ARRAY = new Type[]{};

    /** @meta method [?T](scala.Int) scala.Array[?T]; */
    abstract public Array newArray(int size);

    /** Return the default value for the type (_ in Scala) */
    abstract public Object defaultValue();

    /**
     * Return true iff the given object is an instance of a subtype of
     * this type (implement Scala's isInstanceOf operation).
     */
    abstract public boolean hasAsInstance(Object o);

    /**
     * Check that the given object can be cast to this type, and throw
     * an exception if this is not possible (implement Scala's
     * asInstanceOf operation).
     */
    public Object checkCastability(Object o) {
        if (! (o == null || hasAsInstance(o)))
            throw new ClassCastException(); // TODO provide a message
        return o;
    }

    // Basic types
    public static final TypeDouble  Double  = new TypeDouble();
    public static final TypeFloat   Float   = new TypeFloat();
    public static final TypeLong    Long    = new TypeLong();
    public static final TypeInt     Int     = new TypeInt();
    public static final TypeShort   Short   = new TypeShort();
    public static final TypeChar    Char    = new TypeChar();
    public static final TypeByte    Byte    = new TypeByte();
    public static final TypeBoolean Boolean = new TypeBoolean();
}

public class ConstructedType extends Type {
    private Object outer;
    private Class typeConstr;
    private Type[] args;

    public ConstructedType(Object outer, String typeConstrName) {
        this(outer, typeConstrName, Type.EMPTY_ARRAY);
    }

    public ConstructedType(Object outer, String typeConstrName, Type arg1) {
        this(outer, typeConstrName, new Type[]{ arg1 });
    }

    public ConstructedType(Object outer,
                           String typeConstrName,
                           Type arg1,
                           Type arg2) {
        this(outer, typeConstrName, new Type[]{ arg1, arg2 });
    }

    public ConstructedType(Object outer,
                           String typeConstrName,
                           Type arg1,
                           Type arg2,
                           Type arg3) {
        this(outer, typeConstrName, new Type[]{ arg1, arg2, arg3 });
    }

    public ConstructedType(Object outer,
                           String typeConstrName,
                           Type arg1,
                           Type arg2,
                           Type arg3,
                           Type arg4) {
        this(outer, typeConstrName, new Type[]{ arg1, arg2, arg3, arg4 });
    }

    public ConstructedType(Object outer, String typeConstrName, Type[] args) {
        try {
            this.outer = outer;
            this.typeConstr = Class.forName(typeConstrName,
                                            false,
                                            getClass().getClassLoader());
            this.args = args;
        } catch (ClassNotFoundException e) {
            throw new Error(e);
        }
    }

    public Array newArray(int size) {
        // TODO is that correct if we have type arguments?
        Object[] array =
            (Object[])java.lang.reflect.Array.newInstance(typeConstr, size);
        return RunTime.box_oarray(array);
    }

    public Object defaultValue() {
        return null;
    }

    public boolean hasAsInstance(Object o) {
        return typeConstr.isInstance(o); // TODO complete
    }
}

public class SingleType extends Type {
    private final Object instance;

    public SingleType(Object instance) {
        this.instance = instance;
    }

    public Array newArray(int size) {
        throw new Error();      // TODO
    }

    public Object defaultValue() {
        throw new Error();      // TODO
    }

    public boolean hasAsInstance(Object o) {
        return (o == instance);
    }
}

// The following classes may not be defined in class Type because
// inner classes confuse pico which then attributes the metadata to
// the wrong members.

class TypeDouble extends Type {
    private final Double ZERO = RunTime.box_dvalue(0.0);
    public Array newArray(int size) {
        return RunTime.box_darray(new double[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean hasAsInstance(Object o) {
        throw new UnsupportedOperationException();
    }
};

class TypeFloat extends Type {
    private final Float ZERO = RunTime.box_fvalue(0.0f);
    public Array newArray(int size) {
        return RunTime.box_farray(new float[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean hasAsInstance(Object o) {
        throw new UnsupportedOperationException();
    }
};

class TypeLong extends Type {
    private final Long ZERO = RunTime.box_lvalue(0l);
    public Array newArray(int size) {
        return RunTime.box_larray(new long[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean hasAsInstance(Object o) {
        throw new UnsupportedOperationException();
    }
};

class TypeInt extends Type {
    private final Int ZERO = RunTime.box_ivalue(0);
    public Array newArray(int size) {
        return RunTime.box_iarray(new int[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean hasAsInstance(Object o) {
        throw new UnsupportedOperationException();
    }
};

class TypeShort extends Type {
    private final Short ZERO = RunTime.box_svalue((short)0);
    public Array newArray(int size) {
        return RunTime.box_sarray(new short[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean hasAsInstance(Object o) {
        throw new UnsupportedOperationException();
    }
};

class TypeChar extends Type {
    private final Char ZERO = RunTime.box_cvalue((char)0);
    public Array newArray(int size) {
        return RunTime.box_carray(new char[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean hasAsInstance(Object o) {
        throw new UnsupportedOperationException();
    }
};

class TypeByte extends Type {
    private final Byte ZERO = RunTime.box_bvalue((byte)0);
    public Array newArray(int size) {
        return RunTime.box_barray(new byte[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean hasAsInstance(Object o) {
        throw new UnsupportedOperationException();
    }
};

class TypeBoolean extends Type {
    private final Boolean ZERO = RunTime.box_zvalue(false);
    public Array newArray(int size) {
        return RunTime.box_zarray(new boolean[size]);
    }
    public Object defaultValue() { return ZERO; }
    public boolean hasAsInstance(Object o) {
        throw new UnsupportedOperationException();
    }
};
