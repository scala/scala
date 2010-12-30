/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;

/**
 * Extensible factory to build subclasses of JAttribute based on an
 * attribute name.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JAttributeFactory {
    protected FJBGContext context;
    protected HashMap/*<String, Constructor>*/ constructors = new HashMap();

    protected final static Class[] CONSTRUCTOR_ARGS = new Class[] {
        FJBGContext.class,
        JClass.class,
        Object.class,
        String.class,
        int.class,
        DataInputStream.class
    };

    protected final static Constructor defaultDefaultConstructor;
    static {
        try {
            defaultDefaultConstructor =
                 JOtherAttribute.class.getConstructor(CONSTRUCTOR_ARGS);
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    protected final Constructor defaultConstructor;

    public JAttributeFactory(FJBGContext context,
                             Constructor defaultConstructor) {
        this.context = context;
        this.defaultConstructor = defaultConstructor;
        registerClass("BootstrapInvokeDynamic", JBootstrapInvokeDynamic.class);
        registerClass("Code", JCodeAttribute.class);
        registerClass("ConstantValue", JConstantValueAttribute.class);
        registerClass("EnclosingMethod", JEnclosingMethodAttribute.class);
        registerClass("Exceptions", JExceptionsAttribute.class);
        registerClass("InnerClasses", JInnerClassesAttribute.class);
        registerClass("LineNumberTable", JLineNumberTableAttribute.class);
        registerClass("LocalVariableTable", JLocalVariableTableAttribute.class);
        registerClass("SourceFile", JSourceFileAttribute.class);
        registerClass("StackMapTable", JStackMapTableAttribute.class);
    }

    public JAttributeFactory(FJBGContext context) {
        this(context, defaultDefaultConstructor);
    }

    public void registerClass(String attributeName,
                              Class clazz) {
        if (! JAttribute.class.isAssignableFrom(clazz))
            throw new IllegalArgumentException("Not a subclass of JAttribute: "
                                               + clazz);

        try {
            Constructor constr = clazz.getConstructor(CONSTRUCTOR_ARGS);
            constructors.put(attributeName, constr);
        } catch (NoSuchMethodException e) {
            throw new IllegalArgumentException("No appropriate constructor for "
                                               + clazz);
        }
    }

    public JAttribute newInstance(JClass clazz,
                                  Object owner,
                                  DataInputStream stream)
        throws IOException {
        String name = clazz.getConstantPool().lookupUtf8(stream.readShort());
        Integer size = new Integer(stream.readInt());
        Constructor constr = (Constructor)constructors.get(name);
        if (constr == null) constr = defaultConstructor;

        Object[] args = new Object[] { context, clazz, owner, name, size, stream };
        try {
            return (JAttribute)constr.newInstance(args);
        } catch (InstantiationException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }
}
