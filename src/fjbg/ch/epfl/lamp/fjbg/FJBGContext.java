/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2012 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.IOException;

/**
 * Context in which FJBG executes. Used both as a factory for most
 * FJBG classes and as a repository for other factories.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class FJBGContext {
    /** Class file major version */
    final int MAJOR_VERSION;

    /** Class file minor version */
    final int MINOR_VERSION;

    public FJBGContext() {
        this(45, 3);
    }

    public FJBGContext(int major, int minor) {
        MAJOR_VERSION = major;
        MINOR_VERSION = minor;
    }

    // Factory methods
    //////////////////////////////////////////////////////////////////////

    public JClass JClass(int accessFlags,
                         String name,
                         String superclassName,
                         String[] interfaceNames,
                         String sourceFileName) {
        return new JClass(this,
                          accessFlags,
                          name,
                          superclassName,
                          interfaceNames,
                          sourceFileName);
    }

    public JClass JClass(DataInputStream stream)
        throws IOException {
        return new JClass(this, stream);
    }

    public JConstantPool JConstantPool() {
        return new JConstantPool(this);
    }

    public JConstantPool JConstantPool(DataInputStream stream)
        throws IOException {
        return new JConstantPool(this, stream);
    }

    public JField JField(JClass owner,
			 int accessFlags,
			 String name,
			 JType type) {
        return new JField(this,
                          owner,
			  accessFlags,
			  name,
			  type);
    }

    public JField JField(JClass owner, DataInputStream stream)
        throws IOException {
        return new JField(this, owner, stream);
    }

    public JMethod JMethod(JClass owner,
                           int accessFlags,
                           String name,
                           JType returnType,
                           JType[] argTypes,
                           String[] argNames) {
        return new JMethod(this,
                           owner,
                           accessFlags,
                           name,
                           returnType,
                           argTypes,
                           argNames);
    }

    public JMethod JMethod(JClass owner,
                           int accessFlags,
                           String name,
                           JMethodType type,
                           String[] argNames) {
        return JMethod(owner,
                       accessFlags,
                       name,
                       type.getReturnType(),
                       type.getArgumentTypes(),
                       argNames);
    }

    public JMethod JMethod(JClass owner, DataInputStream stream)
        throws IOException {
        return new JMethod(this, owner, stream);
    }

    public JLocalVariable JLocalVariable(JMethod owner,
                                         JType type,
                                         String name,
                                         int index) {
        return new JLocalVariable(this, owner, type, name, index);
    }

    public JCode JCode(JClass clazz, JMethod owner) {
        return new JExtendedCode(this, clazz, owner);
    }

    public JCode JCode(JClass clazz, JMethod owner, DataInputStream stream)
        throws IOException {
        return new JCode(this, clazz, owner, stream);
    }

    public JAttributeFactory JAttributeFactory() {
        return new JAttributeFactory(this);
    }

    // Attributes
    public JCodeAttribute JCodeAttribute(JClass clazz, JMethod owner) {
        return new JCodeAttribute(this, clazz, owner);
    }

    public JEnclosingMethodAttribute JEnclosingMethodAttribute(JClass clazz,
                                                               String className,
                                                               String methodName,
                                                               JType methodType) {
        return new JEnclosingMethodAttribute(this, clazz, className, methodName, methodType);
    }

    public JExceptionsAttribute JExceptionsAttribute(JClass clazz,
                                                     JMethod owner) {
        return new JExceptionsAttribute(this, clazz, owner);
    }

    public JLineNumberTableAttribute JLineNumberTableAttribute(JClass clazz,
                                                               JCode owner) {
        return new JLineNumberTableAttribute(this, clazz, owner);
    }

    public JLocalVariableTableAttribute JLocalVariableTableAttribute(JClass clazz,
                                                                     JCode owner) {
        return new JLocalVariableTableAttribute(this, clazz, owner);
    }

    public JOtherAttribute JOtherAttribute(JClass clazz,
                                           Object owner,
                                           String name,
                                           byte[] contents,
                                           int length) {
        return new JOtherAttribute(this, clazz, owner, name, contents, length);
    }

    public JOtherAttribute JOtherAttribute(JClass clazz,
                                           Object owner,
                                           String name,
                                           byte[] contents) {
        return JOtherAttribute(clazz, owner, name, contents, contents.length);
    }

    public JSourceFileAttribute JSourceFileAttribute(JClass clazz,
                                                     String sourceFileName) {
        return new JSourceFileAttribute(this, clazz, sourceFileName);
    }

    public JStackMapTableAttribute JStackMapTableAttribute(JClass clazz,
                                                           JCode owner) {
        return new JStackMapTableAttribute(this, clazz, owner);
    }

    /// Repository
    //////////////////////////////////////////////////////////////////////

    protected JAttributeFactory jAttributeFactory = null;
    public JAttributeFactory getJAttributeFactory() {
        if (jAttributeFactory == null)
            jAttributeFactory = JAttributeFactory();
        return jAttributeFactory;
    }
}
