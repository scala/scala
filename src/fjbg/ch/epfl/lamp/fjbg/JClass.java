/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2012 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.util.*;
import java.io.*;

/**
 * Representation of a Java class.
 *
 * @author Michel Schinz, Stephane Micheloud
 * @version 1.1
 */
public class JClass extends JMember {

    /** Magic number for Java class files. */
    public final static int MAGIC_NUMBER = 0xCAFEBABE;

    protected final JAttributeFactory attributeFactory;

    protected final String superclassName;
    protected final String[] interfaceNames;
    protected final String sourceFileName;
    protected final JConstantPool pool;

    public final static String[] NO_INTERFACES = new String[0];

    protected final LinkedList/*<JMethod>*/ methods = new LinkedList();
    protected final LinkedList/*<JField>*/ fields = new LinkedList();

    protected JInnerClassesAttribute innerClasses;

    protected int major;
    protected int minor;

    /**
     * Creates a new class with its access flags, name, superclass name,
     * interfaces names and source file name initialized to a given value.
     * The constructor also initializes the pool and adds a sourceFileName
     * attribute to the class.
     * @param accessFlags the int representing the access flags of the class.
     * @param name the string representing the name of the class.
     * @param superclassName the string representing the name of the class'
     * superclass.
     * @param interfaceNames the list of strings representing the names of the
     * interfaces implemented by the class.
     * @param sourceFileName name of the file from which the class was compiled.
     */
    protected JClass(FJBGContext context,
                     int accessFlags,
                     String name,
                     String superclassName,
                     String[] interfaceNames,
                     String sourceFileName) {
        super(context, accessFlags, name);
        this.attributeFactory = context.getJAttributeFactory();

        this.major = context.MAJOR_VERSION;
        this.minor = context.MINOR_VERSION;

        this.superclassName = superclassName;
        this.interfaceNames = interfaceNames;
        this.sourceFileName = sourceFileName;
        this.pool = context.JConstantPool();
        if (sourceFileName != null)
            addAttribute(context.JSourceFileAttribute(this, sourceFileName));
    }

    protected JClass(FJBGContext context, DataInputStream stream)
        throws IOException {
        super(context);
        this.attributeFactory = context.getJAttributeFactory();

        int magic = stream.readInt();
        if (magic != MAGIC_NUMBER)
            throw new IllegalArgumentException("invalid magic number: "+magic);

        minor = stream.readShort();
        major = stream.readShort();
        pool = context.JConstantPool(stream);
        accessFlags = stream.readShort();

        // This class, super class and interfaces
        name = pool.lookupClass(stream.readShort());
        superclassName = pool.lookupClass(stream.readShort());
        interfaceNames = new String[stream.readShort()];
        for (int i = 0; i < interfaceNames.length; ++i)
            interfaceNames[i] = pool.lookupClass(stream.readShort());

        // Fields, methods and attributes
        int fieldsCount = stream.readShort();
        for (int i = 0; i < fieldsCount; ++i)
            addField(context.JField(this, stream));

        int methodsCount = stream.readShort();
        for (int i = 0; i < methodsCount; ++i)
            addMethod(context.JMethod(this, stream));

        String fileName = null;
        int attributesCount = stream.readShort();
        for (int i = 0; i < attributesCount; ++i) {
            JAttribute attr = attributeFactory.newInstance(this, this, stream);
            if (attr instanceof JSourceFileAttribute)
                fileName = ((JSourceFileAttribute)attr).getFileName();
            else if (attr instanceof JInnerClassesAttribute)
                innerClasses = (JInnerClassesAttribute)attr;
            addAttribute(attr);
        }
        sourceFileName = fileName;
    }

    /**
     * Gets the name of the class' superclass.
     * @return The string representing the name of the class' superclass.
     */
    public String getSuperclassName() { return superclassName; }

    /**
     * Gets the names of the interfaces implemented by the class.
     * @return The array containing the string representations of the
     * names of the interfaces implemented by the class.
     */
    public String[] getInterfaceNames() { return interfaceNames; }

    /**
     * Gets the source file name of this class.
     * @return The string representing the source file name of this class.
     */
    public String getSourceFileName() { return sourceFileName; }

    /**
     * Gets the type of the objects that are instances of the class.
     * @return The type of the instances of the class.
     */
    public JType getType() { return new JObjectType(name); }

    public JClass getJClass() { return this; }

    public boolean isPublic() {
        return (accessFlags & JAccessFlags.ACC_PUBLIC) != 0;
    }

    public boolean isPrivate() {
        return (accessFlags & JAccessFlags.ACC_PRIVATE) != 0;
    }

    public boolean isProtected() {
        return (accessFlags & JAccessFlags.ACC_PROTECTED) != 0;
    }

    public boolean isStatic() {
        return (accessFlags & JAccessFlags.ACC_STATIC) != 0;
    }

    public boolean isFinal() {
        return (accessFlags & JAccessFlags.ACC_FINAL) != 0;
    }

    public boolean isAbstract() {
        return (accessFlags & JAccessFlags.ACC_ABSTRACT) != 0;
    }

    /**
     * Gets the version number of the class.
     * @param major The int representing the major part of the version number
     * of the class.
     * @param minor The int representing the minor part of the version number
     * of the class.
     */
    public void setVersion(int major, int minor) {
        assert !frozen;
        this.major = major;
        this.minor = minor;
    }

    /**
     * Gets the major part of the number describing the version of the class.
     * @return The int representing the major part of the version number of
     * the class.
     */
    public int getMajorVersion() { return major; }

    /**
     * Gets the minor part of the number describing the version of the class.
     * @return The int representing the minor part of the version number of
     * the class.
     */
    public int getMinorVersion() { return minor; }

    /**
     * Gets the constant pool of the class.
     * @return The constant pool of the class.
     */
    public JConstantPool getConstantPool() { return pool; }

    public JInnerClassesAttribute getInnerClasses() {
    	if (innerClasses == null) {
    		innerClasses = new JInnerClassesAttribute(context, this);
    		addAttribute(innerClasses);
    	}
    	return innerClasses;
    }

    /**
     * Decides if the class is an interface.
     * @return The boolean representing if the class is an interface or not.
     */
    public boolean isInterface() {
        return (accessFlags & JAccessFlags.ACC_INTERFACE) != 0;
    }

    public void addField(JField field) {
        assert !frozen;
        fields.add(field);
    }

    /**
     * Create and add a new field to the class.
     */
    public JField addNewField(int accessFlags, String name, JType type) {
        assert !frozen;
        JField f = context.JField(this, accessFlags, name, type);
        addField(f);
        return f;
    }

    protected void addMethod(JMethod method) {
        assert !frozen;
        methods.add(method);
    }

    /**
     * Create and add a new method to the class.
     */
    public JMethod addNewMethod(int accessFlags,
                                String name,
                                JType returnType,
                                JType[] argTypes,
                                String[] argNames) {
        assert !frozen;
        JMethod m = context.JMethod(this,
                                    accessFlags,
                                    name,
                                    returnType,
                                    argTypes,
                                    argNames);
        addMethod(m);
        return m;
    }

    /**
     * Remove a previously-added method. This makes no attempt at
     * minimising the constant pool by removing all constants which
     * were used only by this method.
     */
    public void removeMethod(JMethod m) {
        assert !frozen;
        methods.remove(m);
    }

    public JField[] getFields() {
        return (JField[])fields.toArray(new JField[fields.size()]);
    }

    public JMethod[] getMethods() {
        return (JMethod[])methods.toArray(new JMethod[methods.size()]);
    }

    /**
     * Freeze the contents of this class so that it can be written to
     * a file.
     */
    public void freeze() {
        assert !frozen;
        frozen = true;
    }

    /**
     * Writes the contents of the class to a file referenced by its name.
     * @param fileName The name of the file in which the class must be written.
     */
    public void writeTo(String fileName) throws IOException {
        writeTo(new File(fileName));
    }

    /**
     * Writes the contents of the class to a file.
     * @param file The file in which the class must be written.
     */
    public void writeTo(File file) throws IOException {
        File parent = file.getParentFile();
        if (parent != null && !parent.isDirectory())
            if (!parent.mkdirs())
                throw new IOException("cannot create directory " + parent);

        FileOutputStream fStream = new FileOutputStream(file);
        BufferedOutputStream bStream = new BufferedOutputStream(fStream);
        DataOutputStream dStream = new DataOutputStream(bStream);
        writeTo(dStream);
        dStream.close();
        bStream.close();
        fStream.close();
    }

    /**
     * Writes the contents of the class to a data stream.
     * @param stream The data stream in which the class must be written.
     */
    public void writeTo(DataOutputStream stream) throws IOException {
        if (!frozen) freeze();

        int thisClassIdx = pool.addClass(name);
        int superClassIdx = pool.addClass(superclassName);
        int[] interfacesIdx = new int[interfaceNames.length];

        for (int i = 0; i < interfaceNames.length; ++i)
            interfacesIdx[i] = pool.addClass(interfaceNames[i]);

        pool.freeze();

        // Magic number.
        stream.writeInt(MAGIC_NUMBER);
        // Version
        stream.writeShort(minor);
        stream.writeShort(major);
        // Constant pool
        pool.writeTo(stream);
        // Access flags
        stream.writeShort(accessFlags);

        // This class, super class and interfaces
        stream.writeShort(thisClassIdx);
        stream.writeShort(superClassIdx);
        stream.writeShort(interfacesIdx.length);
        for (int i = 0; i < interfacesIdx.length; ++i)
            stream.writeShort(interfacesIdx[i]);

        // Fields and methods
        stream.writeShort(fields.size());
        Iterator fieldsIt = fields.iterator();
        while (fieldsIt.hasNext())
            ((JField)fieldsIt.next()).writeTo(stream);

        stream.writeShort(methods.size());
        Iterator methodsIt = methods.iterator();
        while (methodsIt.hasNext())
            ((JMethod)methodsIt.next()).writeTo(stream);

        // Attributes
        JAttribute.writeTo(attributes, stream);
    }

    // Follows javap output format for ClassFile.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer();
        if (sourceFileName != null) {
            buf.append("Compiled from \"");
            buf.append(sourceFileName);
            buf.append("\"\n");
        }
        buf.append(getMemberName());
        buf.append(toExternalName(getName()));
        if (!isInterface()) {
            buf.append(" extends ");
            buf.append(toExternalName(getSuperclassName()));
        }
        if (interfaceNames.length > 0) {
            if (isInterface()) buf.append(" extends ");
            else buf.append(" implements ");
            for (int i = 0; i < interfaceNames.length; ++i) {
                if (i > 0) buf.append(",");
                buf.append(toExternalName(interfaceNames[i]));
            }
        }
        buf.append("\n");
        Iterator attrsIt = attributes.iterator();
        while (attrsIt.hasNext()) {
            JAttribute attr = (JAttribute)attrsIt.next();
            buf.append(attr);
        }
        buf.append("  minor version: ");
        buf.append(minor);
        buf.append("\n  major version: ");
        buf.append(major);
        buf.append("\n");
        buf.append(pool);
        buf.append("\n{\n");
        JField[] jfields = getFields();
        for (int i = 0; i < jfields.length; ++i) {
            if (i > 0) buf.append("\n");
            buf.append(jfields[i]);
        }
        buf.append("\n");
        JMethod[] jmethods = getMethods();
        for (int i = 0; i < jmethods.length; ++i) {
            if (i > 0) buf.append("\n");
            buf.append(jmethods[i]);
        }
        buf.append("\n}\n");
        return buf.toString();
    }

    private String getMemberName() {
        StringBuffer buf = new StringBuffer();
        if (isPublic()) buf.append("public ");
        else if (isProtected()) buf.append("protected ");
        else if (isPrivate()) buf.append("private ");
        if (isInterface())
            buf.append("interface ");
        else {
            if (isAbstract()) buf.append("abstract ");
            else if (isFinal()) buf.append("final ");
            buf.append("class ");
        }
        return buf.toString();
    }
}
