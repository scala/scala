/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * Abstract superclass for a Java class, field or method.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */

abstract public class JMember {

    protected boolean frozen = false;

    protected final FJBGContext context;

    protected String name;

    protected int accessFlags;

    protected final List/*<JAttribute>*/ attributes = new LinkedList();

    protected JMember(FJBGContext context) { this.context = context; }

    protected JMember(FJBGContext context, int accessFlags, String name) {
        this(context);
        this.name = name;
        this.accessFlags = accessFlags;
    }

    /**
     * Gets the access flags of the class.
     * @return The int representing the access flags of the class.
     */
    public int getAccessFlags() { return accessFlags; }

    /**
     * Gets the name of the member.
     * @return The string representing the name of the member.
     */
    public String getName() { return name; }

    /**
     * Gets the type of the objects that are instances of the class.
     * @return The type of the instances of the class.
     */
    public abstract JType getType();

    /**
     * Gets the class corresponding to/owning this member
     * @return The class owning this member or the class itself.
     */
    public abstract JClass getJClass();

    /**
     * Gets the constant pool of the class.
     * @return The constant pool of the class.
     */
    public JConstantPool getConstantPool() { return getJClass().getConstantPool(); }

    public FJBGContext getContext() { return context; }

    /**
     * Adds an attribute to the class.
     * @param attr The attribute to be added.
     */
    public void addAttribute(JAttribute attr) {
        assert !frozen;
        attributes.add(attr);
    }

    /**
     * Gets the list of all attributes of the class.
     * @return The list of the attributes of the class representation.
     */
    public List/*<JAttribute>*/ getAttributes() {
        return attributes;
    }

    /**
     * Get the attribute with the given name, or null if it doesn't
     * exist.
     */
    public JAttribute getAttribute(String name) {
        Iterator attrIt = getAttributes().iterator();
        while (attrIt.hasNext()) {
            JAttribute attr = (JAttribute)attrIt.next();
            if (attr.getName().equals(name))
                return attr;
        }
        return null;
    }

    protected static String toExternalName(String name) {
        return name.replace('/', '.');
    }

    protected static String toExternalName(JType tpe) {
        return tpe.toString().replace(':', '.');
    }
}
