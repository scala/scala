/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2012 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * Representation of a Java method.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JMethod extends JFieldOrMethod {
    public final static String CLASS_CONSTRUCTOR_NAME = "<clinit>";
    public final static String INSTANCE_CONSTRUCTOR_NAME = "<init>";

    protected /*final*/ JCode code;
    protected final String[] argNames;

    protected final LinkedList/*<JLocalVariable>*/ localVariables =
        new LinkedList();
    protected int localVariableIndex = 0;


    protected JMethod(FJBGContext context,
                      JClass owner,
                      int accessFlags,
                      String name,
                      JType returnType,
                      JType[] argTypes,
                      String[] argNames) {
        super(context,
              owner,
              accessFlags,
              name,
              new JMethodType(returnType, argTypes));
        this.argNames = argNames;

        assert argTypes.length == argNames.length;

        if (isAbstract() || isNative()) {
            code = null;
        } else {
            code = context.JCode(owner, this);
            addAttribute(context.JCodeAttribute(owner, this));

            if (!isStatic())
                addNewLocalVariable(owner.getType(), "this");

            for (int i = 0; i < argTypes.length; ++i)
                addNewLocalVariable(argTypes[i], argNames[i]);
        }
    }

    protected JMethod(FJBGContext context,
                      JClass owner,
                      DataInputStream stream)
        throws IOException {
        super(context, owner, stream);

        assert isAbstract() || isNative() || code != null;

        int n = 0;
        if (code != null) {
            for (Iterator it = code.getAttributes().iterator(); it.hasNext(); ) {
                JAttribute attr = (JAttribute)it.next();
                if (attr instanceof JLocalVariableTableAttribute)
                   n = ((JLocalVariableTableAttribute)attr).getMaxLocals();
            }
        }
        this.localVariableIndex = n;


        JType[] argTypes = ((JMethodType)getType()).getArgumentTypes();
        argNames = new String[argTypes.length]; // TODO get from attribute
        for (int i = 0; i < argNames.length; ++i)
            argNames[i] = "v"+i;
    }

    public void freeze() throws JCode.OffsetTooBigException {
        if (code != null) code.freeze();
        super.freeze();
    }

    public JType getReturnType() {
        return ((JMethodType)type).getReturnType();
    }

    public JType[] getArgumentTypes() {
        return ((JMethodType)type).getArgumentTypes();
    }

    public int getArgsSize() {
        int size = ((JMethodType)type).getArgsSize();
        if (!isStatic()) size += 1;  // for this
        return size;
    }

    public String[] getArgumentNames() {
        return argNames;
    }

    public JCode getCode() {
        assert !isAbstract();
        return code;
    }

    // Invoked by the JCode constructor
    protected void setCode(JCode code) {
        assert null == this.code;
        this.code = code;
    }

    public JCodeIterator codeIterator() {
        return new JCodeIterator(code);
    }

    // Local variables
    // FIXME : find a better management method for local variables
    public JLocalVariable addNewLocalVariable(JType type, String name) {
        assert !frozen;
        JLocalVariable var =
            context.JLocalVariable(this, type, name, localVariableIndex);
        localVariableIndex += type.getSize();
        localVariables.add(var);
        return var;
    }

    public JLocalVariable getLocalVariable(int index) {
        for (int i = 0; i < localVariables.size(); i++) {
            if (((JLocalVariable)localVariables.get(i)).index == index)
                return (JLocalVariable)localVariables.get(i);
        }
        return null;
    }

    public JLocalVariable[] getLocalVariables() {
        return (JLocalVariable[])localVariables
            .toArray(new JLocalVariable[localVariables.size()]);
    }


    public int getMaxLocals() {
        return localVariableIndex;
    }

    // Follows javap output format for methods.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer(flagsToString());
        String name = getName();
        if (CLASS_CONSTRUCTOR_NAME.equals(name))
            buf.append("{}");
        else {
            if (INSTANCE_CONSTRUCTOR_NAME.equals(name))
                name = getOwner().getName();
            else {
                buf.append(toExternalName(getReturnType()));
                buf.append(" ");
            }
            buf.append(toExternalName(name));
            buf.append("(");
            JType[] ts = getArgumentTypes();
            for (int i = 0; i < ts.length; ++i) {
                if (i > 0) buf.append(", ");
                buf.append(toExternalName(ts[i]));
            }
            buf.append(")");
        }
        buf.append(";\n");
        Iterator it = attributes.iterator();
        while(it.hasNext()) {
            JAttribute attr = (JAttribute)it.next();
            buf.append(attr);
        }
        return buf.toString();
    }

    private String flagsToString() {
        StringBuffer buf = new StringBuffer();
        if (isPublic()) buf.append("public ");
        else if (isProtected()) buf.append("protected ");
        else if (isPrivate()) buf.append("private ");
        if (isBridge()) buf.append("<bridge> ");
        if (hasVarargs()) buf.append("<varargs> ");
        if (isStatic()) buf.append("static ");
        else if (isNative()) buf.append("native ");
        if (isAbstract()) buf.append("abstract ");
        else if (isFinal()) buf.append("final ");
        return buf.toString();
    }
}
