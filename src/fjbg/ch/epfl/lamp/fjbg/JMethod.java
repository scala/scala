// $Id$

package ch.epfl.lamp.fjbg;

import java.util.LinkedList;
import java.util.Iterator;
import java.io.DataInputStream;
import java.io.IOException;

/**
 * Representation of a Java method.
 *
 * @version 1.0
 * @author Michel Schinz
 */

public class JMethod extends JFieldOrMethod {
    public final static String CLASS_CONSTRUCTOR_NAME = "<clinit>";
    public final static String INSTANCE_CONSTRUCTOR_NAME = "<init>";

    protected final JCode code;
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
            JConstantPool pool = owner.getConstantPool();
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

        // Fetch code from the attributes.
        setCode: {
            Iterator attrIt = attributes.iterator();
            while (attrIt.hasNext()) {
                Object attr = attrIt.next();
                if (attr instanceof JCodeAttribute) {
                    code = ((JCodeAttribute)attr).code;
                    break setCode;
                }
            }
            code = null;
        }
        argNames = null;        // TODO get from attribute
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

    public String[] getArgumentNames() {
        return argNames;
    }

    public JCode getCode() {
        assert !isAbstract();
        return code;
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
}
