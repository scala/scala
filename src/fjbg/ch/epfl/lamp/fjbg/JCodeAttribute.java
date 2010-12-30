/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

/**
 * Code attribute, containing code of methods.
 *
 * A Code attribute contains the JVM instructions and auxiliary information
 * for a single method, instance initialization method, or class or interface
 * initialization method. See section 4.8.3 of the JVM specification.
 *
 * @author Michel Schinz, Stephane Micheloud
 * @version 1.1
 */

public class JCodeAttribute extends JAttribute {
    protected final JCode code;
    protected final JMethod owner;
    protected static int UNKNOWN_STACK_SIZE = Integer.MIN_VALUE;
    protected final int maxStackSize;
    protected final int maxLocals;

    public JCodeAttribute(FJBGContext context, JClass clazz, JMethod owner) {
        super(context, clazz);
        this.owner = owner;

        this.maxStackSize = UNKNOWN_STACK_SIZE;
        this.maxLocals = 0; // unknown
        this.code = owner.getCode();

        assert clazz == owner.getOwner();
    }

    public JCodeAttribute(FJBGContext context,
                          JClass clazz,
                          Object owner,
                          String name,
                          int size,
                          DataInputStream stream)
        throws IOException {
        super(context, clazz, name);
        this.owner = (JMethod)owner;

        this.maxStackSize = stream.readShort();
        this.maxLocals = stream.readShort();
        this.code = context.JCode(clazz, (JMethod)owner, stream);

        int handlersCount = stream.readShort();
        for (int i = 0; i < handlersCount; ++i)
            code.addExceptionHandler(code.new ExceptionHandler(stream));
        List/*<JAttribute>*/ attributes =
            JAttribute.readFrom(context, clazz, code, stream);
        Iterator attrIt = attributes.iterator();
        while (attrIt.hasNext())
            code.addAttribute((JAttribute)attrIt.next());

        assert name.equals(getName());
    }

    public String getName() { return "Code"; }

    // Follows javap output format for Code attribute.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer("  Code:");
        buf.append("\n   Stack=");
        buf.append(maxStackSize);
        buf.append(", Locals=");
        buf.append(maxLocals);
        buf.append(", Args_size=");
        buf.append(owner.getArgsSize());
        buf.append(code);
        buf.append("\n");
        Iterator it = code.getAttributes().iterator();
        while (it.hasNext()) {
            JAttribute attr = (JAttribute)it.next();
            buf.append(attr);
            buf.append("\n");
        }
        return buf.toString();
    }

    protected int getSize() {
        int handlersNum = code.getExceptionHandlers().size();

        int attrsSize = 0;
        Iterator attrsIt = code.getAttributes().iterator();
        while (attrsIt.hasNext()) {
            JAttribute attr = (JAttribute)attrsIt.next();
            attrsSize += attr.getSize() + 6;
        }

        return 2                // max stack
            + 2                 // max locals
            + 4                 // code size
            + code.getSize()    // code
            + 2                 // exception table size
            + 8 * handlersNum   // exception table
            + 2                 // attributes count
            + attrsSize;        // attributes
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        List/*<ExceptionHandler>*/ handlers = code.getExceptionHandlers();

        stream.writeShort(code.getMaxStackSize());
        stream.writeShort(owner.getMaxLocals());

        code.writeTo(stream);

        stream.writeShort(handlers.size());
        Iterator handlerIt = handlers.iterator();
        while (handlerIt.hasNext())
            ((JCode.ExceptionHandler)handlerIt.next()).writeTo(stream);
        JAttribute.writeTo(code.getAttributes(), stream);
    }
}
