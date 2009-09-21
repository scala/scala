// $Id$

package ch.epfl.lamp.fjbg;

import java.util.*;
import java.io.*;

/**
 * Code attribute, containing code of methods.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JCodeAttribute extends JAttribute {
    protected final JCode code;

    public JCodeAttribute(FJBGContext context, JClass clazz, JMethod owner) {
        super(context, clazz);
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
        super(context, clazz);

        stream.readShort();     // skip max stack size
        stream.readShort();     // skip max locals

        this.code = context.JCode(clazz, (JMethod)owner, stream);

        int handlersCount = stream.readShort();
        for (int i = 0; i < handlersCount; ++i)
            code.addExceptionHandler(code.new ExceptionHandler(stream));

        List/*<JAttribute>*/ attributes =
            JAttribute.readFrom(context, clazz, owner, stream);
        Iterator attrIt = attributes.iterator();
        while (attrIt.hasNext())
            code.addAttribute((JAttribute)attrIt.next());

        assert name.equals(getName());
    }

    public String getName() { return "Code"; }

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
        List/*<JExceptionHandler>*/ handlers = code.getExceptionHandlers();

        stream.writeShort(code.getMaxStackSize());
        stream.writeShort(code.getOwner().getMaxLocals());

        code.writeTo(stream);

        stream.writeShort(handlers.size());
        Iterator handlerIt = handlers.iterator();
        while (handlerIt.hasNext())
            ((JCode.ExceptionHandler)handlerIt.next()).writeTo(stream);

        JAttribute.writeTo(code.getAttributes(), stream);
    }
}
