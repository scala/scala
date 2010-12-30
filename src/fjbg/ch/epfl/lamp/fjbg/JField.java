/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.IOException;

/**
 * Java class field.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JField extends JFieldOrMethod {

    protected JField(FJBGContext context,
                     JClass owner,
                     int accessFlags,
                     String name,
                     JType type) {
        super(context, owner, accessFlags, name, type);
    }

    protected JField(FJBGContext context,
                     JClass owner,
                     DataInputStream stream)
        throws IOException {
        super(context, owner, stream);
    }

    // Follows javap output format for fields.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer(flagsToString());
        buf.append(toExternalName(getType()));
        buf.append(" ");
        buf.append(getName());
        buf.append(";\n");
        java.util.Iterator attrsIt = attributes.iterator();
        while (attrsIt.hasNext()) {
            JAttribute attrs = (JAttribute)attrsIt.next();
            buf.append(attrs);
        }
        return buf.toString();
    }

    private String flagsToString() {
        StringBuffer buf = new StringBuffer();
        if (isPublic()) buf.append("public ");
        else if (isProtected()) buf.append("protected ");
        else if (isPrivate()) buf.append("private ");
        if (isStatic()) buf.append("static ");
        else if (isTransient()) buf.append("transient ");
        else if (isVolatile()) buf.append("volatile ");
        if (isAbstract()) buf.append("abstract ");
        else if (isFinal()) buf.append("final ");
        return buf.toString();
    }
}
