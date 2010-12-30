/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * ConstantValue attribute representing the value of a constant field.
 *
 * There can be no more than one ConstantValue attribute in the attributes
 * table of a given field_info structure.. See section 4.8.2 of the JVM
 * specification.
 *
 * @author Stephane Micheloud
 * @version 1.0
 */

public class JConstantValueAttribute extends JAttribute {
    /** Constant pool of the current classfile. */
    private JConstantPool pool;

    protected int constantValueIndex;

    public JConstantValueAttribute(FJBGContext context,
                                  JClass clazz,
                                  JField field) {
        super(context, clazz);
        this.pool = clazz.pool;

        assert field.getOwner() == clazz;
    }

    public JConstantValueAttribute(FJBGContext context,
                                   JClass clazz,
                                   Object owner, // JField
                                   String name,
                                   int size,
                                   DataInputStream stream)
        throws IOException {
        super(context, clazz, name);
        this.pool = clazz.pool;

        this.constantValueIndex = stream.readShort();

        assert name.equals(getName());
    }

    public String getName() { return "ConstantValue"; }

    // Follows javap output format for ConstantValue attribute.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer("  Constant value: ");
        buf.append(pool.lookupEntry(constantValueIndex));
        return buf.toString();
    }

    protected int getSize() {
        return 2; // Short.SIZE
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(constantValueIndex);
    }
}
