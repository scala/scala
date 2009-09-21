package ch.epfl.lamp.fjbg;

import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Iterator;

/**
 * BootstrapInvokeDynamic entry, as described by JSR 292 (invoke dynamic)
 *
 * @author Iulian Dragos
 *
 */
public class JBootstrapInvokeDynamic extends JAttribute {
	/** Constant pool of the current classfile. */
	private JConstantPool pool;

	private int classIndex = -1;

    public JBootstrapInvokeDynamic(FJBGContext context,
            JClass clazz, String className) {
    	super(context, clazz);
    	this.pool = clazz.pool;
    	this.classIndex = pool.addClass(className);
    }

    public String getName() { return "BootstrapInvokeDynamic"; }

	protected int getSize() {
		return 2;
	}

	protected void writeContentsTo(DataOutputStream stream) throws IOException {
		stream.writeShort(classIndex);
	}
}
