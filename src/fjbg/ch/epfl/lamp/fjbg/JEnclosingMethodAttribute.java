
package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

/**
 * Sourcefile attribute, which can be attached to class files to
 * associate them with their enclosing method. A class may have
 * at most one EclosingMethod attribute.
 *
 * @version 1.0
 * @author Michel Schinz
 */

public class JEnclosingMethodAttribute extends JAttribute {
    protected final int classIdx;
    protected final int nameAndTypeIdx;

    public JEnclosingMethodAttribute(FJBGContext context,
                                     JClass clazz,
                                     String className, String methodName, JType methodType) {
        super(context, clazz);
        this.classIdx = clazz.getConstantPool().addClass(className);
        this.nameAndTypeIdx = clazz.getConstantPool().addNameAndType(methodName, methodType.getSignature());
    }

    public String getName() { return "EnclosingMethod"; }

    protected int getSize() {
        return 4;
    }

    protected void writeContentsTo(DataOutputStream stream) throws IOException {
        stream.writeShort(classIdx);
        stream.writeShort(nameAndTypeIdx);
    }
}
