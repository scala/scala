// $Id$

package ch.epfl.lamp.fjbg;

import java.io.*;

/**
 * Abstract superclass for a Java field or method.
 *
 * @author Michel Schinz
 * @version 1.0
 */

abstract public class JFieldOrMethod extends JMember {

    protected final JClass owner;
    protected final JType type;

    protected final int nameIndex, signatureIndex;

    protected JFieldOrMethod(FJBGContext context,
                             JClass owner,
                             int accessFlags,
                             String name,
                             JType type) {
        super(context, accessFlags, name);
        this.owner = owner;
        this.type = type;

        JConstantPool pool = owner.getConstantPool();
        nameIndex = pool.addUtf8(name);
        signatureIndex = pool.addUtf8(type.getSignature());
    }

    protected JFieldOrMethod(FJBGContext context,
                             JClass owner,
                             DataInputStream stream)
        throws IOException {
        super(context);
        this.owner = owner;
        this.accessFlags = stream.readShort();
        this.nameIndex = stream.readShort();
        this.name = owner.pool.lookupUtf8(nameIndex);
        this.signatureIndex = stream.readShort();
        this.type = JType.parseSignature(owner.pool.lookupUtf8(signatureIndex));
        this.attributes.addAll(JAttribute.readFrom(context, owner, this, stream));
    }

    public void freeze() throws JCode.OffsetTooBigException {
        assert !frozen;
        frozen = true;
    }

    public JClass getOwner() { return owner; }

    public JType getType() { return type; }

    public JClass getJClass() { return owner; }

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

    public boolean isSuper() {
        return (accessFlags & JAccessFlags.ACC_SUPER) != 0;
    }

    public boolean isVolatile() {
        return (accessFlags & JAccessFlags.ACC_VOLATILE) != 0;
    }

    public boolean isTransient() {
        return (accessFlags & JAccessFlags.ACC_TRANSIENT) != 0;
    }

    public boolean isNative() {
        return (accessFlags & JAccessFlags.ACC_NATIVE) != 0;
    }

    public boolean isInterface() {
        return (accessFlags & JAccessFlags.ACC_INTERFACE) != 0;
    }

    public boolean isAbstract() {
        return (accessFlags & JAccessFlags.ACC_ABSTRACT) != 0;
    }

    public boolean isStrict() {
        return (accessFlags & JAccessFlags.ACC_STRICT) != 0;
    }

    public void writeTo(DataOutputStream stream) throws IOException {
        if (! frozen) {
            try {
                freeze();
            }
            catch (JCode.OffsetTooBigException e) {
                throw new Error(e);
            }
        }
        stream.writeShort(accessFlags);
        stream.writeShort(nameIndex);
        stream.writeShort(signatureIndex);
        JAttribute.writeTo(getAttributes(), stream);
    }
}
