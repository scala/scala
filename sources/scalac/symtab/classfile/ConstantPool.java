/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import scalac.*;
import scalac.symtab.*;
import scalac.util.*;

public class ConstantPool implements ClassfileConstants {

    AbstractFileReader in;
    Signatures sigparser;

    /** the objects of the constant pool
     */
    protected Object[] poolObj;

    /** for every constant pool entry, an index into in.buf where the
     *  defining section of the entry is found
     */
    protected int[] poolIdx;

    /** constructor
     */
    protected ConstantPool(AbstractFileReader in, Signatures sigparser) {
        this.in = in;
        this.sigparser = sigparser;
    }

    /** index all constant pool entries, writing their start
     *  addresses into poolIdx
     */
    public void indexPool() {
        poolIdx = new int[in.nextChar()];
        poolObj = new Object[poolIdx.length];
        int i = 1;
        while (i < poolIdx.length) {
            poolIdx[i++] = in.bp;
            byte tag = in.nextByte();
            switch (tag) {
                case CONSTANT_UTF8:
                case CONSTANT_UNICODE: {
                    int len = in.nextChar();
                    in.skip(len);
                    break;
                }
                case CONSTANT_CLASS:
                case CONSTANT_STRING:
                    in.skip(2);
                    break;
                case CONSTANT_FIELDREF:
                case CONSTANT_METHODREF:
                case CONSTANT_INTFMETHODREF:
                case CONSTANT_NAMEANDTYPE:
                case CONSTANT_INTEGER:
                case CONSTANT_FLOAT:
                    in.skip(4);
                    break;
                case CONSTANT_LONG:
                case CONSTANT_DOUBLE:
                    in.skip(8);
                    i++;
                    break;
                default:
                    throw new RuntimeException("bad constant pool tag: " + tag +
                        " at " + (in.bp - 1));
            }
        }
    }

    /** if name is an array type or class signature, return the
     *  corresponding type; otherwise return a class definition with given name
     */
    protected Object classOrType(Name name) {
        if ((name.charAt(0) == '[') || (name.charAt(name.length() - 1) == ';')) {
            byte[] ascii = name.toAscii();
            return sigparser.sigToType(ascii, 0, ascii.length);
        } else
            return name;
    }

    /** read constant pool entry at start address i, use poolObj as a cache.
     */
    public Object readPool(int i) {
        if (poolObj[i] != null)
            return poolObj[i];
        int index = poolIdx[i];
        if (index == 0)
            return null;
        switch (in.byteAt(index)) {
            case CONSTANT_UTF8:
                poolObj[i] = Name.fromAscii(in.buf, index + 3, in.getChar(index + 1));
                break;
            case CONSTANT_UNICODE:
                throw new RuntimeException("can't read unicode strings in classfiles");
            case CONSTANT_CLASS:
                poolObj[i] = classOrType(readExternal(in.getChar(index + 1)));
                break;
            case CONSTANT_FIELDREF: {
                //Symbol owner = (Symbol)readPool(in.getChar(index + 1));
                //NameAndType nt = (NameAndType)readPool(in.getChar(index + 3));
                //poolObj[i] = new TermSymbol(Kinds.VAR, Position.NOPOS, nt.name, owner, 0)
                //    .type(sigparser.sigToType(Name.names, nt.sig.index, nt.sig.length()));
                throw new RuntimeException("can't read constant_fieldrefs in classfiles");
            }
            case CONSTANT_METHODREF:
            case CONSTANT_INTFMETHODREF: {
                //Symbol owner = (Symbol)readPool(in.getChar(index + 1));
                //NameAndType nt = (NameAndType)readPool(in.getChar(index + 3));
                //poolObj[i] = new TermSymbol(Kinds.FUN, Position.NOPOS, nt.name, owner, 0)
                //        .type(sigparser.sigToType(Name.names, nt.sig.index, nt.sig.length()));
                throw new RuntimeException("can't read constant_methodrefs in classfiles");
            }
            case CONSTANT_NAMEANDTYPE:
                poolObj[i] = new NameAndType((Name)readPool(in.getChar(index + 1)),
                                             readExternal(in.getChar(index + 3)));
                break;
            case CONSTANT_STRING:
            	poolObj[i] = ((Name)readPool(in.getChar(index + 1))).toString();
            	break;
            case CONSTANT_INTEGER:
            	poolObj[i] = new Integer(in.getInt(index + 1));
            	break;
            case CONSTANT_FLOAT:
            	poolObj[i] = new Float(in.getFloat(index + 1));
            	break;
            case CONSTANT_LONG:
            	poolObj[i] = new Long(in.getLong(index + 1));
            	break;
            case CONSTANT_DOUBLE:
                poolObj[i] = new Double(in.getDouble(index + 1));
            	break;
            default:
                throw new RuntimeException("bad constant pool tag: " + in.byteAt(index));
        }
        return poolObj[i];
    }

    /** return internal representation of buf[offset..offset+len-1],
     *  converting '/' to '.'
     */
    public byte[] internalize(byte[] buf, int offset, int len) {
        byte[] translated = new byte[len];
        for (int j = 0; j < len; j++) {
            byte b = buf[offset + j];
            if (b == '/')
                translated[j] = '.';
            else
                translated[j] = b;
        }
        return translated;
    }

    /** read a constant pool string and convert to internal representation.
     */
    public Name readExternal(int i) {
        if (poolObj[i] == null) {
            int index = poolIdx[i];
            if (in.byteAt(index) == CONSTANT_UTF8) {
                int len = in.getChar(index + 1);
                byte[] translated = internalize(in.buf, index + 3, len);
                poolObj[i] = Name.fromAscii(translated, 0, len);
            }
        }
        return (Name)poolObj[i];
    }

    /** the name and type signature of a method or field
     */
    public static final class NameAndType {
        public Name name;
        public Name sig;

        public NameAndType(Name name, Name sig) {
            this.name = name;
            this.sig = sig;
        }
    }
}
