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
import java.util.*;
import Type.*;

public class Signatures {

    /** signature constants
     */
    Name BYTE_SIG = Name.fromString("B");
    Name SHORT_SIG = Name.fromString("S");
    Name CHAR_SIG = Name.fromString("C");
    Name INT_SIG = Name.fromString("I");
    Name LONG_SIG = Name.fromString("J");
    Name FLOAT_SIG = Name.fromString("F");
    Name DOUBLE_SIG = Name.fromString("D");
    Name BOOLEAN_SIG = Name.fromString("Z");
    Name VOID_SIG = Name.fromString("V");
    Name CLASS_SIG = Name.fromString("L");
    Name ARRAY_SIG = Name.fromString("[");
    Name ARGBEGIN_SIG = Name.fromString("(");
    Name ARGEND_SIG = Name.fromString(")");

    Global global;
    JavaTypeFactory make;


    public Signatures(Global global, JavaTypeFactory make) {
        this.make = make;
        this.global = global;
    }

    /** the type represented by signature[offset..].
     */
    protected byte[] signature;
    protected int sigp;
    protected int limit;

    public Type sigToType(byte[] sig, int offset, int len) {
        signature = sig;
        sigp = offset;
        limit = offset + len;
        return sigToType();
    }

    protected Type sigToType() {
        switch (signature[sigp]) {
            case 'B':
                sigp++;
                return make.byteType();
            case 'C':
                sigp++;
                return make.charType();
            case 'D':
                sigp++;
                return make.doubleType();
            case 'F':
                sigp++;
                return make.floatType();
            case 'I':
                sigp++;
                return make.intType();
            case 'J':
                sigp++;
                return make.longType();
            case 'L':
                sigp++;
                int start = sigp;
                while (signature[sigp] != ';')
                    sigp++;
                return make.classType(Name.fromAscii(signature, start, (sigp++) - start));
            case 'S':
                sigp++;
                return make.shortType();
            case 'V':
                sigp++;
                return make.voidType();
            case 'Z':
                sigp++;
                return make.booleanType();
            case '[':
                sigp++;
                while (('0' <= signature[sigp]) && (signature[sigp] <= '9'))
                    sigp++;
                return make.arrayType(sigToType());
            case '(':
                return make.methodType(sigToTypes(')'), sigToType(), Type.EMPTY_ARRAY);
            default:
                global.error("bad signature: " +
                    SourceRepresentation.ascii2string(signature, sigp, 1));
                return Type.ErrorType;
        }
    }

    protected Type[] sigToTypes(char terminator) {
        sigp++;
        return sigToTypes(terminator, 0);
    }

    protected Type[] sigToTypes(char terminator, int i) {
        if (signature[sigp] == terminator) {
            sigp++;
            return new Type[i];
        } else {
            Type    t = sigToType();
            Type[]  vec = sigToTypes(terminator, i+1);
            vec[i] = t;
            return vec;
        }
    }
}
