/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import ch.epfl.lamp.util.Position;
import scalac.Global;
import scalac.util.*;
import scalac.symtab.*;
import Type.*;


public class JavaTypeCreator implements JavaTypeFactory {

    protected Global global;

    public JavaTypeCreator(Global global) {
        this.global = global;
    }

    public Type byteType() {
        return global.definitions.BYTE_TYPE;
    }

    public Type shortType() {
        return global.definitions.SHORT_TYPE;
    }

    public Type charType() {
        return global.definitions.CHAR_TYPE;
    }

    public Type intType() {
        return global.definitions.INT_TYPE;
    }

    public Type longType() {
        return global.definitions.LONG_TYPE;
    }

    public Type floatType() {
        return global.definitions.FLOAT_TYPE;
    }

    public Type doubleType() {
        return global.definitions.DOUBLE_TYPE;
    }

    public Type booleanType() {
        return global.definitions.BOOLEAN_TYPE;
    }

    public Type voidType() {
        return global.definitions.UNIT_TYPE;
    }

    public Type classType(Name classname) {
        return global.definitions.getJavaType(classname);
    }

    public Type arrayType(Type elemtpe) {
        return global.definitions.arrayType(elemtpe);
    }

    public Type methodType(Type[] argtpes, Type restpe, Type[] thrown) {
        Symbol[] args = new Symbol[argtpes.length];
        for (int i = 0; i < args.length; i++) {
            args[i] = new TermSymbol(
                Position.NOPOS, Name.fromString("x" + i), Symbol.NONE, Modifiers.PARAM);
            args[i].setInfo(argtpes[i]);
        }
        return new MethodType(args, restpe);
    }

    public Type packageType(Name packagename) {
        return null;
    }
}
