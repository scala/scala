/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import ch.epfl.lamp.util.Position;
import scalac.util.*;
import scalac.symtab.*;
import Type.*;


public class JavaTypeCreator implements JavaTypeFactory {

    protected final Definitions definitions;

    protected final Type ANY_TYPE;
    protected final Type DOUBLE_TYPE;
    protected final Type FLOAT_TYPE;
    protected final Type LONG_TYPE;
    protected final Type INT_TYPE;
    protected final Type CHAR_TYPE;
    protected final Type SHORT_TYPE;
    protected final Type BYTE_TYPE;
    protected final Type BOOLEAN_TYPE;
    protected final Type UNIT_TYPE;
    protected final Type ARRAY_TYPE;
    protected final Type JAVA_OBJECT_TYPE;

    public JavaTypeCreator(Definitions definitions) {
        this.definitions = definitions;
        this.ANY_TYPE = definitions.ANY_CLASS.typeConstructor();
        this.DOUBLE_TYPE = definitions.DOUBLE_CLASS.typeConstructor();
        this.FLOAT_TYPE = definitions.FLOAT_CLASS.typeConstructor();
        this.LONG_TYPE = definitions.LONG_CLASS.typeConstructor();
        this.INT_TYPE = definitions.INT_CLASS.typeConstructor();
        this.CHAR_TYPE = definitions.CHAR_CLASS.typeConstructor();
        this.SHORT_TYPE = definitions.SHORT_CLASS.typeConstructor();
        this.BYTE_TYPE = definitions.BYTE_CLASS.typeConstructor();
        this.BOOLEAN_TYPE = definitions.BOOLEAN_CLASS.typeConstructor();
        this.UNIT_TYPE = definitions.UNIT_CLASS.typeConstructor();
        this.ARRAY_TYPE = definitions.ARRAY_CLASS.typeConstructor();
        this.JAVA_OBJECT_TYPE =definitions.JAVA_OBJECT_CLASS.typeConstructor();
    }

    public Type anyType() {
        return ANY_TYPE;
    }

    public Type byteType() {
        return BYTE_TYPE;
    }

    public Type shortType() {
        return SHORT_TYPE;
    }

    public Type charType() {
        return CHAR_TYPE;
    }

    public Type intType() {
        return INT_TYPE;
    }

    public Type longType() {
        return LONG_TYPE;
    }

    public Type floatType() {
        return FLOAT_TYPE;
    }

    public Type doubleType() {
        return DOUBLE_TYPE;
    }

    public Type booleanType() {
        return BOOLEAN_TYPE;
    }

    public Type voidType() {
        return UNIT_TYPE;
    }

    public Type classType(Name classname) {
        return definitions.getClass(classname).typeConstructor();
    }

    public Type arrayType(Type elemtpe) {
        return Type.appliedType(ARRAY_TYPE, new Type[]{elemtpe});
    }

    public Type methodType(Type[] argtpes, Type restpe, Type[] thrown) {
        Symbol[] args = new Symbol[argtpes.length];
        for (int i = 0; i < args.length; i++) {
            args[i] = new TermSymbol(
                Position.NOPOS, Name.fromString("x" + i), Symbol.NONE, Modifiers.PARAM);
            args[i].setInfo(objToAny(argtpes[i]));
        }
        return new MethodType(args, restpe);
    }
    private Type objToAny(Type tp) {
	if (tp.isSameAs(JAVA_OBJECT_TYPE))
	    return ANY_TYPE;
	else
	    return tp;
    }

    public Type packageType(Name packagename) {
        return null;
    }
}
