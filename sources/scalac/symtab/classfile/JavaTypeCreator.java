/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab.classfile;

import ch.epfl.lamp.util.Position;
import scalac.atree.AConstant;
import scalac.util.Debug;
import scalac.util.Name;
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
        return classType(definitions.getClass(classname));
    }

    public Type classType(Symbol clasz) {
        return clasz.typeConstructor();
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

    /** return the constant type for the given constant.
     */
    public Type constantType(Type base, Object value) {
        return Type.constantType(constantValue(base, value));
    }

    private AConstant constantValue(Type base, Object value) {
        if (base.symbol() == definitions.BOOLEAN_CLASS)
            return AConstant.BOOLEAN(((Number)value).intValue() != 0);
        if (base.symbol() == definitions.BYTE_CLASS)
            return AConstant.BYTE(((Number)value).byteValue());
        if (base.symbol() == definitions.SHORT_CLASS)
            return AConstant.SHORT(((Number)value).shortValue());
        if (base.symbol() == definitions.CHAR_CLASS)
            return AConstant.CHAR((char)((Number)value).intValue());
        if (base.symbol() == definitions.INT_CLASS)
            return AConstant.INT(((Number)value).intValue());
        if (base.symbol() == definitions.LONG_CLASS)
            return AConstant.LONG(((Number)value).longValue());
        if (base.symbol() == definitions.FLOAT_CLASS)
            return AConstant.FLOAT(((Number)value).floatValue());
        if (base.symbol() == definitions.DOUBLE_CLASS)
            return AConstant.DOUBLE(((Number)value).doubleValue());
        if (base.symbol() == definitions.JAVA_STRING_CLASS)
            return AConstant.STRING((String)value);
    	throw Debug.abort("illegal value", value + " - " + base);
    }

    /** return the type of a given constant.
     */
    public Type typeOfValue(Object value) {
    	if (value instanceof Character)
            return charType();
        else if (value instanceof Integer)
            return intType();
        else if (value instanceof Long)
            return longType();
        else if (value instanceof Float)
            return floatType();
        else if (value instanceof Double)
            return doubleType();
        else if (value instanceof String)
            return definitions.JAVA_STRING_CLASS.typeConstructor();
        else if (value instanceof Boolean)
            return booleanType();
        else
	    throw Debug.abort("unknown constant type", value.getClass());
    }

}
