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
    protected final Type OBJECT_TYPE;
    protected final Type STRING_TYPE;
    protected final Type ARRAY_TYPE;

    public JavaTypeCreator(Definitions definitions) {
        this.definitions = definitions;
        this.ANY_TYPE = classType(definitions.ANY_CLASS);
        this.DOUBLE_TYPE = classType(definitions.DOUBLE_CLASS);
        this.FLOAT_TYPE = classType(definitions.FLOAT_CLASS);
        this.LONG_TYPE = classType(definitions.LONG_CLASS);
        this.INT_TYPE = classType(definitions.INT_CLASS);
        this.CHAR_TYPE = classType(definitions.CHAR_CLASS);
        this.SHORT_TYPE = classType(definitions.SHORT_CLASS);
        this.BYTE_TYPE = classType(definitions.BYTE_CLASS);
        this.BOOLEAN_TYPE = classType(definitions.BOOLEAN_CLASS);
        this.UNIT_TYPE = classType(definitions.UNIT_CLASS);
        this.OBJECT_TYPE = classType(definitions.OBJECT_CLASS);
        this.STRING_TYPE = classType(definitions.STRING_CLASS);
        this.ARRAY_TYPE = classType(definitions.ARRAY_CLASS);
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

    public Type objectType() {
        return OBJECT_TYPE;
    }

    public Type stringType() {
        return STRING_TYPE;
    }

    public Type classType(String classname) {
        return classType(definitions.getClass(classname));
    }

    public Type classType(Symbol clasz) {
        return clasz.staticType();
    }

    public Type arrayType(Type elemtpe) {
        return Type.appliedType(ARRAY_TYPE, new Type[]{elemtpe});
    }

    public Type methodType(Type[] argtpes, Type restpe, Type[] thrown) {
        Symbol[] args = new Symbol[argtpes.length];
        for (int i = 0; i < args.length; i++) {
            args[i] = Symbol.NONE.newTerm( // !!! should be newVParam
                Position.NOPOS, Modifiers.PARAM, Name.fromString("x" + i));
            args[i].setInfo(objToAny(argtpes[i]));
        }
        return new MethodType(args, restpe);
    }
    private Type objToAny(Type tp) {
	if (tp.isSameAs(OBJECT_TYPE))
	    return ANY_TYPE;
	else
	    return tp;
    }

    public Type packageType(Name packagename) {
        return null;
    }

    /** return the constant type for the given constant.
     */
    public Type constantType(AConstant value) {
        return Type.constantType(value);
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
            return stringType();
        else if (value instanceof Boolean)
            return booleanType();
        else
	    throw Debug.abort("unknown constant type", value.getClass());
    }

}
