/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab;

import scalac.*;
import scalac.util.*;
import scalac.symtab.classfile.*;
import Type.*;

public class Definitions {

    /** the root module
     */
    public final Symbol ROOT;
    public final Symbol ROOT_CLASS;
    public final Type   ROOT_TYPE;

    /** the scala module
     */
    public final Symbol SCALA;
    public final Symbol SCALA_CLASS;
    public final Type   SCALA_TYPE;

    /** the java module
     */
    public final Symbol JAVA;
    public final Symbol JAVA_CLASS;
    public final Type   JAVA_TYPE;

    /** the java.lang module
     */
    public final Symbol JAVALANG;
    public final Symbol JAVALANG_CLASS;
    public final Type   JAVALANG_TYPE;

    /** the scala.runtime module
     */
    public final Symbol SCALARUNTIME;
    public final Symbol SCALARUNTIME_CLASS;
    public final Type   SCALARUNTIME_TYPE;

    /** the partial function class
     */
    public final Symbol PARTIALFUNCTION_CLASS;

    /** the null value
     */
    public final Symbol NULL;

    /** the scala.Any class
     */
    public final Symbol ANY_CLASS;
    public final Type   ANY_TYPE;

    /** some methods of the scala.Any class
     */
    public final Symbol MATCH;
    public final Symbol IS;
    public final Symbol AS;
    public final Symbol EQEQ;
    public final Symbol BANGEQ;
    public final Symbol TOSTRING;
    public final Symbol HASHCODE;

    /** a method of class java.lang.Throwable
     */
    public final Symbol THROW;

    /** the scala.AnyVal class
     */
    public final Symbol ANYVAL_CLASS;
    public final Type   ANYVAL_TYPE;

    /** the scala.AnyRef class
     */
    public final Symbol ANYREF_CLASS;
    public final Type   ANYREF_TYPE;

    /** the java.lang.Object class
     */
    public final Symbol JAVA_OBJECT_CLASS;
    public final Type   JAVA_OBJECT_TYPE;

    /** the scala.Object class
     */
    public final Symbol OBJECT_CLASS;
    public final Type   OBJECT_TYPE;

    /** the primitive types
     */
    public final Symbol BYTE_CLASS;
    public final Type   BYTE_TYPE;
    public final Symbol SHORT_CLASS;
    public final Type   SHORT_TYPE;
    public final Symbol CHAR_CLASS;
    public final Type   CHAR_TYPE;
    public final Symbol INT_CLASS;
    public final Type   INT_TYPE;
    public final Symbol LONG_CLASS;
    public final Type   LONG_TYPE;
    public final Symbol FLOAT_CLASS;
    public final Type   FLOAT_TYPE;
    public final Symbol DOUBLE_CLASS;
    public final Type   DOUBLE_TYPE;
    public final Symbol BOOLEAN_CLASS;
    public final Type   BOOLEAN_TYPE;
    public final Symbol UNIT_CLASS;
    public final Type   UNIT_TYPE;

    /** the array class
     */
    public final Symbol ARRAY_CLASS;

    /** types from java.lang
     */
    public final Symbol JAVA_STRING_CLASS;
    public final Type   JAVA_STRING_TYPE;
    public final Symbol JAVA_THROWABLE_CLASS;
    public final Type   JAVA_THROWABLE_TYPE;

    /** types from scala
     */
    public final Symbol STRING_CLASS;
    public final Type   STRING_TYPE;

    public final Symbol SEQ_CLASS;

    /** string concatenation pseudo-methods of classes scala.Any and
     *  java.lang.String
     */
    //public final Symbol ANY_PLUS_STRING;
    public final Symbol STRING_PLUS_ANY;

    /** members of class Boolean
     */
    private Symbol BARBAR;
    private Symbol AMPAMP;

    public Symbol BARBAR() { loadBooleanMembers(); return BARBAR; }
    public Symbol AMPAMP() { loadBooleanMembers(); return AMPAMP; }

    public Definitions(Global global) {
        // a hack to make definitions accessible earlier to other
        // components
        global.definitions = this;
        PackageParser pparser = new PackageParser(global);

        // this is the root value; all top-level functions,
        // modules etc. are a member of this value
        ROOT = TermSymbol.newJavaPackageModule(
            Names.EMPTY, Symbol.NONE, pparser);
        ROOT_CLASS = ROOT.moduleClass();
        // this is a prefix for all types inside of the anonymous package
        ROOT_TYPE = ROOT_CLASS.thisType();

        // the scala module
        SCALA = getModule(Names.scala);
        // the scala class
        SCALA_CLASS = SCALA.moduleClass();
        // the scala package as a prefix
        SCALA_TYPE = Type.singleType(ROOT_TYPE, SCALA);

        // the java module
        JAVA = getModule(Names.java);
        // the java class
        JAVA_CLASS = JAVA.moduleClass();
        // the java package as a prefix
        JAVA_TYPE = Type.singleType(ROOT_TYPE, JAVA);

        // the java.lang module
        JAVALANG = getModule(Names.java_lang);
        // the java.lang class
        JAVALANG_CLASS = JAVALANG.moduleClass();
        // the java.lang package as a prefix
        JAVALANG_TYPE = Type.singleType(JAVA_TYPE, JAVALANG);

        // the scala.runtime module
        SCALARUNTIME = getModule(Names.scala_runtime);
        // the scala.runtime class
        SCALARUNTIME_CLASS = SCALARUNTIME.moduleClass();
        // the scala.runtime package as a prefix
        SCALARUNTIME_TYPE = Type.singleType(SCALA_TYPE, SCALARUNTIME);

        // the scala.ANY classs
        ANY_CLASS = new ClassSymbol(
	    Position.NOPOS, Names.Any.toTypeName(), SCALA_CLASS, Modifiers.JAVA);
        SCALA_CLASS.members().enter(ANY_CLASS);
        ANY_TYPE = ANY_CLASS.typeConstructor();
        ANY_CLASS.setInfo(Type.compoundType(Type.EMPTY_ARRAY, new Scope(), ANY_CLASS));
        ANY_CLASS.constructor().setInfo(
	    Type.PolyType(Symbol.EMPTY_ARRAY, ANY_TYPE));

        // the java.lang.OBJECT class
        JAVA_OBJECT_CLASS = getClass(Names.java_lang_Object);
        JAVA_OBJECT_TYPE = JAVA_OBJECT_CLASS.typeConstructor();
        JAVA_OBJECT_CLASS.setInfo(pparser.classCompletion);

	// the scala.PartialFunction class
	PARTIALFUNCTION_CLASS = getClass(Names.scala_PartialFunction);

        // the scala.ANYREF class
	ANYREF_CLASS = new TypeSymbol(
	    Kinds.ALIAS, Position.NOPOS, Names.AnyRef.toTypeName(),
	    SCALA_CLASS, Modifiers.JAVA)
	    .setInfo(JAVA_OBJECT_TYPE);
	ANYREF_TYPE = ANYREF_CLASS.typeConstructor();
        SCALA.members().enter(ANYREF_CLASS);

        // the scala.OBJECT class
	OBJECT_CLASS = getClass(Names.scala_Object);
        OBJECT_TYPE = OBJECT_CLASS.typeConstructor();

        // the scala.ANYVAL class
	ANYVAL_CLASS = getClass(Names.scala_AnyVal);
        ANYVAL_TYPE = ANYVAL_CLASS.typeConstructor();

        // the primitive types
        DOUBLE_CLASS = getClass(Names.scala_Double);
        DOUBLE_TYPE = DOUBLE_CLASS.typeConstructor();
        FLOAT_CLASS = getClass(Names.scala_Float);
        FLOAT_TYPE = FLOAT_CLASS.typeConstructor();
        LONG_CLASS = getClass(Names.scala_Long);
        LONG_TYPE = LONG_CLASS.typeConstructor();
        INT_CLASS = getClass(Names.scala_Int);
        INT_TYPE = INT_CLASS.typeConstructor();
        CHAR_CLASS = getClass(Names.scala_Char);
        CHAR_TYPE = CHAR_CLASS.typeConstructor();
        SHORT_CLASS = getClass(Names.scala_Short);
        SHORT_TYPE = SHORT_CLASS.typeConstructor();
        BYTE_CLASS = getClass(Names.scala_Byte);
        BYTE_TYPE = BYTE_CLASS.typeConstructor();
        BOOLEAN_CLASS = getClass(Names.scala_Boolean);
        BOOLEAN_TYPE = BOOLEAN_CLASS.typeConstructor();
        UNIT_CLASS = getClass(Names.scala_Unit);
        UNIT_TYPE = UNIT_CLASS.typeConstructor();

        // the array class
        ARRAY_CLASS = getClass(Names.scala_Array);

        // add members to java.lang.Throwable
        JAVA_THROWABLE_CLASS = getClass(Names.java_lang_Throwable);
        JAVA_THROWABLE_TYPE = JAVA_THROWABLE_CLASS.typeConstructor();
        THROW = new TermSymbol(
	    Position.NOPOS, Names.throw_, JAVA_THROWABLE_CLASS, Modifiers.FINAL);
        Symbol tvar = newTypeParameter(THROW, ANY_TYPE);
        THROW.setInfo(Type.PolyType(new Symbol[]{tvar}, tvar.type()));
        JAVA_THROWABLE_CLASS.members().enter(THROW);

        // add the java.lang.String class to the scala package
        JAVA_STRING_CLASS = getClass(Names.java_lang_String);
        JAVA_STRING_TYPE = JAVA_STRING_CLASS.typeConstructor();
        STRING_CLASS = new TypeSymbol(
	    Kinds.ALIAS, Position.NOPOS, Names.String.toTypeName(), SCALA_CLASS, 0)
	    .setInfo(JAVA_STRING_TYPE);
	STRING_TYPE = STRING_CLASS.typeConstructor();
        SCALA.members().enter(STRING_CLASS);

	SEQ_CLASS = getClass(Names.scala_Seq);

	/*
        ANY_PLUS_STRING = new TermSymbol(
	    Position.NOPOS, Names.PLUS, ANY_CLASS, Modifiers.FINAL);
        ANY_PLUS_STRING.setInfo(
	    Type.MethodType(
		new Symbol[]{newParameter(ANY_PLUS_STRING, STRING_TYPE)},
		STRING_TYPE));
        ANY_CLASS.members().enter(ANY_PLUS_STRING);
	*/

        STRING_PLUS_ANY = new TermSymbol(
	    Position.NOPOS, Names.PLUS, STRING_CLASS, Modifiers.FINAL);
        STRING_PLUS_ANY.setInfo(
	    Type.MethodType(
		new Symbol[]{newParameter(STRING_PLUS_ANY, ANY_TYPE)},
		STRING_TYPE));
        STRING_CLASS.members().enter(STRING_PLUS_ANY);

        // add members to class scala.Any
        MATCH = new TermSymbol(
	    Position.NOPOS, Names.match, ANY_CLASS, Modifiers.FINAL);
        MATCH.setInfo(
	    Type.MethodType(
		new Symbol[]{newParameter(MATCH, OBJECT_TYPE)},
		OBJECT_TYPE));
        ANY_CLASS.members().enter(MATCH);

        AS = new TermSymbol(
	    Position.NOPOS, Names.as, ANY_CLASS, Modifiers.FINAL);
        tvar = newTypeParameter(AS, ANY_TYPE);
        AS.setInfo(Type.PolyType(new Symbol[]{tvar}, tvar.type()));
        ANY_CLASS.members().enter(AS);

        IS = new TermSymbol(
	    Position.NOPOS, Names.is, ANY_CLASS, Modifiers.FINAL);
        IS.setInfo(Type.PolyType(new Symbol[]{newTypeParameter(IS, ANY_TYPE)},
				 BOOLEAN_TYPE));
        ANY_CLASS.members().enter(IS);

        EQEQ = new TermSymbol(
	    Position.NOPOS, Names.EQEQ, ANY_CLASS, 0);
        EQEQ.setInfo(Type.MethodType(new Symbol[]{newParameter(EQEQ, ANY_TYPE)},
				     BOOLEAN_TYPE));
        ANY_CLASS.members().enter(EQEQ);

        BANGEQ = new TermSymbol(
	    Position.NOPOS, Names.BANGEQ, ANY_CLASS, 0);
        BANGEQ.setInfo(Type.MethodType(new Symbol[]{newParameter(BANGEQ, ANY_TYPE)},
				       BOOLEAN_TYPE));
        ANY_CLASS.members().enter(BANGEQ);

        TOSTRING = new TermSymbol(
	    Position.NOPOS, Names.toString, ANY_CLASS, 0);
        TOSTRING.setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, STRING_TYPE));
        ANY_CLASS.members().enter(TOSTRING);

        HASHCODE = new TermSymbol(
	    Position.NOPOS, Names.hashCode, ANY_CLASS, 0);
        HASHCODE.setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, INT_TYPE));
        ANY_CLASS.members().enter(HASHCODE);

        // add a null value to the root scope
	NULL = new TermSymbol(
	    Position.NOPOS, Names.null_, ROOT_CLASS, 0);
	tvar = newTypeParameter(NULL, ANYREF_TYPE);
        NULL.setInfo(Type.PolyType(new Symbol[]{tvar}, tvar.type()));
        ROOT.members().enter(NULL);
    }

    private Symbol newParameter(Symbol owner, Type tp) {
	return new TermSymbol(Position.NOPOS, Name.fromString("v"), owner, Modifiers.PARAM)
	    .setInfo(tp);
    }

    private Symbol newTypeParameter(Symbol owner, Type bound) {
	return new TypeSymbol(
	    Kinds.TYPE, Position.NOPOS, Name.fromString("T").toTypeName(), owner, Modifiers.PARAM)
	    .setInfo(bound);
    }

    public Symbol getModule(Name fullname) {
	Scope scope = ROOT_CLASS.members();
        int i = 0;
        int j = fullname.pos((byte)'.', i);
	while (j < fullname.length()) {
	    scope = scope.lookup(fullname.subName(i, j)).members();
	    i = j + 1;
	    j = fullname.pos((byte)'.', i);
	}
	Symbol sym = scope.lookup(fullname.subName(i, fullname.length()));
        if (!sym.isModule()) {
            switch (sym.type()) {
            case OverloadedType(Symbol[] alts, Type[] alttypes):
                for (int k = 0; k < alts.length; k++)
                    if ((sym = alts[k]).isModule()) break;
            }
        }
        assert sym.isModule() : "no module '" + fullname + "'";
        return sym;
    }

    public Symbol getClass(Name fullname) {
	Scope scope = ROOT_CLASS.members();
        int i = 0;
        int j = fullname.pos((byte)'.', i);
	while (j < fullname.length()) {
	    scope = scope.lookup(fullname.subName(i, j)).members();
	    i = j + 1;
	    j = fullname.pos((byte)'.', i);
	}
	Symbol sym = scope.lookup(fullname.subName(i, fullname.length()).toTypeName());
	assert sym.kind != Kinds.NONE : "no class '" + fullname + "'";
	return sym;
    }

    public Type getType(Name fullname) {
	return getClass(fullname).type();
    }

    public Type getJavaType(Name fullname) {
	return getClass(fullname).typeConstructor();
    }

    private void loadBooleanMembers() {
        if (BARBAR != null) return;
        Symbol booleanStatics = getModule(Names.scala_Boolean);
        BARBAR = BOOLEAN_TYPE.lookup(Names.BARBAR);
        AMPAMP = BOOLEAN_TYPE.lookup(Names.AMPAMP);
    }

    public Type arrayType(Type elemtpe) {
        return Type.appliedType(ARRAY_CLASS.typeConstructor(), new Type[]{elemtpe});
    }

    public Type functionType(Type[] argtps, Type restp) {
	Type[] argtps1 = new Type[argtps.length + 1];
	System.arraycopy(argtps, 0, argtps1, 0, argtps.length);
	argtps1[argtps.length] = Type.covarType(restp);
	return Type.appliedType(
	    getType(Name.fromString("scala.Function" + argtps.length)),
	    argtps1);
    }

    public Type partialFunctionType(Type argtpe, Type restpe) {
	Type[] argtps1 = new Type[2];
	argtps1[0] = argtpe;
	argtps1[1] = Type.covarType(restpe);
	return Type.appliedType(PARTIALFUNCTION_CLASS.typeConstructor(), argtps1);
    }

    public Type seqType(Type argtpe) {
	return Type.appliedType(getType(Names.scala_Seq), new Type[]{argtpe});
    }
}
