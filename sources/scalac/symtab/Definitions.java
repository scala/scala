/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab;

import ch.epfl.lamp.util.Position;
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

    /** the null value
     */
    public final Symbol NULL;

    /** the zero value (a default null for type variables with bound Any)
     */
    public final Symbol ZERO;

    /** the scala.Any class
     */
    public final Symbol ANY_CLASS;
    public final Type   ANY_TYPE() {return ANY_CLASS.type();}

    public final Symbol MATCH;
    public final Symbol IS;
    public final Symbol AS;
    public final Symbol EQEQ;
    public final Symbol BANGEQ;
    public final Symbol EQUALS;
    public final Symbol EQ;
    public final Symbol TOSTRING;
    public final Symbol HASHCODE;

    /** the scala.AnyVal class
     */
    public final Symbol ANYVAL_CLASS;
    public final Type   ANYVAL_TYPE() {return ANYVAL_CLASS.type();}

    /** the scala.AnyRef class
     */
    public final Symbol ANYREF_CLASS;
    public final Type   ANYREF_TYPE() {return ANYREF_CLASS.type();}

    /** the scala.AllRef class
     */
    public final Symbol ALLREF_CLASS;
    public final Type   ALLREF_TYPE() {return ALLREF_CLASS.type();}

    /** the scala.All class
     */
    public final Symbol ALL_CLASS;
    public final Type   ALL_TYPE() {return ALL_CLASS.type();}

    /** the java.lang.Object class
     */
    public final Symbol JAVA_OBJECT_CLASS;
    public final Type   JAVA_OBJECT_TYPE() {return JAVA_OBJECT_CLASS.type();}

    /** the java.lang.String class
     */
    public final Symbol JAVA_STRING_CLASS;
    public final Type   JAVA_STRING_TYPE() {return JAVA_STRING_CLASS.type();}

    /** the java.lang.Throwable class
     */
    public final Symbol JAVA_THROWABLE_CLASS;
    public final Type   JAVA_THROWABLE_TYPE() {return JAVA_THROWABLE_CLASS.type();}

    public final Symbol THROW;

    /** the scala.Object class
     */
    public final Symbol OBJECT_CLASS;
    public final Type   OBJECT_TYPE() {return OBJECT_CLASS.type();}

    private Symbol OBJECT_TAG;
    public Symbol OBJECT_TAG() {
        if (OBJECT_TAG == null)
            OBJECT_TAG = loadTerm(OBJECT_CLASS, Names.tag);
        return OBJECT_TAG;
    }

    /** the primitive types
     */
    public final Symbol BYTE_CLASS;
    public final Type   BYTE_TYPE() {return BYTE_CLASS.type();}
    public final Symbol SHORT_CLASS;
    public final Type   SHORT_TYPE() {return SHORT_CLASS.type();}
    public final Symbol CHAR_CLASS;
    public final Type   CHAR_TYPE() {return CHAR_CLASS.type();}
    public final Symbol INT_CLASS;
    public final Type   INT_TYPE() {return INT_CLASS.type();}
    public final Symbol LONG_CLASS;
    public final Type   LONG_TYPE() {return LONG_CLASS.type();}
    public final Symbol FLOAT_CLASS;
    public final Type   FLOAT_TYPE() {return FLOAT_CLASS.type();}
    public final Symbol DOUBLE_CLASS;
    public final Type   DOUBLE_TYPE() {return DOUBLE_CLASS.type();}
    public final Symbol BOOLEAN_CLASS;
    public final Type   BOOLEAN_TYPE() {return BOOLEAN_CLASS.type();}
    public final Symbol UNIT_CLASS;
    public final Type   UNIT_TYPE() {return UNIT_CLASS.type();}

    private Symbol BOOLEAN_OR;
    private Symbol BOOLEAN_AND;
    private Symbol BOOLEAN_NOT;

    public Symbol BOOLEAN_OR() {
        if (BOOLEAN_OR == null)
            BOOLEAN_OR = loadTerm(BOOLEAN_CLASS, Names.BARBAR);
        return BOOLEAN_OR;
    }
    public Symbol BOOLEAN_AND() {
        if (BOOLEAN_AND == null)
            BOOLEAN_AND = loadTerm(BOOLEAN_CLASS, Names.AMPAMP);
        return BOOLEAN_AND;
    }
    public Symbol BOOLEAN_NOT() {
        if (BOOLEAN_NOT == null)
            BOOLEAN_NOT = loadTerm(BOOLEAN_CLASS, Names.BANG);
        return BOOLEAN_NOT;
    }

    /** the scala.String class
     */
    public final Symbol STRING_CLASS;
    public final Type   STRING_TYPE() {return STRING_CLASS.type();}

    /** the scala.TupleX classes
     */
    public final int  TUPLE_count = 10;
    public final Symbol[] TUPLE_CLASS = new Symbol[TUPLE_count];

    public Type tupleType(Type[] args) {
        assert 0 < args.length && args.length < TUPLE_count: args.length;
	return getType(TUPLE_CLASS[args.length], args);
    }

    private final Symbol[][] TUPLE_FIELD = new Symbol[TUPLE_count][];

    public Symbol TUPLE_FIELD(int arity, int index) {
        assert 0 < arity && arity < TUPLE_count: arity;
        assert 0 < index && index <= arity: arity + " - " + index;
        if (TUPLE_FIELD[arity][index - 1] == null)
            TUPLE_FIELD[arity][index - 1] = loadTerm(TUPLE_CLASS[arity],Names.TUPLE_FIELD(index));
        return TUPLE_FIELD[arity][index - 1];
    }

    /** the scala.FunctionX classes
     */
    public final int  FUNCTION_count = 10;
    public final Symbol[] FUNCTION_CLASS = new Symbol[FUNCTION_count];

    public Type functionType(Type[] args, Type result) {
        assert 0 <= args.length && args.length < FUNCTION_count: args.length;
        args = Type.cloneArray(args, 1);
        args[args.length - 1] = result;
	return getType(FUNCTION_CLASS[args.length - 1], args);
    }

    private final Symbol[] FUNCTION_APPLY = new Symbol[FUNCTION_count];

    public Symbol FUNCTION_APPLY(int arity) {
        assert 0 <= arity && arity < FUNCTION_count: arity;
        if (FUNCTION_APPLY[arity] == null)
            FUNCTION_APPLY[arity] = loadTerm(FUNCTION_CLASS[arity],Names.apply);
        return FUNCTION_APPLY[arity];
    }

    /** the scala.PartialFunction class
     */
    public final Symbol PARTIALFUNCTION_CLASS;

    public Type partialFunctionType(Type arg, Type result) {
	return getType(PARTIALFUNCTION_CLASS, new Type[] { arg, result });
    }

    private Symbol PARTIALFUNCTION_ISDEFINEDAT;

    public Symbol PARTIALFUNCTION_ISDEFINEDAT() {
        if (PARTIALFUNCTION_ISDEFINEDAT == null)
            PARTIALFUNCTION_ISDEFINEDAT = loadTerm(PARTIALFUNCTION_CLASS, Names.isDefinedAt);
        return PARTIALFUNCTION_ISDEFINEDAT;
    }

    /** the scala.Ref class
     */
    public final Symbol REF_CLASS;

    public Type refType(Type element) {
        return getType(REF_CLASS, element);
    }

    private Symbol REF_ELEM;

    public Symbol REF_ELEM() {
        if (REF_ELEM == null)
            REF_ELEM = loadTerm(REF_CLASS, Names.elem);
        return REF_ELEM;
    }

    /** the scala.List class
     */
    public final Symbol LIST_CLASS;
    // !!! public final Symbol LIST_NIL_CLASS;
    // !!! public final Symbol LIST_CONS_CLASS;

    public Type listType(Type element) {
        return getType(LIST_CLASS, element);
    }

    private Symbol LIST_ISEMPTY;
    private Symbol LIST_HEAD;
    private Symbol LIST_TAIL;

    public Symbol LIST_ISEMPTY() {
        if (LIST_ISEMPTY == null)
            LIST_ISEMPTY = loadTerm(LIST_CLASS, Names.isEmpty);
        return LIST_ISEMPTY;
    }
    public Symbol LIST_HEAD() {
        if (LIST_HEAD == null)
            LIST_HEAD = loadTerm(LIST_CLASS, Names.head);
        return LIST_HEAD;
    }
    public Symbol LIST_TAIL() {
        if (LIST_TAIL == null)
            LIST_TAIL = loadTerm(LIST_CLASS, Names.tail);
        return LIST_TAIL;
    }

    /** the scala.Iterator class
     */
    public final Symbol ITERATOR_CLASS;

    public Type iteratorType(Type element) {
        return getType(ITERATOR_CLASS, element);
    }

    private Symbol ITERATOR_NEXT;
    private Symbol ITERATOR_HASNEXT;

    public Symbol ITERATOR_NEXT() {
        if (ITERATOR_NEXT == null)
            ITERATOR_NEXT = loadTerm(ITERATOR_CLASS, Names.next);
        return ITERATOR_NEXT;
    }
    public Symbol ITERATOR_HASNEXT() {
        if (ITERATOR_HASNEXT == null)
            ITERATOR_HASNEXT = loadTerm(ITERATOR_CLASS, Names.hasNext);
        return ITERATOR_HASNEXT;
    }

    /** the scala.Iterable class
     */
    public final Symbol ITERABLE_CLASS;

    public Type iterableType(Type element) {
        return getType(ITERABLE_CLASS, element);
    }

    private Symbol ITERABLE_ELEMENTS;

    public Symbol ITERABLE_ELEMENTS() {
        if (ITERABLE_ELEMENTS == null)
            ITERABLE_ELEMENTS = loadTerm(ITERABLE_CLASS, Names.elements);
        return ITERABLE_ELEMENTS;
    }

    /** the scala.Seq class
     */
    public final Symbol SEQ_CLASS;

    public Type seqType(Type element) {
	return getType(SEQ_CLASS, element);
    }

    private Symbol SEQ_LENGTH;

    public Symbol SEQ_LENGTH() {
        if (SEQ_LENGTH == null)
            SEQ_LENGTH = loadTerm(SEQ_CLASS, Names.length);
        return SEQ_LENGTH;
    }

    /** the scala.Array class
     */
    public final Symbol ARRAY_CLASS;

    public Type arrayType(Type element) {
        return getType(ARRAY_CLASS, element);
    }

    /** the scala.MatchError class
     */
    public final Symbol MATCHERROR;
    public final Symbol MATCHERROR_CLASS;

    private Symbol MATCHERROR_FAIL;

    public Symbol MATCHERROR_FAIL() {
        if (MATCHERROR_FAIL == null)
            MATCHERROR_FAIL = loadTerm(MATCHERROR, Names.fail);
        return MATCHERROR_FAIL;
    }

    /** string concatenation pseudo-methods of classes scala.Any and
     *  java.lang.String
     */
    //public final Symbol ANY_PLUS_STRING;
    public final Symbol STRING_PLUS_ANY;

    public final Symbol PATTERN_WILDCARD;

    public Definitions(Global global) {
        // a hack to make definitions accessible earlier to other
        // components
        global.definitions = this;
        PackageParser pparser = new PackageParser(global);

        // this is the root value; all top-level functions,
        // modules etc. are a member of this value
        ROOT = TermSymbol.newJavaPackageModule(
            Names.ROOT, Symbol.NONE, pparser);
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
        ANY_CLASS.setInfo(Type.compoundType(Type.EMPTY_ARRAY, new Scope(), ANY_CLASS));
        ANY_CLASS.primaryConstructor().setInfo(
	    Type.MethodType(Symbol.EMPTY_ARRAY, ANY_CLASS.typeConstructor()));

        // the java.lang.OBJECT class
        JAVA_OBJECT_CLASS = getClass(Names.java_lang_Object);
        JAVA_OBJECT_CLASS.setInfo(pparser.classCompletion); // !!!

        // the primitive types
        DOUBLE_CLASS = getClass(Names.scala_Double);
        FLOAT_CLASS = getClass(Names.scala_Float);
        LONG_CLASS = getClass(Names.scala_Long);
        INT_CLASS = getClass(Names.scala_Int);
        CHAR_CLASS = getClass(Names.scala_Char);
        SHORT_CLASS = getClass(Names.scala_Short);
        BYTE_CLASS = getClass(Names.scala_Byte);
        BOOLEAN_CLASS = getClass(Names.scala_Boolean);
        UNIT_CLASS = getClass(Names.scala_Unit);

        // the scala.ANYREF class
	ANYREF_CLASS = new AliasTypeSymbol(
	    Position.NOPOS, Names.AnyRef.toTypeName(),
	    SCALA_CLASS, Modifiers.JAVA)
	    .setInfo(JAVA_OBJECT_TYPE());
        SCALA_CLASS.members().enter(ANYREF_CLASS);
	ANYREF_CLASS.primaryConstructor().setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, JAVA_OBJECT_TYPE()));

        // the scala.OBJECT class
	OBJECT_CLASS = getClass(Names.scala_Object);

        // the scala.ANYVAL class
	ANYVAL_CLASS = getClass(Names.scala_AnyVal);

        // the scala.ALL class
        ALL_CLASS = new ClassSymbol(
	    Position.NOPOS, Names.All.toTypeName(), SCALA_CLASS, 0);
        SCALA_CLASS.members().enter(ALL_CLASS);
        ALL_CLASS.setInfo(Type.compoundType(new Type[]{ANY_TYPE()}, new Scope(), ALL_CLASS));
        ALL_CLASS.primaryConstructor().setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, ALL_CLASS.typeConstructor()));

        // the scala.ALLREF class
        ALLREF_CLASS = new ClassSymbol(
	    Position.NOPOS, Names.AllRef.toTypeName(), SCALA_CLASS, 0);
        SCALA_CLASS.members().enter(ALLREF_CLASS);
        ALLREF_CLASS.setInfo(Type.compoundType(new Type[]{ANYREF_TYPE()}, new Scope(), ALLREF_CLASS));
        ALLREF_CLASS.primaryConstructor().setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, ALLREF_CLASS.typeConstructor()));

        // the array class
        ARRAY_CLASS = getClass(Names.scala_Array);

        // add members to java.lang.Throwable
        JAVA_THROWABLE_CLASS = getClass(Names.java_lang_Throwable);
        THROW = new TermSymbol(
	    Position.NOPOS, Names.throw_, JAVA_THROWABLE_CLASS, Modifiers.FINAL);
        THROW.setInfo(Type.PolyType(Symbol.EMPTY_ARRAY, ALL_TYPE()));
        JAVA_THROWABLE_CLASS.members().enter(THROW);

        // add the java.lang.String class to the scala package
        JAVA_STRING_CLASS = getClass(Names.java_lang_String);
        STRING_CLASS = new AliasTypeSymbol(
	    Position.NOPOS, Names.String.toTypeName(), SCALA_CLASS, 0)
	    .setInfo(JAVA_STRING_TYPE());
        SCALA_CLASS.members().enter(STRING_CLASS);
	STRING_CLASS.primaryConstructor().setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, STRING_CLASS.typeConstructor()));

        for (int i = 1; i < TUPLE_count; i++) {
            TUPLE_CLASS[i] = getClass(Names.scala_Tuple(i));
            TUPLE_FIELD[i] = new Symbol[i];
        }
        for (int i = 0; i < FUNCTION_count; i++)
            FUNCTION_CLASS[i] = getClass(Names.scala_Function(i));
	PARTIALFUNCTION_CLASS = getClass(Names.scala_PartialFunction);
        REF_CLASS = getClass(Names.scala_Ref);
	LIST_CLASS = getClass(Names.scala_List);
	ITERATOR_CLASS = getClass(Names.scala_Iterator);
	ITERABLE_CLASS = getClass(Names.scala_Iterable);
	SEQ_CLASS = getClass(Names.scala_Seq);
	MATCHERROR = getModule(Names.scala_MatchError);
	MATCHERROR_CLASS = getClass(Names.scala_MatchError);

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
	    Position.NOPOS, Names.PLUS, JAVA_STRING_CLASS, Modifiers.FINAL);
        STRING_PLUS_ANY.setInfo(
	    Type.MethodType(
		new Symbol[]{newParameter(STRING_PLUS_ANY, ANY_TYPE())},
		STRING_TYPE()));
        JAVA_STRING_CLASS.members().enter(STRING_PLUS_ANY);

        // add members to class scala.Any
        MATCH = new TermSymbol(
	    Position.NOPOS, Names.match, ANY_CLASS, Modifiers.FINAL);
	Symbol matchTyParam1 = newTypeParameter(MATCH, ANY_TYPE());
	Symbol matchTyParam2 = newTypeParameter(MATCH, ANY_TYPE());
        MATCH.setInfo(
	    Type.PolyType(
		new Symbol[]{matchTyParam1, matchTyParam2},
		Type.MethodType(
		    new Symbol[]{
			newParameter(
			    MATCH,
			    functionType(
				new Type[]{matchTyParam1.typeConstructor()},
				matchTyParam2.typeConstructor()))},
		    matchTyParam2.typeConstructor())));
        ANY_CLASS.members().enter(MATCH);

        AS = new TermSymbol(
	    Position.NOPOS, Names.asInstanceOf, ANY_CLASS, Modifiers.FINAL);
        Symbol tvar = newTypeParameter(AS, ANY_TYPE());
        AS.setInfo(Type.PolyType(new Symbol[]{tvar}, tvar.type()));
        ANY_CLASS.members().enter(AS);

        IS = new TermSymbol(
	    Position.NOPOS, Names.isInstanceOf, ANY_CLASS, Modifiers.FINAL);
        IS.setInfo(Type.PolyType(new Symbol[]{newTypeParameter(IS, ANY_TYPE())},
				 BOOLEAN_TYPE()));
        ANY_CLASS.members().enter(IS);

        EQEQ = new TermSymbol(
	    Position.NOPOS, Names.EQEQ, ANY_CLASS, Modifiers.FINAL);
        EQEQ.setInfo(Type.MethodType(new Symbol[]{newParameter(EQEQ, ANY_TYPE())},
				     BOOLEAN_TYPE()));
        ANY_CLASS.members().enter(EQEQ);

        BANGEQ = new TermSymbol(
	    Position.NOPOS, Names.BANGEQ, ANY_CLASS, Modifiers.FINAL);
        BANGEQ.setInfo(Type.MethodType(new Symbol[]{newParameter(BANGEQ, ANY_TYPE())},
				       BOOLEAN_TYPE()));
        ANY_CLASS.members().enter(BANGEQ);

        EQUALS = new TermSymbol(
	    Position.NOPOS, Names.equals, ANY_CLASS, 0);
        EQUALS.setInfo(Type.MethodType(new Symbol[]{newParameter(EQUALS, ANY_TYPE())},
				     BOOLEAN_TYPE()));
        ANY_CLASS.members().enter(EQUALS);

	EQ = new TermSymbol(
	    Position.NOPOS, Names.eq, ANY_CLASS, 0);
        EQ.setInfo(Type.MethodType(new Symbol[]{newParameter(EQ, ANY_TYPE())},
				   BOOLEAN_TYPE()));
        ANY_CLASS.members().enter(EQ);

        TOSTRING = new TermSymbol(
	    Position.NOPOS, Names.toString, ANY_CLASS, 0);
        TOSTRING.setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, STRING_TYPE()));
        ANY_CLASS.members().enter(TOSTRING);

        HASHCODE = new TermSymbol(
	    Position.NOPOS, Names.hashCode, ANY_CLASS, 0);
        HASHCODE.setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, INT_TYPE()));
        ANY_CLASS.members().enter(HASHCODE);

        // add a null value to the root scope
	NULL = new TermSymbol(
	    Position.NOPOS, Names.null_, ROOT_CLASS, 0);
        NULL.setInfo(ALLREF_TYPE());
        ROOT.members().enter(NULL);

        // add a null value to the root scope
	ZERO = new TermSymbol(
	    Position.NOPOS, Names.ZERO, ROOT_CLASS, 0);
        ZERO.setInfo(ALL_TYPE());
        ROOT.members().enter(ZERO);

        PATTERN_WILDCARD = new TermSymbol(
            Position.NOPOS, Names.PATTERN_WILDCARD, Symbol.NONE, 0);
        PATTERN_WILDCARD.setType(ALL_TYPE());
    }

    private Symbol newParameter(Symbol owner, Type tp) {
	return new TermSymbol(Position.NOPOS, Name.fromString("v"), owner, Modifiers.PARAM)
	    .setInfo(tp);
    }

    private Symbol newTypeParameter(Symbol owner, Type bound) {
	return new AbsTypeSymbol(
	    Position.NOPOS, Name.fromString("T").toTypeName(), owner, Modifiers.PARAM)
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

    private Symbol loadTerm(Symbol clasz, Name name) {
        Symbol sym = clasz.lookup(name);
        assert sym.isTerm() && !sym.isOverloaded(): clasz+"."+name+" -> "+sym;
        return sym;
    }

    private Type getType(Symbol clasz, Type arg) {
	return getType(clasz, new Type[] { arg });
    }
    private Type getType(Symbol clasz, Type[] args) {
        return Type.appliedType(clasz.typeConstructor(), args);
    }
}
