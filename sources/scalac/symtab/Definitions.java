/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2004, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab;

import scala.tools.util.Position;

import scalac.Global;
import scalac.atree.ATreeTyper;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Names;

public class Definitions {

    //########################################################################
    // Public Fields - Root module

    /** The attributed tree typer */
    public final ATreeTyper atyper;

    //########################################################################
    // Public Fields & Methods - Root class and some standard packages

    /** The root class */
    public final Symbol ROOT_CLASS;

    /** The java package */
    public final Symbol JAVA;

    /** The java.lang package */
    public final Symbol JAVALANG;

    /** The scala package */
    public final Symbol SCALA;

    //########################################################################
    // Public Fields & Methods - Top and bottom classes

    /** The scala.Any class */
    public final Symbol ANY_CLASS;
    public final Type   ANY_TYPE() {return ANY_CLASS.staticType();}

    /** The scala.AnyVal class */
    public final Symbol ANYVAL_CLASS;
    public final Type   ANYVAL_TYPE() {return ANYVAL_CLASS.staticType();}

    /** The scala.AnyRef class */
    public final Symbol ANYREF_CLASS;
    public final Type   ANYREF_TYPE() {return ANYREF_CLASS.staticType();}

    /** The scala.AllRef class */
    public final Symbol ALLREF_CLASS;
    public final Type   ALLREF_TYPE() {return ALLREF_CLASS.staticType();}

    /** The scala.All class */
    public final Symbol ALL_CLASS;
    public final Type   ALL_TYPE() {return ALL_CLASS.staticType();}

    //########################################################################
    // Public Fields & Methods - Java classes

    /** The java.lang.Object class */
    public final Symbol OBJECT_CLASS;
    public final Type   OBJECT_TYPE() {return OBJECT_CLASS.staticType();}

    /** The java.lang.String class */
    public final Symbol STRING_CLASS;
    public final Type   STRING_TYPE() {return STRING_CLASS.staticType();}

    /** The java.lang.Throwable class */
    public final Symbol THROWABLE_CLASS;
    public final Type   THROWABLE_TYPE() {return THROWABLE_CLASS.staticType();}

    /** The java.io.Serializable class */
    public final Symbol SERIALIZABLE_CLASS;
    public final Type   SERIALIZABLE_TYPE() {return SERIALIZABLE_CLASS.staticType();}

    //########################################################################
    // Public Fields & Methods - Scala value classes

    /** The scala.Unit class */
    public final Symbol UNIT_CLASS;
    public final Type   UNIT_TYPE() {return UNIT_CLASS.staticType();}

    /** The scala.Boolean class */
    public final Symbol BOOLEAN_CLASS;
    public final Type   BOOLEAN_TYPE() {return BOOLEAN_CLASS.staticType();}

    /** The scala.Byte class */
    public final Symbol BYTE_CLASS;
    public final Type   BYTE_TYPE() {return BYTE_CLASS.staticType();}

    /** The scala.Short class */
    public final Symbol SHORT_CLASS;
    public final Type   SHORT_TYPE() {return SHORT_CLASS.staticType();}

    /** The scala.Char class */
    public final Symbol CHAR_CLASS;
    public final Type   CHAR_TYPE() {return CHAR_CLASS.staticType();}

    /** The scala.Int class */
    public final Symbol INT_CLASS;
    public final Type   INT_TYPE() {return INT_CLASS.staticType();}

    /** The scala.Long class */
    public final Symbol LONG_CLASS;
    public final Type   LONG_TYPE() {return LONG_CLASS.staticType();}

    /** The scala.Float class */
    public final Symbol FLOAT_CLASS;
    public final Type   FLOAT_TYPE() {return FLOAT_CLASS.staticType();}

    /** The scala.Double class */
    public final Symbol DOUBLE_CLASS;
    public final Type   DOUBLE_TYPE() {return DOUBLE_CLASS.staticType();}


    //########################################################################
    // Public Fields & Methods - Scala reference classes

    /** The scala.ScalaObject class */
    public final Symbol SCALAOBJECT_CLASS;
    public final Type   SCALAOBJECT_TYPE() {return SCALAOBJECT_CLASS.staticType();}

    /** The scala.Attribute class */
    public final Symbol ATTRIBUTE_CLASS;
    public final Type   ATTRIBUTE_TYPE() {return ATTRIBUTE_CLASS.staticType();}

    /** The scala.Ref class */
    public final Symbol REF_CLASS;
    public final Type   REF_TYPE(Type element) {
        return REF_CLASS.staticType(element);
    }

    /** The scala.TupleX classes */
    public final int      TUPLE_COUNT = 10;
    public final Symbol[] TUPLE_CLASS = new Symbol[TUPLE_COUNT];
    public final Type     TUPLE_TYPE(Type[] args) {
        assert 0 < args.length && args.length < TUPLE_COUNT: args.length;
        return TUPLE_CLASS[args.length].staticType(args);
    }

    /** The scala.FunctionX classes */
    public final int      FUNCTION_COUNT = 10;
    public final Symbol[] FUNCTION_CLASS = new Symbol[FUNCTION_COUNT];
    public final Type     FUNCTION_TYPE(Type[] args, Type result) {
        assert 0 <= args.length;
        if (args.length >= FUNCTION_COUNT)
            throw new Type.Error("function has too many arguments; limit = " + (FUNCTION_COUNT-1));
        args = Type.cloneArray(args, 1);
        args[args.length - 1] = result;
        return FUNCTION_CLASS[args.length - 1].staticType(args);
    }

    /** The scala.PartialFunction class */
    public final Symbol PARTIALFUNCTION_CLASS;
    public final Type   PARTIALFUNCTION_TYPE(Type argument, Type result) {
        return PARTIALFUNCTION_CLASS.staticType(argument, result);
    }

    /** The scala.Iterable class */
    public final Symbol ITERABLE_CLASS;
    public final Type   ITERABLE_TYPE(Type element) {
        return ITERABLE_CLASS.staticType(element);
    }

    /** The scala.Iterator class */
    public final Symbol ITERATOR_CLASS;
    public final Type   ITERATOR_TYPE(Type element) {
        return ITERATOR_CLASS.staticType(element);
    }

    /** The scala.Seq class */
    public final Symbol SEQ_CLASS;
    public final Type   SEQ_TYPE(Type element) {
        return SEQ_CLASS.staticType(element);
    }

    /** The scala.List class */
    public final Symbol LIST_CLASS;
    public final Type   LIST_TYPE(Type element) {
        return LIST_CLASS.staticType(element);
    }

    /** The scala.Nil module
     *  evaluated on demand to make bootstrap possible.
     */
    public final Symbol NIL() {
        return getModule("scala.Nil");
    }

    /** The scala.:: class
     *  evaluated on demand to make bootstrap possible.
     */
    public final Symbol CONS_CLASS() {
        return getClass("scala.$colon$colon");
    }
    public final Type   CONS_TYPE(Type element) {
        return CONS_CLASS().staticType(element);
    }

    /** The scala.Array class */
    public final Symbol ARRAY_CLASS;
    public final Type   ARRAY_TYPE(Type element) {
        return ARRAY_CLASS.staticType(element);
    }

    /** The scala.Type class & its subclasses */
    public final Symbol TYPE_CLASS;
    public final Type   TYPE_TYPE() {
        return TYPE_CLASS.staticType();
    }

    public final Symbol CLASSTYPE_CLASS;
    public final Type   CLASSTYPE_TYPE() {
        return CLASSTYPE_CLASS.staticType();
    }

    public final Symbol JAVACLASSTYPE_CLASS;

    public final Symbol SCALACLASSTYPE_CLASS;
    public final Type   SCALACLASSTYPE_TYPE() {
        return SCALACLASSTYPE_CLASS.staticType();
    }

    public final Symbol SINGLETYPE_CLASS;

    public final Symbol TYPECONSTRUCTOR_CLASS;
    public final Type   TYPECONSTRUCTOR_TYPE() {
        return TYPECONSTRUCTOR_CLASS.staticType();
    }

    public final Symbol COMPOUNDTYPE_CLASS;
    public final Symbol METHODTYPE_CLASS;

    public final Symbol REFINEMENT_CLASS;
    public final Type   REFINEMENT_TYPE() {
        return REFINEMENT_CLASS.staticType();
    }

    /** The scala.Predef module */
    public final Symbol PREDEF;

    /** The scala.Console module */
    public final Symbol CONSOLE;

    /** The scala.MatchError module */
    public final Symbol MATCHERROR;

    //########################################################################
    // Public Fields & Methods - Scala primitive types

    /** Returns the primitive type void. */
    public final Type void_TYPE() {
        return void_TYPE.type().resultType();
    }

    /** Returns the primitive type boolean. */
    public final Type boolean_TYPE() {
        return boolean_TYPE.type().resultType();
    }

    /** Returns the primitive type byte. */
    public final Type byte_TYPE() {
        return byte_TYPE.type().resultType();
    }

    /** Returns the primitive type short. */
    public final Type short_TYPE() {
        return short_TYPE.type().resultType();
    }

    /** Returns the primitive type char. */
    public final Type char_TYPE() {
        return char_TYPE.type().resultType();
    }

    /** Returns the primitive type int. */
    public final Type int_TYPE() {
        return int_TYPE.type().resultType();
    }

    /** Returns the primitive type long. */
    public final Type long_TYPE() {
        return long_TYPE.type().resultType();
    }

    /** Returns the primitive type float. */
    public final Type float_TYPE() {
        return float_TYPE.type().resultType();
    }

    /** Returns the primitive type double. */
    public final Type double_TYPE() {
        return double_TYPE.type().resultType();
    }

    /** Returns the primitive array type of given element type. */
    public final Type array_TYPE(Type element) {
        Type type = array_TYPE.type().resultType();
        switch (type) {
        case TypeRef(Type prefix, Symbol clasz, _):
            return Type.typeRef(prefix, clasz, new Type[]{element});
        case UnboxedArrayType(_):
            return Type.UnboxedArrayType(element);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    //########################################################################
    // Public Fields & Methods - Top and bottom class methods

    /** Some scala.Any methods */
    public final Symbol ANY_EQEQ;
    public final Symbol ANY_BANGEQ;
    public final Symbol ANY_EQUALS;
    public final Symbol ANY_HASHCODE;
    public final Symbol ANY_TOSTRING;
    //public final Symbol ANY_PLUS;
    public final Symbol ANY_IS;
    public final Symbol ANY_AS;
    public final Symbol ANY_MATCH;

    //########################################################################
    // Public Fields & Methods - Java class methods

    /** Some java.lang.Object methods */
    public final Symbol OBJECT_EQ;
    public final Symbol OBJECT_SYNCHRONIZED;

    /** Some java.lang.String methods */
    public final Symbol STRING_PLUS;

    /** Some java.lang.Throwable methods */
    public final Symbol THROWABLE_THROW;

    //########################################################################
    // Public Fields & Methods - Scala value class methods

    /** Some scala.Boolean methods */
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

    //########################################################################
    // Public Fields & Methods - Scala reference class methods

    /** Some scala.ScalaObject methods */
    private Symbol SCALAOBJECT_TAG;

    public Symbol SCALAOBJECT_TAG() {
        if (SCALAOBJECT_TAG == null)
            SCALAOBJECT_TAG = loadTerm(SCALAOBJECT_CLASS, Names.tag);
        return SCALAOBJECT_TAG;
    }

    private Symbol SCALAOBJECT_GETTYPE;

    public Symbol SCALAOBJECT_GETTYPE() {
        if (SCALAOBJECT_GETTYPE == null)
            SCALAOBJECT_GETTYPE = loadTerm(SCALAOBJECT_CLASS, Names.getType);
        return SCALAOBJECT_GETTYPE;
    }

    /** Some scala.Ref methods */
    private Symbol REF_ELEM;

    public Symbol REF_ELEM() {
        if (REF_ELEM == null)
            REF_ELEM = loadTerm(REF_CLASS, Names.elem);
        return REF_ELEM;
    }

    /** Some scala.TupleX methods */
    private final Symbol[][] TUPLE_FIELD = new Symbol[TUPLE_COUNT][];

    public Symbol TUPLE_FIELD(int arity, int index) {
        assert 0 < arity && arity < TUPLE_COUNT: arity;
        assert 0 < index && index <= arity: arity + " - " + index;
        if (TUPLE_FIELD[arity][index - 1] == null)
            TUPLE_FIELD[arity][index - 1] = loadTerm(TUPLE_CLASS[arity],Names.TUPLE_FIELD(index));
        return TUPLE_FIELD[arity][index - 1];
    }

    /** Some scala.FunctionX methods */
    private final Symbol[] FUNCTION_APPLY = new Symbol[FUNCTION_COUNT];

    public Symbol FUNCTION_APPLY(int arity) {
        assert 0 <= arity && arity < FUNCTION_COUNT: arity;
        if (FUNCTION_APPLY[arity] == null)
            FUNCTION_APPLY[arity] = loadTerm(FUNCTION_CLASS[arity],Names.apply);
        return FUNCTION_APPLY[arity];
    }

    /** Some scala.PartialFunction methods */
    private Symbol PARTIALFUNCTION_ISDEFINEDAT;

    public Symbol PARTIALFUNCTION_ISDEFINEDAT() {
        if (PARTIALFUNCTION_ISDEFINEDAT == null)
            PARTIALFUNCTION_ISDEFINEDAT = loadTerm(PARTIALFUNCTION_CLASS, Names.isDefinedAt);
        return PARTIALFUNCTION_ISDEFINEDAT;
    }

    /** Some scala.Iterable methods */
    private Symbol ITERABLE_ELEMENTS;

    public Symbol ITERABLE_ELEMENTS() {
        if (ITERABLE_ELEMENTS == null)
            ITERABLE_ELEMENTS = loadTerm(ITERABLE_CLASS, Names.elements);
        return ITERABLE_ELEMENTS;
    }

    /** Some scala.Iterator methods */
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

    /** Some scala.Seq methods */
    private Symbol SEQ_LENGTH;

    public Symbol SEQ_LENGTH() {
        if (SEQ_LENGTH == null)
            SEQ_LENGTH = loadTerm(SEQ_CLASS, Names.length);
        return SEQ_LENGTH;
    }

    /** Some scala.List methods */
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

    /** The scala.Array class */
    private Symbol ARRAY_LENGTH;
    private Symbol ARRAY_GET;
    private Symbol ARRAY_SET;

    public Symbol ARRAY_LENGTH() {
        if (ARRAY_LENGTH == null)
            ARRAY_LENGTH = loadTerm(ARRAY_CLASS, Names.length);
        return ARRAY_LENGTH;
    }

    public Symbol ARRAY_GET() {
        if (ARRAY_GET == null)
            ARRAY_GET = loadTerm(ARRAY_CLASS, Names.apply, new Type[]{int_TYPE()});
        return ARRAY_GET;
    }

    public Symbol ARRAY_SET() {
        if (ARRAY_SET == null)
            ARRAY_SET = loadTerm(ARRAY_CLASS, Names.update);
        return ARRAY_SET;
    }

    /** Some scala.Predef methods */
    private Symbol PREDEF_ARRAY;

    public Symbol PREDEF_ARRAY() {
        if (PREDEF_ARRAY == null)
            PREDEF_ARRAY = loadTerm(PREDEF, Names.Array);
        return PREDEF_ARRAY;
    }

    /** Some scala.Console methods */
    private Symbol CONSOLE_PRINT;

    public Symbol CONSOLE_PRINT() {
        if (CONSOLE_PRINT == null)
            CONSOLE_PRINT = loadTerm(CONSOLE, Names.print);
        return CONSOLE_PRINT;
    }

    /** Some scala.MatchError methods */
    private Symbol MATCHERROR_FAIL;

    public Symbol MATCHERROR_FAIL() {
        if (MATCHERROR_FAIL == null)
            MATCHERROR_FAIL = loadTerm(MATCHERROR, Names.fail);
        return MATCHERROR_FAIL;
    }

    private Symbol MATCHERROR_REPORT;

    public Symbol MATCHERROR_REPORT() {
        if (MATCHERROR_REPORT == null)
            MATCHERROR_REPORT = loadTerm(MATCHERROR, Names.report);
        return MATCHERROR_REPORT;
    }

    /** The scala.Type class (and subclasses) */
    private Symbol TYPE_DEFAULTVALUE;
    public Symbol TYPE_DEFAULTVALUE() {
        if (TYPE_DEFAULTVALUE == null)
            TYPE_DEFAULTVALUE = loadTerm(TYPE_CLASS, Names.defaultValue);
        return TYPE_DEFAULTVALUE;
    }

    private Symbol TYPE_NEWARRAY;
    public Symbol TYPE_NEWARRAY() {
        if (TYPE_NEWARRAY == null)
            TYPE_NEWARRAY = loadTerm(TYPE_CLASS, Names.newArray);
        return TYPE_NEWARRAY;
    }

    private Symbol TYPE_ISINSTANCE;
    public Symbol TYPE_ISINSTANCE() {
        if (TYPE_ISINSTANCE == null)
            TYPE_ISINSTANCE = loadTerm(TYPE_CLASS, Names.isInstance);
        assert TYPE_ISINSTANCE != Symbol.NONE;
        return TYPE_ISINSTANCE;
    }

    private Symbol TYPE_CHECKCASTABILITY;
    public Symbol TYPE_CHECKCASTABILITY() {
        if (TYPE_CHECKCASTABILITY == null)
            TYPE_CHECKCASTABILITY = loadTerm(TYPE_CLASS, Names.checkCastability);
        return TYPE_CHECKCASTABILITY;
    }

    private Symbol RTT_DOUBLE;
    public Symbol RTT_DOUBLE() {
        if (RTT_DOUBLE == null)
            RTT_DOUBLE = loadTerm(TYPE_CLASS.linkedModule().moduleClass(), Names.Double);
        return RTT_DOUBLE;
    }

    private Symbol RTT_FLOAT;
    public Symbol RTT_FLOAT() {
        if (RTT_FLOAT == null)
            RTT_FLOAT = loadTerm(TYPE_CLASS.linkedModule().moduleClass(), Names.Float);
        return RTT_FLOAT;
    }

    private Symbol RTT_LONG;
    public Symbol RTT_LONG() {
        if (RTT_LONG == null)
            RTT_LONG = loadTerm(TYPE_CLASS.linkedModule().moduleClass(), Names.Long);
        return RTT_LONG;
    }

    private Symbol RTT_INT;
    public Symbol RTT_INT() {
        if (RTT_INT == null)
            RTT_INT = loadTerm(TYPE_CLASS.linkedModule().moduleClass(), Names.Int);
        return RTT_INT;
    }

    private Symbol RTT_SHORT;
    public Symbol RTT_SHORT() {
        if (RTT_SHORT == null)
            RTT_SHORT = loadTerm(TYPE_CLASS.linkedModule().moduleClass(), Names.Short);
        return RTT_SHORT;
    }

    private Symbol RTT_CHAR;
    public Symbol RTT_CHAR() {
        if (RTT_CHAR == null)
            RTT_CHAR = loadTerm(TYPE_CLASS.linkedModule().moduleClass(), Names.Char);
        return RTT_CHAR;
    }

    private Symbol RTT_BYTE;
    public Symbol RTT_BYTE() {
        if (RTT_BYTE == null)
            RTT_BYTE = loadTerm(TYPE_CLASS.linkedModule().moduleClass(), Names.Byte);
        return RTT_BYTE;
    }

    private Symbol RTT_BOOLEAN;
    public Symbol RTT_BOOLEAN() {
        if (RTT_BOOLEAN == null)
            RTT_BOOLEAN = loadTerm(TYPE_CLASS.linkedModule().moduleClass(), Names.Boolean);
        return RTT_BOOLEAN;
    }

    private Symbol TYPECONSTRUCTOR_GETINSTANTIATION;
    public Symbol TYPECONSTRUCTOR_GETINSTANTIATION() {
        if (TYPECONSTRUCTOR_GETINSTANTIATION == null) {
            TYPECONSTRUCTOR_GETINSTANTIATION =
                loadTerm(TYPECONSTRUCTOR_CLASS, Names.getInstantiation);
        }
        return TYPECONSTRUCTOR_GETINSTANTIATION;
    }

    private Symbol TYPECONSTRUCTOR_INSTANTIATE;
    public Symbol TYPECONSTRUCTOR_INSTANTIATE() {
        if (TYPECONSTRUCTOR_INSTANTIATE == null)
            TYPECONSTRUCTOR_INSTANTIATE =
                loadTerm(TYPECONSTRUCTOR_CLASS, Names.instantiate);
        return TYPECONSTRUCTOR_INSTANTIATE;
    }

    //########################################################################
    // Public Fields - Global values

    /** The universal pattern */
    public final Symbol PATTERN_WILDCARD;

    //########################################################################
    // Private Fields - Symbol

    private final Symbol void_TYPE;
    private final Symbol boolean_TYPE;
    private final Symbol byte_TYPE;
    private final Symbol short_TYPE;
    private final Symbol char_TYPE;
    private final Symbol int_TYPE;
    private final Symbol long_TYPE;
    private final Symbol float_TYPE;
    private final Symbol double_TYPE;
    private final Symbol array_TYPE;

    //########################################################################
    // Public Constructor

    /** Initializes this instance. */
    public Definitions(Global global) {
        // make definitions accessible earlier to other components
        global.definitions = this;
        // create attributed tree typer
        atyper = new ATreeTyper(global, this);

        // the root class
        ROOT_CLASS = ClassSymbol.newRootClass(global);

        // the java, java.lang and scala packages
        JAVA = getModule("java");
        JAVALANG = getModule("java.lang");
        SCALA = getModule("scala");

        // the top and bottom classes
        Symbol SCALA_CLASS = SCALA.moduleClass();
        ANY_CLASS = newClass(SCALA_CLASS, Names.Any, 0);
        ANYVAL_CLASS = getClass("scala.AnyVal");
        ANYREF_CLASS = newAlias(SCALA_CLASS, Names.AnyRef, 0);
        ALLREF_CLASS = newClass(SCALA_CLASS, Names.AllRef, 0);
        ALL_CLASS = newClass(SCALA_CLASS, Names.All, 0);

        final boolean forMSIL = global.target == Global.TARGET_MSIL;
        // the java classes
        OBJECT_CLASS = getClass(forMSIL ? "System.Object" : "java.lang.Object");
	STRING_CLASS = getClass(forMSIL ? "System.String" : "java.lang.String");
        THROWABLE_CLASS =
            getClass(forMSIL ? "System.Exception" : "java.lang.Throwable");
	SERIALIZABLE_CLASS = getClass("java.io.Serializable");

        // the scala value classes
        UNIT_CLASS = getClass("scala.Unit");
        BOOLEAN_CLASS = getClass("scala.Boolean");
        BYTE_CLASS = getClass("scala.Byte");
        SHORT_CLASS = getClass("scala.Short");
        CHAR_CLASS = getClass("scala.Char");
        INT_CLASS = getClass("scala.Int");
        LONG_CLASS = getClass("scala.Long");
        FLOAT_CLASS = getClass("scala.Float");
        DOUBLE_CLASS = getClass("scala.Double");

        // the scala reference classes
        SCALAOBJECT_CLASS = getClass("scala.ScalaObject");
        ATTRIBUTE_CLASS = getClass("scala.Attribute");
        REF_CLASS = getClass("scala.Ref");
        for (int i = 1; i < TUPLE_COUNT; i++) {
            TUPLE_CLASS[i] = getClass("scala.Tuple" + i);
            TUPLE_FIELD[i] = new Symbol[i];
        }
        for (int i = 0; i < FUNCTION_COUNT; i++)
            FUNCTION_CLASS[i] = getClass("scala.Function" + i);
        PARTIALFUNCTION_CLASS = getClass("scala.PartialFunction");
        ITERABLE_CLASS = getClass("scala.Iterable");
        ITERATOR_CLASS = getClass("scala.Iterator");
        SEQ_CLASS = getClass("scala.Seq");
        LIST_CLASS = getClass("scala.List");
        ARRAY_CLASS = getClass("scala.Array");
        TYPE_CLASS = getClass("scala.Type");

        CLASSTYPE_CLASS = getJVMClass("scala.runtime.types.ClassType");
        JAVACLASSTYPE_CLASS = getJVMClass("scala.runtime.types.JavaClassType");
        SCALACLASSTYPE_CLASS = getJVMClass("scala.runtime.types.ScalaClassType");
        SINGLETYPE_CLASS = getJVMClass("scala.runtime.types.SingleType");
        TYPECONSTRUCTOR_CLASS = getJVMClass("scala.runtime.types.TypeConstructor");
        COMPOUNDTYPE_CLASS = getJVMClass("scala.runtime.types.CompoundType");
        METHODTYPE_CLASS = getJVMClass("scala.runtime.types.MethodType");
        REFINEMENT_CLASS = getJVMClass("scala.runtime.types.Refinement");

        PREDEF = getModule("scala.Predef");
        CONSOLE = getModule("scala.Console");
        MATCHERROR = getModule("scala.MatchError");

        // initialize generated classes and aliases
        initClass(ANY_CLASS, Type.EMPTY_ARRAY);
        initAlias(ANYREF_CLASS, OBJECT_TYPE());
        initClass(ALLREF_CLASS, new Type[]{ANYREF_TYPE()});
        initClass(ALL_CLASS, new Type[]{ANY_TYPE()});

        // create type symbols
        void_TYPE    = newTypeMethod(Names.Unit   ,UNIT_TYPE());
        boolean_TYPE = newTypeMethod(Names.Boolean,BOOLEAN_TYPE());
        byte_TYPE    = newTypeMethod(Names.Byte   ,BYTE_TYPE());
        short_TYPE   = newTypeMethod(Names.Short  ,SHORT_TYPE());
        char_TYPE    = newTypeMethod(Names.Char   ,CHAR_TYPE());
        int_TYPE     = newTypeMethod(Names.Int    ,INT_TYPE());
        long_TYPE    = newTypeMethod(Names.Long   ,LONG_TYPE());
        float_TYPE   = newTypeMethod(Names.Float  ,FLOAT_TYPE());
        double_TYPE  = newTypeMethod(Names.Double ,DOUBLE_TYPE());
        array_TYPE   = newTypeMethod(Names.Array  ,ARRAY_TYPE(ANYREF_TYPE()));

        // add members to scala.Any
        ANY_EQEQ     = newMethod(ANY_CLASS,Names.EQEQ        ,Modifiers.FINAL);
        ANY_BANGEQ   = newMethod(ANY_CLASS,Names.BANGEQ      ,Modifiers.FINAL);
        ANY_EQUALS   = newMethod(ANY_CLASS,Names.equals      ,0);
        ANY_HASHCODE = newMethod(ANY_CLASS,Names.hashCode    ,0);
        ANY_TOSTRING = newMethod(ANY_CLASS,Names.toString    ,0);
        // ANY_PLUS  = newMethod(ANY_CLASS,Names.PLUS        ,Modifiers.FINAL);
        ANY_IS       = newMethod(ANY_CLASS,Names.isInstanceOf,Modifiers.FINAL);
        ANY_AS       = newMethod(ANY_CLASS,Names.asInstanceOf,Modifiers.FINAL);
        ANY_MATCH    = newMethod(ANY_CLASS,Names._match      ,Modifiers.FINAL);

        initMethod(ANY_EQEQ    , new Type[]{ANY_TYPE()}   , boolean_TYPE());
        initMethod(ANY_BANGEQ  , new Type[]{ANY_TYPE()}   , boolean_TYPE());
        initMethod(ANY_EQUALS  , new Type[]{ANY_TYPE()}   , boolean_TYPE());
        initMethod(ANY_HASHCODE, new Type[]{}             , int_TYPE());
        initMethod(ANY_TOSTRING, new Type[]{}             , STRING_TYPE());
        // initMethod(ANY_PLUS , new Type[]{STRING_TYPE()}, STRING_TYPE());

        Symbol[] ANY_IS_TPARAMS = {newTParam(ANY_IS, 0, ANY_TYPE())};
        ANY_IS.setInfo(Type.PolyType(ANY_IS_TPARAMS, boolean_TYPE()));

        Symbol[] ANY_AS_TPARAMS = {newTParam(ANY_AS, 0, ANY_TYPE())};
        ANY_AS.setInfo(Type.PolyType(ANY_AS_TPARAMS,ANY_AS_TPARAMS[0].type()));

        Symbol[] ANY_MATCH_TPARAMS = {
            newTParam(ANY_MATCH, 0, ANY_TYPE()),
            newTParam(ANY_MATCH, 1, ANY_TYPE())};
        Symbol[] ANY_MATCH_VPARAMS = {
            newVParam(ANY_MATCH, 0, FUNCTION_TYPE(
                new Type[]{ANY_MATCH_TPARAMS[0].type()},
                ANY_MATCH_TPARAMS[1].type()))};
        ANY_MATCH.setInfo(
            Type.PolyType(
                ANY_MATCH_TPARAMS,
                Type.MethodType(
                    ANY_MATCH_VPARAMS,
                    ANY_MATCH_TPARAMS[1].type())));

        // add members to java.lang.Object
        OBJECT_EQ =
            newMethod(OBJECT_CLASS, Names.eq, Modifiers.FINAL);
        OBJECT_SYNCHRONIZED =
            newMethod(OBJECT_CLASS, Names.synchronized_, Modifiers.FINAL);

        initMethod(OBJECT_EQ, new Type[]{ANYREF_TYPE()}, boolean_TYPE());
        Symbol OBJECT_SYNCHRONIZED_TPARAM =
            newTParam(OBJECT_SYNCHRONIZED,0,ANY_TYPE());
        Symbol OBJECT_SYNCHRONIZED_VPARAM =
            newVParam(OBJECT_SYNCHRONIZED,0,OBJECT_SYNCHRONIZED_TPARAM.type());
        OBJECT_SYNCHRONIZED.setInfo(
            Type.PolyType(
                new Symbol[] {OBJECT_SYNCHRONIZED_TPARAM},
                Type.MethodType(
                    new Symbol[] {OBJECT_SYNCHRONIZED_VPARAM},
                    OBJECT_SYNCHRONIZED_TPARAM.type())));

	if (forMSIL) {
	    Symbol WAIT0 = newMethod(OBJECT_CLASS, Names.wait, Modifiers.FINAL);
	    initMethod(WAIT0, Type.EMPTY_ARRAY, UNIT_TYPE());

	    Symbol WAIT1 = newMethod(OBJECT_CLASS, Names.wait, Modifiers.FINAL);
	    initMethod(WAIT1, new Type[]{LONG_TYPE()}, UNIT_TYPE());

	    Symbol WAIT2 = newMethod(OBJECT_CLASS, Names.wait, Modifiers.FINAL);
	    initMethod(WAIT2, new Type[]{LONG_TYPE(), INT_TYPE()}, UNIT_TYPE());

	    Symbol NOTIFY =
		newMethod(OBJECT_CLASS, Names.notify, Modifiers.FINAL);
	    initMethod(NOTIFY, Type.EMPTY_ARRAY, UNIT_TYPE());

	    Symbol NOTIFY_ALL =
		newMethod(OBJECT_CLASS, Names.notifyAll, Modifiers.FINAL);
	    initMethod(NOTIFY_ALL, Type.EMPTY_ARRAY, UNIT_TYPE());

	    Symbol JLOA = newAlias(JAVALANG, Names.Object, 0);
	    initAlias(JLOA, OBJECT_TYPE());
	}

        // add members to java.lang.String
        STRING_PLUS = newMethod(STRING_CLASS, Names.PLUS, Modifiers.FINAL);
        initMethod(STRING_PLUS, new Type[]{ANY_TYPE()}, STRING_TYPE());

	if (forMSIL) {
	    Symbol s = newMethod(STRING_CLASS, Name.fromString("length"), 0);
	    initMethod(s, Type.EMPTY_ARRAY, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("compareTo"), 0);
	    initMethod(s, new Type[] {STRING_TYPE()}, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("charAt"), 0);
	    initMethod(s, new Type[] {INT_TYPE()}, CHAR_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("concat"), 0);
	    initMethod(s, new Type[] {STRING_TYPE()}, STRING_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("indexOf"), 0);
	    initMethod(s, new Type[] {INT_TYPE()}, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("indexOf"), 0);
	    initMethod(s, new Type[] {INT_TYPE(), INT_TYPE()}, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("indexOf"), 0);
	    initMethod(s, new Type[] {STRING_TYPE()}, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("indexOf"), 0);
	    initMethod(s, new Type[] {STRING_TYPE(), INT_TYPE()}, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("lastIndexOf"), 0);
	    initMethod(s, new Type[] {INT_TYPE()}, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("lastIndexOf"), 0);
	    initMethod(s, new Type[] {INT_TYPE(), INT_TYPE()}, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("lastIndexOf"), 0);
	    initMethod(s, new Type[] {STRING_TYPE()}, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("lastIndexOf"), 0);
	    initMethod(s, new Type[] {STRING_TYPE(), INT_TYPE()}, INT_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("toLowerCase"), 0);
	    initMethod(s, Type.EMPTY_ARRAY, STRING_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("toUpperCase"), 0);
	    initMethod(s, Type.EMPTY_ARRAY, STRING_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("startsWith"), 0);
	    initMethod(s, new Type[]{STRING_TYPE()}, BOOLEAN_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("endsWith"), 0);
	    initMethod(s, new Type[]{STRING_TYPE()}, BOOLEAN_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("substring"), 0);
	    initMethod(s, new Type[]{INT_TYPE()}, STRING_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("substring"), 0);
	    initMethod(s, new Type[]{INT_TYPE(), INT_TYPE()}, STRING_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("intern"), 0);
	    initMethod(s, Type.EMPTY_ARRAY, STRING_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("replace"), 0);
	    initMethod(s, new Type[]{CHAR_TYPE(), CHAR_TYPE()}, STRING_TYPE());

	    s = newMethod(STRING_CLASS, Name.fromString("toCharArray"), 0);
	    initMethod(s, Type.EMPTY_ARRAY, array_TYPE(CHAR_TYPE()));

	    Symbol JLSA = newAlias(JAVALANG, Names.String, 0);
	    initAlias(JLSA, STRING_TYPE());
	}

        // add members to java.lang.Throwable
        THROWABLE_THROW =
            newMethod(THROWABLE_CLASS, Names.throw_, Modifiers.FINAL);
        THROWABLE_THROW.setInfo(Type.PolyType(Symbol.EMPTY_ARRAY, ALL_TYPE()));

        // create global values
        PATTERN_WILDCARD = Symbol.NONE.newTerm(
            Position.NOPOS, 0, Names.PATTERN_WILDCARD);
        PATTERN_WILDCARD.setInfo(ALL_TYPE());

        // initialize unboxed types in class Type
        Type.initializeUnboxedTypes(this);
    }

    //########################################################################
    // Public Methods

    /** Returns the symbol of the module with the given fullname. */
    public Symbol getModule(String fullname) {
        Scope scope = ROOT_CLASS.members();
        int i = 0;
        int j = fullname.indexOf('.', i);
        while (j >= 0) {
            Name name = Name.fromString(fullname.substring(i, j));
            scope = scope.lookup(name).members();
            i = j + 1;
            j = fullname.indexOf('.', i);
        }
        Name name = Name.fromString(fullname.substring(i, fullname.length()));
        Symbol sym = scope.lookup(name);
        if (!sym.isModule()) {
            switch (sym.type()) {
            case OverloadedType(Symbol[] alts, Type[] alttypes):
                for (int k = 0; k < alts.length; k++)
                    if ((sym = alts[k]).isModule()) break;
            }
        }
        return sym;
    }

    /** Returns the symbol of the class with the given fullname. */
    public Symbol getClass(String fullname) {
        Scope scope = ROOT_CLASS.members();
        int i = 0;
        int j = fullname.indexOf('.', i);
        while (j >= 0) {
            Name name = Name.fromString(fullname.substring(i, j));
            scope = scope.lookup(name).members();
            i = j + 1;
            j = fullname.indexOf('.', i);
        }
        Name name = Name.fromString(fullname.substring(i, fullname.length()));
        Symbol sym = scope.lookup(name.toTypeName());
        assert sym.kind != Kinds.NONE : "no class '" + fullname + "'";
        return sym;
    }

    private Symbol getJVMClass(String fullname) {
        return Global.instance.target == Global.TARGET_MSIL ? null :
            getClass(fullname);
    }

    //########################################################################
    // Private Methods

    /** Creates a new class */
    private Symbol newClass(Symbol owner, Name name, int flags) {
        name = name.toTypeName();
        Symbol clasz = owner.newClass(Position.NOPOS, flags, name);
        owner.members().enter(clasz);
        return clasz;
    }

    /** Creates a new type alias */
    private Symbol newAlias(Symbol owner, Name name, int flags) {
        name = name.toTypeName();
        Symbol alias = owner.newTypeAlias(Position.NOPOS, flags, name);
        owner.members().enter(alias);
        return alias;
    }

    /** Creates a new method */
    private Symbol newMethod(Symbol owner, Name name, int flags) {
        assert owner.isClassType(): Debug.show(owner) + " -- " + name;
        Symbol term = owner.newMethod(Position.NOPOS, flags, name);
        owner.members().enterOrOverload(term);
        return term;
    }

    /** Creates a new type parameter */
    private Symbol newTParam(Symbol owner, int index, Type bound) {
        Name name = Name.fromString("T" + index).toTypeName();
        return owner.newTParam(Position.NOPOS, 0, name, bound);
    }

    /** Creates a new value parameter */
    private Symbol newVParam(Symbol owner, int index, Type type) {
        Name name = Name.fromString("v" + index);
        return owner.newVParam(Position.NOPOS, 0, name, type);
    }

    /** Creates a new type method */
    private Symbol newTypeMethod(Name name, Type type) {
        Symbol symbol = ANY_CLASS.newMethod(Position.NOPOS, 0, name);
        initMethod(symbol, Type.EMPTY_ARRAY, type);
        return symbol;
    }

    /** Initializes the given class */
    private void initClass(Symbol clasz, Type[] parents) {
        clasz.setInfo(Type.compoundType(parents, new Scope(), clasz));
        clasz.primaryConstructor().setInfo(
            Type.MethodType(Symbol.EMPTY_ARRAY, clasz.typeConstructor()));
    }

    /** Initializes the given type alias */
    private void initAlias(Symbol alias, Type aliased) {
        alias.setInfo(aliased);
        alias.primaryConstructor().setInfo(
            Type.MethodType(Symbol.EMPTY_ARRAY, aliased));
    }

    /** Initializes the given method */
    private void initMethod(Symbol method, Type[] vargs, Type result) {
        Symbol[] vparams = new Symbol[vargs.length];
        for (int i = 0; i < vargs.length; i++)
            vparams[i] = newVParam(method, i, vargs[i]);
        method.setInfo(Type.MethodType(vparams, result));
    }

    /** Returns the term member of given class with given name. */
    private Symbol loadTerm(Symbol clasz, Name name) {
        Symbol sym = clasz.lookup(name);
        assert sym.isTerm(): clasz+"."+name+" -> "+sym;
        assert !sym.isOverloaded(): clasz+"."+name+" -> "+sym;
        return sym;
    }

    /**
     * Returns the term member of given class with given name and
     * value argument types.
     */
    private Symbol loadTerm(Symbol clasz, Name name, Type[] vargs) {
        Symbol sym = clasz.lookup(name);
        assert sym.isTerm(): Debug.show(clasz, name, vargs, sym);
        Symbol[] alts = sym.alternativeSymbols();
        for (int i = 0; i < alts.length; i++) {
            switch (alts[i].type()) {
            case PolyType(_, MethodType(Symbol[] vparams, _)):
                if (Type.isSameAs(Symbol.type(vparams), vargs)) return alts[i];
                continue;
            case MethodType(Symbol[] vparams, _):
                if (Type.isSameAs(Symbol.type(vparams), vargs)) return alts[i];
                continue;
            }
        }
        throw Debug.abort(Debug.show(clasz, name, vargs, alts));
    }

    //########################################################################
}
