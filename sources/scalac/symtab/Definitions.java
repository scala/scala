/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab;

import ch.epfl.lamp.util.Position;

import scalac.Global;
import scalac.atree.ATreeTyper;
import scalac.symtab.classfile.PackageParser;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Names;

public class Definitions {

    //########################################################################
    // Public Fields - Root module

    /** The attributed tree typer */
    public final ATreeTyper atyper;

    //########################################################################
    // Public Fields & Methods - Root module

    /** The root module */
    public final Symbol ROOT_CLASS;
    public final Type   ROOT_TYPE() {return ROOT_CLASS.type();}

    //########################################################################
    // Public Fields & Methods - Top and bottom classes

    /** The scala.Any class */
    public final Symbol ANY_CLASS;
    public final Type   ANY_TYPE() {return ANY_CLASS.type();}

    /** The scala.AnyVal class */
    public final Symbol ANYVAL_CLASS;
    public final Type   ANYVAL_TYPE() {return ANYVAL_CLASS.type();}

    /** The scala.AnyRef class */
    public final Symbol ANYREF_CLASS;
    public final Type   ANYREF_TYPE() {return ANYREF_CLASS.type();}

    /** The scala.AllRef class */
    public final Symbol ALLREF_CLASS;
    public final Type   ALLREF_TYPE() {return ALLREF_CLASS.type();}

    /** The scala.All class */
    public final Symbol ALL_CLASS;
    public final Type   ALL_TYPE() {return ALL_CLASS.type();}

    //########################################################################
    // Public Fields & Methods - Java classes

    /** The java.lang.Object class */
    public final Symbol JAVA_OBJECT_CLASS;
    public final Type   JAVA_OBJECT_TYPE() {return JAVA_OBJECT_CLASS.type();}

    /** The java.lang.String class */
    public final Symbol JAVA_STRING_CLASS;
    public final Type   JAVA_STRING_TYPE() {return JAVA_STRING_CLASS.type();}

    /** The java.lang.Throwable class */
    public final Symbol JAVA_THROWABLE_CLASS;
    public final Type   JAVA_THROWABLE_TYPE() {return JAVA_THROWABLE_CLASS.type();}

    //########################################################################
    // Public Fields & Methods - Scala value classes

    /** The scala.Unit class */
    public final Symbol UNIT_CLASS;
    public final Type   UNIT_TYPE() {
        return UNIT_TYPE.type().resultType();
    }

    /** The scala.Boolean class */
    public final Symbol BOOLEAN_CLASS;
    public final Type   BOOLEAN_TYPE() {
        return BOOLEAN_TYPE.type().resultType();
    }

    /** The scala.Byte class */
    public final Symbol BYTE_CLASS;
    public final Type   BYTE_TYPE() {
        return BYTE_TYPE.type().resultType();
    }

    /** The scala.Short class */
    public final Symbol SHORT_CLASS;
    public final Type   SHORT_TYPE() {
        return SHORT_TYPE.type().resultType();
    }

    /** The scala.Char class */
    public final Symbol CHAR_CLASS;
    public final Type   CHAR_TYPE() {
        return CHAR_TYPE.type().resultType();
    }

    /** The scala.Int class */
    public final Symbol INT_CLASS;
    public final Type   INT_TYPE() {
        return INT_TYPE.type().resultType();
    }

    /** The scala.Long class */
    public final Symbol LONG_CLASS;
    public final Type   LONG_TYPE() {
        return LONG_TYPE.type().resultType();
    }

    /** The scala.Float class */
    public final Symbol FLOAT_CLASS;
    public final Type   FLOAT_TYPE() {
        return FLOAT_TYPE.type().resultType();
    }

    /** The scala.Double class */
    public final Symbol DOUBLE_CLASS;
    public final Type   DOUBLE_TYPE() {
        return DOUBLE_TYPE.type().resultType();
    }

    //########################################################################
    // Public Fields & Methods - Scala reference classes

    /** The scala.Object class */
    public final Symbol OBJECT_CLASS;
    public final Type   OBJECT_TYPE() {return OBJECT_CLASS.type();}

    /** The scala.String class */
    public final Symbol STRING_CLASS;
    public final Type   STRING_TYPE() {return STRING_CLASS.type();}

    /** The scala.Ref class */
    public final Symbol REF_CLASS;
    public final Type   REF_TYPE(Type element) {
        return getType(REF_CLASS, element);
    }

    /** The scala.TupleX classes */
    public final int      TUPLE_COUNT = 10;
    public final Symbol[] TUPLE_CLASS = new Symbol[TUPLE_COUNT];
    public final Type     TUPLE_TYPE(Type[] args) {
        assert 0 < args.length && args.length < TUPLE_COUNT: args.length;
        return getType(TUPLE_CLASS[args.length], args);
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
        return getType(FUNCTION_CLASS[args.length - 1], args);
    }

    /** The scala.PartialFunction class */
    public final Symbol PARTIALFUNCTION_CLASS;
    public final Type   PARTIALFUNCTION_TYPE(Type argument, Type result) {
        return getType(PARTIALFUNCTION_CLASS, new Type[] { argument, result });
    }

    /** The scala.Iterable class */
    public final Symbol ITERABLE_CLASS;
    public final Type   ITERABLE_TYPE(Type element) {
        return getType(ITERABLE_CLASS, element);
    }

    /** The scala.Iterator class */
    public final Symbol ITERATOR_CLASS;
    public final Type   ITERATOR_TYPE(Type element) {
        return getType(ITERATOR_CLASS, element);
    }

    /** The scala.Seq class */
    public final Symbol SEQ_CLASS;
    public final Type   SEQ_TYPE(Type element) {
        return getType(SEQ_CLASS, element);
    }

    /** The scala.List class */
    public final Symbol LIST_CLASS;
    public final Type   LIST_TYPE(Type element) {
        return getType(LIST_CLASS, element);
    }

    /** The scala.Nil module */
    public final Symbol NIL;

    /** The scala.:: class */
    public final Symbol CONS_CLASS;
    public final Type   CONS_TYPE(Type element) {
        return getType(CONS_CLASS, element);
    }

    /** The scala.Array class */
    public final Symbol ARRAY_CLASS;
    public final Type   ARRAY_TYPE(Type element) {
        Type type = ARRAY_TYPE.type().resultType();
        switch (type) {
        case TypeRef(Type prefix, Symbol clasz, _):
            return Type.typeRef(prefix, clasz, new Type[]{element});
        case UnboxedArrayType(_):
            return Type.UnboxedArrayType(element);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** The scala.Predef module */
    public final Symbol PREDEF;

    /** The scala.Console module */
    public final Symbol CONSOLE;

    /** The scala.MatchError module */
    public final Symbol MATCHERROR;

    //########################################################################
    // Public Fields & Methods - Top and bottom class methods

    /** Some scala.Any methods */
    public final Symbol ANY_EQ;
    public final Symbol ANY_EQEQ;
    public final Symbol ANY_BANGEQ;
    public final Symbol ANY_EQUALS;
    public final Symbol ANY_HASHCODE;
    public final Symbol ANY_TOSTRING;
    //public final Symbol ANY_PLUS;
    public final Symbol ANY_IS;
    public final Symbol ANY_AS;
    public final Symbol ANY_MATCH;

    /** Some scala.AnyRef methods */
    public final Symbol ANYREF_SYNCHRONIZED;

    //########################################################################
    // Public Fields & Methods - Java class methods

    /** Some java.lang.String methods */
    public final Symbol JAVA_STRING_PLUS;

    /** Some java.lang.Throwable methods */
    public final Symbol JAVA_THROWABLE_THROW;

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

    /** Some scala.Object methods */
    private Symbol OBJECT_TAG;

    public Symbol OBJECT_TAG() {
        if (OBJECT_TAG == null)
            OBJECT_TAG = loadTerm(OBJECT_CLASS, Names.tag);
        return OBJECT_TAG;
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
            ARRAY_GET = loadTerm(ARRAY_CLASS, Names.apply, new Type[]{INT_TYPE()});
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

    //########################################################################
    // Public Fields - Global values

    /** The universal pattern */
    public final Symbol PATTERN_WILDCARD;

    //########################################################################
    // Private Fields - Symbol

    private final Symbol UNIT_TYPE;
    private final Symbol BOOLEAN_TYPE;
    private final Symbol BYTE_TYPE;
    private final Symbol SHORT_TYPE;
    private final Symbol CHAR_TYPE;
    private final Symbol INT_TYPE;
    private final Symbol LONG_TYPE;
    private final Symbol FLOAT_TYPE;
    private final Symbol DOUBLE_TYPE;
    private final Symbol ARRAY_TYPE;

    //########################################################################
    // Public Constructor

    /** Initializes this instance. */
    public Definitions(Global global) {
        // make definitions accessible earlier to other components
        global.definitions = this;
        // force initialization of class Type
        Type.localThisType.symbol();
        // create attributed tree typer
        atyper = new ATreeTyper(global, this);

        // the root package
        ROOT_CLASS = ClassSymbol.newRootClass(new PackageParser(global));

        // the scala package
        Symbol SCALA_PACKAGE = getClass(Names.scala);

        // the top and bottom classes
        ANY_CLASS = newClass(SCALA_PACKAGE, Names.Any, 0);
        ANYVAL_CLASS = getClass(Names.scala_AnyVal);
        ANYREF_CLASS = newAlias(SCALA_PACKAGE, Names.AnyRef, 0);
        ALLREF_CLASS = newClass(SCALA_PACKAGE, Names.AllRef, 0);
        ALL_CLASS = newClass(SCALA_PACKAGE, Names.All, 0);

        // the java classes
        JAVA_OBJECT_CLASS = getClass(Names.java_lang_Object);
        JAVA_THROWABLE_CLASS = getClass(Names.java_lang_Throwable);
        JAVA_STRING_CLASS = getClass(Names.java_lang_String);

        // the scala value classes
        UNIT_CLASS = getClass(Names.scala_Unit);
        BOOLEAN_CLASS = getClass(Names.scala_Boolean);
        BYTE_CLASS = getClass(Names.scala_Byte);
        SHORT_CLASS = getClass(Names.scala_Short);
        CHAR_CLASS = getClass(Names.scala_Char);
        INT_CLASS = getClass(Names.scala_Int);
        LONG_CLASS = getClass(Names.scala_Long);
        FLOAT_CLASS = getClass(Names.scala_Float);
        DOUBLE_CLASS = getClass(Names.scala_Double);

        // the scala reference classes
        OBJECT_CLASS = getClass(Names.scala_ScalaObject);
        STRING_CLASS = newAlias(SCALA_PACKAGE, Names.String, 0);
        REF_CLASS = getClass(Names.scala_Ref);
        for (int i = 1; i < TUPLE_COUNT; i++) {
            TUPLE_CLASS[i] = getClass(Names.scala_Tuple(i));
            TUPLE_FIELD[i] = new Symbol[i];
        }
        for (int i = 0; i < FUNCTION_COUNT; i++)
            FUNCTION_CLASS[i] = getClass(Names.scala_Function(i));
        PARTIALFUNCTION_CLASS = getClass(Names.scala_PartialFunction);
        ITERABLE_CLASS = getClass(Names.scala_Iterable);
        ITERATOR_CLASS = getClass(Names.scala_Iterator);
        SEQ_CLASS = getClass(Names.scala_Seq);
        LIST_CLASS = getClass(Names.scala_List);
        NIL = getModule(Names.scala_Nil);
        CONS_CLASS = getClass(Names.scala_COLONCOLON);
        ARRAY_CLASS = getClass(Names.scala_Array);
        PREDEF = getModule(Names.scala_Predef);
        CONSOLE = getModule(Names.scala_Console);
        MATCHERROR = getModule(Names.scala_MatchError);

        // initialize generated classes and aliases
        initClass(ANY_CLASS, Type.EMPTY_ARRAY);
        initAlias(ANYREF_CLASS, JAVA_OBJECT_TYPE());
        initClass(ALLREF_CLASS, new Type[]{ANYREF_TYPE()});
        initClass(ALL_CLASS, new Type[]{ANY_TYPE()});

        // create type symbols
        UNIT_TYPE    = newTypeSymbol(Names.Unit   , UNIT_CLASS.type   ());
        BOOLEAN_TYPE = newTypeSymbol(Names.Boolean, BOOLEAN_CLASS.type());
        BYTE_TYPE    = newTypeSymbol(Names.Byte   , BYTE_CLASS.type   ());
        SHORT_TYPE   = newTypeSymbol(Names.Short  , SHORT_CLASS.type  ());
        CHAR_TYPE    = newTypeSymbol(Names.Char   , CHAR_CLASS.type   ());
        INT_TYPE     = newTypeSymbol(Names.Int    , INT_CLASS.type    ());
        LONG_TYPE    = newTypeSymbol(Names.Long   , LONG_CLASS.type   ());
        FLOAT_TYPE   = newTypeSymbol(Names.Float  , FLOAT_CLASS.type  ());
        DOUBLE_TYPE  = newTypeSymbol(Names.Double , DOUBLE_CLASS.type ());
        ARRAY_TYPE   = newTypeSymbol(Names.Array  ,
            Type.appliedType(ARRAY_CLASS.type(), new Type[]{ANYREF_TYPE()}));

        // initialize generated classes and aliases
        initAlias(STRING_CLASS, JAVA_STRING_TYPE());

        // add members to scala.Any
        ANY_EQ       = newTerm(ANY_CLASS, Names.eq          , 0);
        ANY_EQEQ     = newTerm(ANY_CLASS, Names.EQEQ        , Modifiers.FINAL);
        ANY_BANGEQ   = newTerm(ANY_CLASS, Names.BANGEQ      , Modifiers.FINAL);
        ANY_EQUALS   = newTerm(ANY_CLASS, Names.equals      , 0);
        ANY_HASHCODE = newTerm(ANY_CLASS, Names.hashCode    , 0);
        ANY_TOSTRING = newTerm(ANY_CLASS, Names.toString    , 0);
        // ANY_PLUS  = newTerm(ANY_CLASS, Names.PLUS        , Modifiers.FINAL);
        ANY_IS       = newTerm(ANY_CLASS, Names.isInstanceOf, Modifiers.FINAL);
        ANY_AS       = newTerm(ANY_CLASS, Names.asInstanceOf, Modifiers.FINAL);
        ANY_MATCH    = newTerm(ANY_CLASS, Names.match       , Modifiers.FINAL);

        initMethod(ANY_EQ      , new Type[]{ANY_TYPE()}   , BOOLEAN_TYPE());
        initMethod(ANY_EQEQ    , new Type[]{ANY_TYPE()}   , BOOLEAN_TYPE());
        initMethod(ANY_BANGEQ  , new Type[]{ANY_TYPE()}   , BOOLEAN_TYPE());
        initMethod(ANY_EQUALS  , new Type[]{ANY_TYPE()}   , BOOLEAN_TYPE());
        initMethod(ANY_HASHCODE, new Type[]{}             , INT_TYPE());
        initMethod(ANY_TOSTRING, new Type[]{}             , STRING_TYPE());
        // initMethod(ANY_PLUS , new Type[]{STRING_TYPE()}, STRING_TYPE());

        Symbol[] ANY_IS_TPARAMS = {newTParam(ANY_IS, 0, ANY_TYPE())};
        ANY_IS.setInfo(Type.PolyType(ANY_IS_TPARAMS, BOOLEAN_TYPE()));

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

        // add members to scala.AnyREF
        ANYREF_SYNCHRONIZED =
            newTerm(ANYREF_CLASS, Names.synchronized_, Modifiers.FINAL);

        Symbol ANYREF_SYNCHRONIZED_TPARAM =
            newTParam(ANYREF_SYNCHRONIZED,0,ANY_TYPE());
        Symbol ANYREF_SYNCHRONIZED_VPARAM =
            newVParam(ANYREF_SYNCHRONIZED,0,ANYREF_SYNCHRONIZED_TPARAM.type());
        ANYREF_SYNCHRONIZED.setInfo(
            Type.PolyType(
                new Symbol[] {ANYREF_SYNCHRONIZED_TPARAM},
                Type.MethodType(
                    new Symbol[] {ANYREF_SYNCHRONIZED_VPARAM},
                    ANYREF_SYNCHRONIZED_TPARAM.type())));

        // add members to java.lang.String
        JAVA_STRING_PLUS =
            newTerm(JAVA_STRING_CLASS, Names.PLUS, Modifiers.FINAL);
        initMethod(JAVA_STRING_PLUS, new Type[]{ANY_TYPE()}, STRING_TYPE());

        // add members to java.lang.Throwable
        JAVA_THROWABLE_THROW =
            newTerm(JAVA_THROWABLE_CLASS, Names.throw_, Modifiers.FINAL);
        JAVA_THROWABLE_THROW.setInfo(
            Type.PolyType(Symbol.EMPTY_ARRAY, ALL_TYPE()));

        // create global values
        PATTERN_WILDCARD = new TermSymbol(
            Position.NOPOS, Names.PATTERN_WILDCARD, Symbol.NONE, 0);
        PATTERN_WILDCARD.setInfo(ALL_TYPE());

        // initialize unboxed types in class Type
        Type.initializeUnboxedTypes(this);
    }

    //########################################################################
    // Public Methods

    /** Returns the symbol of the module with the given fullname. */
    public Symbol getModule(Name fullname, boolean fail) {
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
        if (fail)
        	assert sym.isModule() : "no module '" + fullname + "'";
        return sym;
    }

    /** Returns the symbol of the module with the given fullname. */
    public Symbol getModule(Name fullname) {
        return getModule(fullname, true);
    }

    /** Returns the symbol of the class with the given fullname. */
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

    /** Returns the type of the class with the given fullname. */
    public Type getType(Name fullname) {
        return getClass(fullname).type();
    }

    //########################################################################
    // Private Methods

    /** Creates a new class */
    private Symbol newClass(Symbol owner, Name name, int flags) {
        name = name.toTypeName();
        Symbol clasz = new ClassSymbol(Position.NOPOS, name, owner, flags);
        owner.members().enter(clasz);
        return clasz;
    }

    /** Creates a new type alias */
    private Symbol newAlias(Symbol owner, Name name, int flags) {
        name = name.toTypeName();
        Symbol alias = new AliasTypeSymbol(Position.NOPOS, name, owner, flags);
        owner.members().enter(alias);
        return alias;
    }

    /** Creates a new term */
    private Symbol newTerm(Symbol owner, Name name, int flags) {
        if (owner.isTypeAlias()) owner = owner.type().unalias().symbol();
        assert owner.isClassType(): Debug.show(owner) + " -- " + name;
        Symbol term = new TermSymbol(Position.NOPOS, name, owner, flags);
        owner.members().enterOrOverload(term);
        return term;
    }

    /** Creates a new type parameter */
    private Symbol newTParam(Symbol owner, int index, Type bound) {
        Name name = Name.fromString("T" + index).toTypeName();
        return new AbsTypeSymbol(Position.NOPOS, name, owner, Modifiers.PARAM)
            .setInfo(bound);
    }

    /** Creates a new value parameter */
    private Symbol newVParam(Symbol owner, int index, Type type) {
        Name name = Name.fromString("v" + index);
        return new TermSymbol(Position.NOPOS, name, owner, Modifiers.PARAM)
            .setInfo(type);
    }

    /** Creates a new type symbol */
    private Symbol newTypeSymbol(Name name, Type type) {
        Symbol symbol = new TermSymbol(Position.NOPOS, name, ROOT_CLASS, 0);
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
        assert sym.isTerm() && !sym.isOverloaded(): clasz+"."+name+" -> "+sym;
        return sym;
    }

    /**
     * Returns the term member of given class with given name and
     * value argument types.
     */
    private Symbol loadTerm(Symbol clasz, Name name, Type[] vargs) {
        Symbol sym = clasz.lookup(name);
        assert sym.isTerm(): Debug.show(clasz,"."+name+" - ",vargs," -> ",sym);
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
        throw Debug.abort(Debug.show(clasz,"."+name+" - ",vargs," -> ",alts));
    }


    /** Returns the type of given class applied to given type argument. */
    private Type getType(Symbol clasz, Type arg) {
        return getType(clasz, new Type[] { arg });
    }

    /** Returns the type of given class applied to given type arguments. */
    private Type getType(Symbol clasz, Type[] args) {
        return Type.appliedType(clasz.type(), args);
    }

    //########################################################################
}
