/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

import scalac.Unit;
import scalac.ast.Tree;
import scalac.ast.Tree.Ident;
import scalac.ast.Tree.Template;
import scalac.symtab.Definitions;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;
import scalac.util.Name;

/** This class translates syntax trees into attributed trees. */
public class ATreeFromSTree {

    //########################################################################
    // Private Fields

    /** The global definitions */
    private final Definitions definitions;

    /** The attributed tree factory */
    private final ATreeFactory make;

    /** A mapping from primitive classes to initialization state */
    private final Map/*<Symbol,Boolean>*/ states;

    /** A mapping from primitive methods to generators */
    private final Map/*<Symbol,Generator>*/ generators;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ATreeFromSTree(Definitions definitions) {
        this.definitions = definitions;
        this.make = new ATreeFactory();
        this.states = new HashMap();
        this.generators = new HashMap();
        Symbol[] classes = {
            definitions.ANY_CLASS,
            definitions.OBJECT_CLASS,
            definitions.STRING_CLASS,
            definitions.THROWABLE_CLASS,
            definitions.ARRAY_CLASS,
            definitions.UNIT_CLASS,
            definitions.BOOLEAN_CLASS,
            definitions.BYTE_CLASS,
            definitions.SHORT_CLASS,
            definitions.CHAR_CLASS,
            definitions.INT_CLASS,
            definitions.LONG_CLASS,
            definitions.FLOAT_CLASS,
            definitions.DOUBLE_CLASS,
        };
        for (int i = 0; i < classes.length; i++)
            states.put(classes[i], Boolean.FALSE);
    }

    //########################################################################
    // Public Methods - Translating units

    /** Translates the unit's body and stores the result in it. */
    public void translate(Unit unit) {
        template(unit.repository = new ARepository(), unit.body);
    }

    //########################################################################
    // Private Methods - Translating templates

    /** Translates the templates and adds them to the repository. */
    private void template(ARepository repository, Tree[] trees) {
        for (int i = 0; i < trees.length; i++) template(repository, trees[i]);
    }

    /** Translates the template and adds it to the repository. */
    private void template(ARepository repository, Tree tree) {
        switch (tree) {

        case Empty:
            return;

        case ClassDef(_, _, _, _, _, Template(_, Tree[] body)):
            AClass clasz = new AClass(tree.symbol());
            // !!! add static field to global modules
            repository.addClass(clasz);
            member(clasz, body);
            return;

        case PackageDef(_, Template(_, Tree[] body)):
            template(repository, body);
            return;

        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    //########################################################################
    // Private Methods - Translating members

    /** Translates the members and adds them to the class. */
    private void member(AClass clasz, Tree[] trees) {
        for (int i = 0; i < trees.length; i++) member(clasz, trees[i]);
    }

    /** Translates the member and adds it to the class. */
    private void member(AClass clasz, Tree tree) {
        switch (tree) {

        case Empty:
            return;

        case ClassDef(_, _, _, _, _, _):
            template(clasz, tree);
            return;

        case ValDef(_, _, _, Tree rhs):
            AField field = new AField(tree.symbol(), false);
            clasz.addField(field);
            return;

        case DefDef(_, _, _, _, _, Tree rhs):
            AMethod method = new AMethod(tree.symbol(), false);
            clasz.addMethod(method);
            if (!method.isAbstract()) method.setCode(expression(rhs));
            return;

        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    //########################################################################
    // Private Methods - Translating statements

     /** Translates the statements. */
    private ACode[] statement(List locals, Tree[] trees) {
        List codes = new ArrayList();
        for (int i = 0; i < trees.length; i++) {
            ACode code = statement(locals, trees[i]);
            if (code != ACode.Void) codes.add(code);
        }
        return (ACode[])codes.toArray(new ACode[codes.size()]);
    }

    /** Translates the statement. */
    private ACode statement(List locals, Tree tree) {
        switch (tree) {

        case Empty:
            return make.Void;

        case ValDef(_, _, _, Tree rhs):
            Symbol symbol = tree.symbol();
            locals.add(symbol);
            ALocation location = ALocation.Local(symbol, false);
            return make.Store(tree, location, expression(rhs));

        default:
            return ACode.Drop(expression(tree), tree.type());
        }
    }

    //########################################################################
    // Private Methods - Translating expressions

    /** Translates the expressions. */
    private ACode[] expression(Tree[] trees) {
        ACode[] codes = new ACode[trees.length];
        for (int i = 0; i < codes.length; i++) codes[i] = expression(trees[i]);
        return codes;
    }

    /** Translates the expression. */
    private ACode expression(Tree tree) {
        switch (tree) {

        case LabelDef(_, Ident[] idents, Tree rhs):
            Symbol[] locals = Tree.symbolOf(idents);
            return make.Label(tree, tree.symbol(), locals, expression(rhs));

        case Block(Tree[] stats, Tree value):
            List locals = new ArrayList();
            ACode[] codes = statement(locals, stats);
            ACode code = expression(value);
            if (locals.size() == 0 && codes.length == 0) return code;
            Symbol[] symbols =
                (Symbol[])locals.toArray(new Symbol[locals.size()]);
            return make.Block(tree, symbols, codes, code);

        case Assign(Tree lhs, Tree rhs):
            return make.Block(tree, Symbol.EMPTY_ARRAY, new ACode[] {
                make.Store(tree, location(lhs), expression(rhs))},
                make.Void);

        case If(Tree cond, Tree thenp, Tree elsep):
            ACode test = expression(cond);
            return make.If(tree, test, expression(thenp), expression(elsep));

        case Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise):
            int[][] tagss = new int[tags.length][];
            for (int i = 0; i < tagss.length; i++)
                tagss[i] = new int[] {tags[i]};
            ACode[] codes = new ACode[bodies.length + 1];
            for (int i = 0; i < bodies.length; i++)
                codes[i] = expression(bodies[i]);
            codes[tags.length] = expression(otherwise);
            return make.Switch(tree, expression(test), tagss, codes);

        case Return(Tree value):
            return make.Return(tree, tree.symbol(), expression(value));

        case Throw(Tree value):
            return make.Throw(tree, expression(value));

        case New(Tree init):
            return expression(init);

        case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs):
            return apply(tree, fun, targs, vargs);
        case Apply(Tree fun, Tree[] vargs):
            return apply(tree, fun, Tree.EMPTY_ARRAY, vargs);

        case Super(_, _):
        case This(_):
            return make.This(tree, tree.symbol());

        case Select(_, _):
        case Ident(_):
            return make.Load(tree, location(tree));

        case Literal(AConstant value):
            return make.Constant(tree, value);

        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    /** Translates the application. */
    private ACode apply(Tree tree, Tree fun, Tree[] targs, Tree[] vargs) {
        Symbol symbol = fun.symbol();
        ACode[] codes = expression(vargs);
        if (symbol.isLabel()) return make.Goto(tree, symbol, codes);
        Type[] types = Tree.typeOf(targs);
        AFunction function = function(fun);
        switch (function) {
        case Method(ACode object, Symbol method, AInvokeStyle style):
            if (!style.isDynamic()) break;
            Symbol clasz = method.owner();
            Object state = states.get(clasz);
            if (state == null) break;
            if (state != Boolean.TRUE) addGeneratorsOf(clasz);
            Object generator = generators.get(method);
            if (generator == null) break;
            return generate((Generator)generator, tree, object, types, vargs);
        }
        return make.Apply(tree, function, types, codes);
    }

    //########################################################################
    // Private Methods - Translating functions

    /** Translates the method. */
    private AFunction function(Tree tree) {
        Symbol symbol = tree.symbol();
        switch (tree) {

        case Select(Tree qualifier, _):
            AInvokeStyle style = invokeStyle(qualifier);
            return AFunction.Method(expression(qualifier), symbol, style);

        case Ident(_):
            AInvokeStyle style = symbol.isInitializer()
                ? AInvokeStyle.New
                : AInvokeStyle.StaticClass;
            return AFunction.Method(make.Void, symbol, style);

        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    /** Returns the InvokeStyle to use for the qualifier. */
    private AInvokeStyle invokeStyle(Tree qualifier) {
        switch (qualifier) {
        case Super(_, _):
            return AInvokeStyle.StaticInstance;
        default:
            return AInvokeStyle.Dynamic;
        }
    }

    //########################################################################
    // Private Methods - Translating locations

    /** Translates the location. */
    private ALocation location(Tree tree) {
        Symbol symbol = tree.symbol();
        switch (tree) {

        case Select(Tree qualifier, _):
            return ALocation.Field(expression(qualifier), symbol, false);

        case Ident(_):
            if (symbol.isModule()) return ALocation.Module(symbol);
            return symbol.owner().isClass()
                ? ALocation.Field(make.Void, symbol, true)
                : ALocation.Local(symbol, symbol.isParameter());

        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    //########################################################################
    // Private Methods - Translating constants

    /** Translates the constant. */
    private AConstant constant(Object value) {
        if (value instanceof Boolean  ) return make.BOOLEAN((Boolean  )value);
        if (value instanceof Byte     ) return make.BYTE   (((Byte    )value));
        if (value instanceof Short    ) return make.SHORT  ((Short    )value);
        if (value instanceof Character) return make.CHAR   ((Character)value);
        if (value instanceof Integer  ) return make.INT    ((Integer  )value);
        if (value instanceof Long     ) return make.LONG   ((Long     )value);
        if (value instanceof Float    ) return make.FLOAT  ((Float    )value);
        if (value instanceof Double   ) return make.DOUBLE ((Double   )value);
        if (value instanceof String   ) return make.STRING ((String   )value);
        throw Debug.abort("illegal constant", value +" -- "+ value.getClass());
    }

    //########################################################################
    // Private Methods - Generating code for primitive methods

    /** Applies generator to given object and arguments. */
    private ACode generate(Generator generator, Tree tree, ACode object,
        Type[] targs, Tree[] vargs)
    {
        switch (generator) {

        case ANYID:
            assert targs.length == 0 && vargs.length == 1: tree;
            return make.EQ(tree, ATypeKind.REF, object, expression(vargs[0]));

        case ANYEQ:
            Symbol lf = newLocal(tree, definitions.ANY_TYPE());
            Symbol rg = newLocal(tree, definitions.ANY_TYPE());
            return make.Block(tree,
                new Symbol[] {lf, rg},
                new ACode[] {
                    store(tree, lf, object),
                    store(tree, rg, expression(vargs[0]))},
                make.If(tree,
                    make.EQ(tree, ATypeKind.REF, load(tree, lf)),
                    make.EQ(tree, ATypeKind.REF, load(tree, rg)),
                    make.Apply(tree,
                        AFunction.Method(
                            load(tree, lf),
                            definitions.ANY_EQUALS,
                            AInvokeStyle.Dynamic),
                        Type.EMPTY_ARRAY,
                        new ACode[] {load(tree, rg)})));

        case ANYNE:
            Symbol lf = newLocal(tree, definitions.ANY_TYPE());
            Symbol rg = newLocal(tree, definitions.ANY_TYPE());
            return make.Block(tree,
                new Symbol[] {lf, rg},
                new ACode[] {
                    store(tree, lf, object),
                    store(tree, rg, expression(vargs[0]))},
                make.If(tree,
                    make.EQ(tree, ATypeKind.REF, load(tree, lf)),
                    make.NE(tree, ATypeKind.REF, load(tree, rg)),
                    make.NOT(tree,
                        ATypeKind.BOOL,
                        make.Apply(tree,
                            AFunction.Method(
                                load(tree, lf),
                                definitions.ANY_EQUALS,
                                AInvokeStyle.Dynamic),
                            Type.EMPTY_ARRAY,
                            new ACode[] {load(tree, rg)}))));

        case ISAS(boolean cast):
            assert targs.length == 1 && vargs.length == 0: tree;
            return make.IsAs(tree, object, targs[0], cast);

        case SYNCHRONIZED:
            assert targs.length == 1 && vargs.length == 1: tree;
            return make.Synchronized(tree, object, expression(vargs[0]));

        case THROW:
            assert targs.length == 0 && vargs.length == 0: tree;
            return make.Throw(tree, object);

        case CONCAT(ATypeKind prefix):
            assert targs.length == 0 && vargs.length == 1: tree;
            ATypeKind suffix = kind(vargs[0].type());
            ACode argument = expression(vargs[0]);
            return make.CONCAT(tree, prefix, suffix, object, argument);

        default:
            throw Debug.abort("unknown case", generator);
        }
    }

    /** Generates a load operation with given variable. */
    private ACode load(Tree tree, Symbol local) {
        assert local.owner().isNone(): Debug.show(local);
        return make.Load(tree, ALocation.Local(local, false));
    }

    /** Generates a store operation with given variable and value. */
    private ACode store(Tree tree, Symbol local, ACode value) {
        assert local.owner().isNone(): Debug.show(local);
        return make.Store(tree, ALocation.Local(local, false), value);
    }

    /** Creates a variable with tree's position and given type. */
    private Symbol newLocal(Tree tree, Type type) {
        Symbol owner = Symbol.NONE; // !!!
        Name name = Name.fromString("local"); // !!!
        return owner.newTerm(tree.pos, 0, name).setType(type);
    }

    /** Returns the type kind of given type. */
    private ATypeKind kind(Type type) {
        switch (type) {
        case SingleType(_, _):
        case ConstantType(_, _):
            return kind(type.singleDeref());
        case TypeRef(_, Symbol clasz, _):
            if (clasz == definitions.BOOLEAN_CLASS) return ATypeKind.BOOL;
            if (clasz == definitions.BYTE_CLASS) return ATypeKind.I1;
            if (clasz == definitions.SHORT_CLASS) return ATypeKind.I2;
            if (clasz == definitions.CHAR_CLASS) return ATypeKind.U2;
            if (clasz == definitions.INT_CLASS) return ATypeKind.I4;
            if (clasz == definitions.LONG_CLASS) return ATypeKind.I8;
            if (clasz == definitions.FLOAT_CLASS) return ATypeKind.R4;
            if (clasz == definitions.DOUBLE_CLASS) return ATypeKind.R8;
            if (clasz == definitions.STRING_CLASS) return ATypeKind.STR;
            return ATypeKind.REF;
        default:
            return ATypeKind.REF;
        }
    }

    //########################################################################
    // Private Methods - Collecting primitive methods

    /** Associates generators to primitive methods of given class. */
    private void addGeneratorsOf(Symbol clasz) {
        if (clasz == definitions.ANY_CLASS) {
            addGenerator(definitions.ANY_EQ, Generator.ANYID);
            addGenerator(definitions.ANY_EQEQ, Generator.ANYEQ);
            addGenerator(definitions.ANY_BANGEQ, Generator.ANYNE);
            addGenerator(definitions.ANY_IS, Generator.ISAS(false));
            addGenerator(definitions.ANY_AS, Generator.ISAS(true));
        }
        if (clasz == definitions.OBJECT_CLASS) {
            addGenerator(definitions.OBJECT_SYNCHRONIZED, Generator.SYNCHRONIZED);
        }
        if (clasz == definitions.STRING_CLASS) {
            addGenerator(definitions.STRING_PLUS, Generator.CONCAT(ATypeKind.STR));
        }
        if (clasz == definitions.THROWABLE_CLASS) {
            addGenerator(definitions.THROWABLE_THROW, Generator.THROW);
        }
        if (clasz == definitions.ARRAY_CLASS) {
            // !!! addAll(defs.ARRAY_CLASS, Names.length, Primitive.LENGTH, 1);
            // !!! addAll(defs.ARRAY_CLASS, Names.apply, Primitive.APPLY, 2);
            // !!! addAll(defs.ARRAY_CLASS, Names.update, Primitive.UPDATE, 1);
        }
        if (clasz == definitions.UNIT_CLASS) {
            // !!!
        }
        if (clasz == definitions.BOOLEAN_CLASS) {
            // !!!
        }
        if (clasz == definitions.BYTE_CLASS) {
            // !!!
        }
        if (clasz == definitions.SHORT_CLASS) {
            // !!!
        }
        if (clasz == definitions.CHAR_CLASS) {
            // !!!
        }
        if (clasz == definitions.INT_CLASS) {
            // !!!
        }
        if (clasz == definitions.LONG_CLASS) {
            // !!!
        }
        if (clasz == definitions.FLOAT_CLASS) {
            // !!!
        }
        if (clasz == definitions.DOUBLE_CLASS) {
            // !!!
        }
        states.put(clasz, Boolean.TRUE);
    }

    /** Associates given generator to given primitive method. */
    private void addGenerator(Symbol method, Generator generator) {
        generators.put(method, generator);
    }

    //########################################################################
    // Private Class - Code generators

    /** Code generators for primitive methods. */
    private static class Generator {
        case ANYID;
        case ANYEQ;
        case ANYNE;
        case ISAS(boolean cast);
        case SYNCHRONIZED;
        case THROW;
        case CONCAT(ATypeKind prefix);
    }

    //########################################################################
}
