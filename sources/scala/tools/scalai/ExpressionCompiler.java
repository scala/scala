/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ExpressionCompiler.java,v 1.16 2002/10/04 15:37:10 paltherr Exp $
// $Id$

package scala.tools.scalai;

import java.lang.reflect.Constructor;
import java.util.ArrayList;

import scalac.ast.Tree;
import scalac.atree.AConstant;
import scalac.backend.Primitives;
import scalac.symtab.Definitions;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;
import scalac.util.Name;

public class ExpressionCompiler {

    //########################################################################
    // Private Fields

    private final Definitions definitions;
    private final Primitives primitives;
    private final Constants constants;
    private final ExpressionContext context;

    //########################################################################
    // Public Constructors

    public ExpressionCompiler(Definitions definitions, Primitives primitives, Constants constants, ExpressionContext context, Symbol[] params) {
        this.definitions = definitions;
        this.primitives = primitives;
        this.constants = constants;
        this.context = context;
        for (int i = 0; i < params.length; i++)
            context.insertVariable(params[i], Variable.Argument(i));
    }

    //########################################################################
    // Public Methods

    public CodeContainer compile(Tree tree) {
        Code code = compute(tree);
        return new CodeContainer(
            context.source(), context.owner(), code, context.stackmax());
    }

    public CodeContainer compile(ArrayList items) {
        Code value = Code.Literal(constants.literal());
        CodeBuffer buffer = new CodeBuffer();
        for (int i = 0, m = items.size(); i < m;) {
            if (i > 0) buffer.append(value);
            Object item = items.get(i++);
            value = compute((Tree)item);
        }
        Code code = buffer.code(value);
        return new CodeContainer(
            context.source(), context.owner(), code, context.stackmax());
    }

    //########################################################################
    // Private Methods - declare

    private void declare(Tree tree, CodeBuffer buffer) {
        switch (tree) {

        case Empty:
            return;

        case ValDef(_, _, _, Tree body):
            Symbol symbol = tree.symbol();
            Variable variable = Variable.Local(context.push());
            context.insertVariable(symbol, variable);
            // !!! this should be done in an earlier phase
            Code value = body != Tree.Empty ?
                compute(body) : Code.Literal(constants.zero(symbol.type()));
            buffer.append(Code.Store(Code.Self, variable, value));
            return;

        default:
            buffer.append(compute(tree));
            return;
        }
    }

    //########################################################################
    // Private Methods - compute

    private Code[] compute(Tree[] trees) {
        Code[] codes = new Code[trees.length];
        for (int i = 0; i < codes.length; i++) codes[i] = compute(trees[i]);
        return codes;
    }

    private Code compute(Tree tree) {
        switch (tree) {

        case LabelDef(_, Tree.Ident[] params, Tree body):
            Symbol symbol = tree.symbol();
            Variable[] vars = new Variable[params.length];
            for (int i = 0; i < params.length; i++) {
                vars[i] = context.lookupVariable(params[i].symbol());
                // !!!
                assert
                    vars[i] instanceof Variable.Argument ||
                    vars[i] instanceof Variable.Local :
                    Debug.show(vars[i]);
            }
            context.insertLabel(symbol);
            return Code.Label(symbol, vars, compute(body));

        case Block(Tree[] stats, Tree value):
            if (stats.length == 0) return compute(value);
            CodeBuffer buffer = new CodeBuffer();
            int stacksize = context.stacksize();
            for (int i = 0; i < stats.length; i++) declare(stats[i], buffer);
            Code result = compute(value);
            context.stacksize(stacksize);
            return buffer.code(result);

        case Assign(Tree lhs, Tree rhs):
            return store(lhs, lhs.symbol(), compute(rhs));

        case If(Tree cond, Tree thenp, Tree elsep):
            return Code.If(compute(cond), compute(thenp),
                // !!! can we remove this test ?
                elsep == Tree.Empty ? Code.Literal(constants.literal()) : compute(elsep));

        case Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise):
            return Code.Switch(
                compute(test), tags, compute(bodies), compute(otherwise));

        case New(Tree init):
            switch (context.lookupTemplate(tree.getType().symbol())) {
            case Global(ScalaTemplate template):
                Variable local = Variable.Local(context.push());
                Code create = Code.Create(template);
                Code store = Code.Store(Code.Null, local, create);
                Code load = Code.Load(Code.Null, local);
                Code code = compute(init);
                switch (code) {
                case Invoke(Null, Function fun, Code[] args, int pos):
                    Code initialize = Code.Invoke(load, fun, args, pos);
                    return Code.Block(new Code[] {store, initialize}, load);
                default:
                    throw Debug.abort("illegal case", code);
                }
            default:
                return compute(init);
            }

        case Apply(TypeApply(Tree tfun, Tree[] targs), Tree[] vargs):
            return tapply(tfun, tfun.symbol(), targs, vargs);

        case Apply(Tree vfun, Tree[] vargs):
            return vapply(vfun, vfun.symbol(), vargs);

        case This(_):
            return Code.Self;

        case Literal(AConstant constant):
            switch (constant) {
            case UNIT:
                return Code.Literal(constants.literal());
            case BOOLEAN(boolean value):
                return Code.Literal(new Boolean(value));
            case BYTE(byte value):
                return Code.Literal(new Byte(value));
            case SHORT(short value):
                return Code.Literal(new Short(value));
            case CHAR(char value):
                return Code.Literal(new Character(value));
            case INT(int value):
                return Code.Literal(new Integer(value));
            case LONG(long value):
                return Code.Literal(new Long(value));
            case FLOAT(float value):
                return Code.Literal(new Float(value));
            case DOUBLE(double value):
                return Code.Literal(new Double(value));
            case STRING(String value):
                return Code.Literal(new String(value));
            case NULL:
                return Code.Null;
            default:
                throw Debug.abort("illegal case", constant);
            }

        default:
            return load(tree, tree.symbol());
        }
    }

    private Code object(Tree tree) {
        switch (tree) {

        case Select(Super(_, _), _):
            return Code.Self;

        case Select(Create(_, _), _):
            return Code.Null;

        case Select(Tree expr, _):
            return compute(expr);

        case Ident(_):
            return Code.Null;

        default:
            throw Debug.abort("illegal tree", tree);
        }
    }

    //########################################################################
    // Private Methods - apply

    private Code tapply(Tree target, Symbol symbol, Tree[] targs, Tree[]vargs){
        Code object = object(target);
        if (symbol == definitions.ANY_IS || symbol == definitions.ANY_AS) {
            assert targs.length == 1: Debug.show(targs);
            assert vargs.length == 0 : Debug.show(vargs);
            boolean cast = symbol == definitions.ANY_AS;
            Type type = targs[0].type();
            return Code.IsAs(object, type, context.getClass(type), cast);
        }
        if (symbol == primitives.NEW_OARRAY) {
            assert object == Code.Null: object;
            assert targs.length == 1: Debug.show(targs);
            assert vargs.length == 1 : Debug.show(vargs);
            Type arraytype = Type.UnboxedArrayType(targs[0].type());
            Class component = context.getClass(arraytype).getComponentType();
            return Code.CreateArray(component, compute(vargs[0]));
        }
        throw Debug.abort("unknown method", symbol);
    }

    // !!! only used for the hack in vapply => remove !
    private static final Name plus_N  = Name.fromString("$plus");
    private static final Name minus_N = Name.fromString("$minus");

    private Code vapply(Tree target, Symbol symbol, Tree[] trees) {
        // !!! optimize ?
        Code object = object(target);
        if (symbol == definitions.BOOLEAN_OR()) {
            return Code.Or(object, compute(trees[0]));
        }
        if (symbol == definitions.BOOLEAN_AND()) {
            return Code.And(object, compute(trees[0]));
        }
        if (symbol == definitions.OBJECT_SYNCHRONIZED) {
            return Code.Synchronized(object, compute(trees[0]));
        }
        // !!! System.out.println("!!! method: " + Debug.show(symbol));
        // !!! System.out.println("!!! -owner: " + Debug.show(symbol.owner()));
        Function function = context.lookupFunction(symbol);
        if (trees.length == 0
            && (symbol.name == plus_N || symbol.name == minus_N)
            // !!! the following line does not work. why? (because of erasure?)
            // !!! symbol.owner().isSubClass(definitions.DOUBLE_CLASS))
            && (
                symbol.owner().isSubClass(definitions.INT_CLASS) ||
                symbol.owner().isSubClass(definitions.LONG_CLASS) ||
                symbol.owner().isSubClass(definitions.FLOAT_CLASS) ||
                symbol.owner().isSubClass(definitions.DOUBLE_CLASS)))
        {
            function = symbol.name == plus_N ? Function.Pos : Function.Neg;
        }
        switch (target) {
        case Select(Super(_, _), _):
            Template template = context.lookupTemplate(symbol.owner());
            switch (template) {
            case Global(ScalaTemplate template_):
                function = Function.Global(template_.getMethod(symbol));
                break;

            case JavaClass(Class clasz):
                if (symbol.isInitializer()) break;
                throw Debug.abort("!!! illegal super on java class", symbol);

            default:
                throw Debug.abort("illegal template", template);
            }
        }
        Code[] args = compute(trees);
        return Code.Invoke(object, function, args, target.pos);
    }

    //########################################################################
    // Private Methods - load & store

    private Code load(Tree target, Symbol symbol) {
        return Code.Load(object(target), context.lookupVariable(symbol));
    }

    private Code store(Tree target, Symbol symbol, Code value) {
        return Code.Store(object(target),context.lookupVariable(symbol),value);
    }

    //########################################################################
}
