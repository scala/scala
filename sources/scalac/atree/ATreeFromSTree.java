/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import java.util.List;
import java.util.ArrayList;

import scalac.Unit;
import scalac.ast.Tree;
import scalac.ast.Tree.Ident;
import scalac.ast.Tree.Template;
import scalac.symtab.Definitions;
import scalac.symtab.Symbol;
import scalac.util.Debug;

/** This class translates syntax trees into attributed trees. */
public class ATreeFromSTree {

    //########################################################################
    // Private Fields

    /** The global definitions */
    private final Definitions definitions;

    /** The attributed tree factory */
    private final ATreeFactory make;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ATreeFromSTree(Definitions definitions) {
        this.definitions = definitions;
        this.make = new ATreeFactory();
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
            repository.addClass(clasz);
            member(clasz, body);
            return;

        case PackageDef(_, Template(_, Tree[] body)):
            template(repository, body);
            return;

        case ValDef(_, _, _, Tree rhs):
            // !!!
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
    private ACode[] statement(List locals, Tree[] trees, int start, int count){
        List codes = new ArrayList();
        for (int i = start; i < start + count; i++) {
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

        case Block(Tree[] statements):
            if (statements.length == 0) return make.Void;
            int statement_count = statements.length - 1;
            List locals = new ArrayList();
            ACode[] codes = statement(locals,statements,0, statement_count);
            ACode value = expression(statements[statement_count]);
            if (locals.size() == 0 && codes.length == 0) return value;
            Symbol[] symbols =
                (Symbol[])locals.toArray(new Symbol[locals.size()]);
            return make.Block(tree, symbols, codes, value);

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

        case New(Template(Tree[] bases, _)):
            switch (bases[0]) {
            case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs):
                return apply(tree, method(fun), targs, vargs);
            case Apply(Tree fun, Tree[] vargs):
                return apply(tree, method(fun), Tree.EMPTY_ARRAY, vargs);
            default:
                throw Debug.abort("illegal case", bases[0]);
            }

        case Apply(TypeApply(Tree fun, Tree[] targs), Tree[] vargs):
            return apply(tree, fun, targs, vargs);
        case Apply(Tree fun, Tree[] vargs):
            return apply(tree, fun, Tree.EMPTY_ARRAY, vargs);

        case Super(_, _):
        case This(_):
            return make.This(tree, tree.symbol());

        case Select(_, _):
            return make.Load(tree, location(tree));

        case Ident(_):
            if (tree.symbol() == definitions.NULL)
                return make.Constant(tree, make.NULL);
            if (tree.symbol() == definitions.ZERO)
                return make.Constant(tree, make.ZERO);
            return make.Load(tree, location(tree));

        case Literal(AConstant value):
            return make.Constant(tree, value);

        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    /** Translates the application. */
    private ACode apply(Tree tree, Tree fun, Tree[] targs, Tree[] vargs) {
        switch (fun) {
        case Ident(_):
            return make.Goto(tree, fun.symbol(), expression(vargs));
        default:
            return apply(tree, method(fun), targs, vargs);
        }
    }

    /** Translates the application. */
    private ACode apply(Tree tree, AFunction function,Tree[]targs,Tree[]vargs){
        return make.Apply(tree, function,Tree.typeOf(targs),expression(vargs));
    }

    //########################################################################
    // Private Methods - Translating functions

    /** Translates the method. */
    private AFunction method(Tree tree) {
        Symbol symbol = tree.symbol();
        switch (tree) {

        case Select(Tree qualifier, _):
            if (symbol.isJava() && symbol.owner().isModuleClass())
                return AFunction.Method(make.Void, symbol, AInvokeStyle.StaticClass); // !!! qualifier is ignored !
            ACode object = expression(qualifier);
            return AFunction.Method(object, symbol, invokeStyle(qualifier));

        case Ident(_):
            return AFunction.Method(make.Void, symbol, AInvokeStyle.New);

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
            if (symbol.isModule())
                return ALocation.Module(symbol); // !!! qualifier is ignored !
            if (symbol.isJava() && symbol.owner().isModuleClass())
                return ALocation.Field(make.Void, symbol, true); // !!! qualifier is ignored !
            return ALocation.Field(expression(qualifier), symbol, false);

        case Ident(_):
            if (symbol.isModule()) return ALocation.Module(symbol);
            return ALocation.Local(symbol, symbol.isParameter());

        default:
            throw Debug.abort("illegal case", tree);
        }
    }

    //########################################################################
}
