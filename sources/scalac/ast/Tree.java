/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast;

import scalac.ast.printer.*;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Position;
import scalac.symtab.Type;
import scalac.symtab.Symbol;

public class Tree {

    //########################################################################
    // Public Fields

    public int pos = Position.NOPOS;
    public Type type;

    //########################################################################
    // Public Constants

    /** Empty tree array */
    public static final Tree[] EMPTY_ARRAY = new Tree[0];

    //########################################################################
    // Public Cases

    /**
     * Representation for parser errors
     * - introduced by: parser
     * - eliminated by: -
     */
    public case Bad();

    /**
     * A tree node for the absence of a tree
     * - introduced by: parser, ... !!! ?
     * - eliminated by: !!! ?
     */
    public case Empty; static { Empty.type = Type.NoType; }

    /**
     * Class and data declaration
     * - introduced by: parser
     * - eliminated by: -
     */
    public case ClassDef(int mods,
                         Name name,
                         TypeDef[] tparams,
                         ValDef[][] vparams,
			 Tree tpe,
                         Template impl) {
	assert name.isTypeName();
    }

    /**
     * Package declaration
     * - introduced by: parser
     * - eliminated by: !!! ?
     */
    public case PackageDef(Tree packaged,
                           Template impl) {
	if (!packaged.isTerm())
	    throw unexpected("PackageDef expects term as rhs", packaged);
    }

    /**
     * Module declaration
     * - introduced by: parser
     * - eliminated by: !!! ?
     */
    public case ModuleDef(int mods,
                          Name name,
                          Tree tpe,
                          Template impl) {
	assert !name.isTypeName();
    }

    /**
     * Value declaration (var or let)
     * - introduced by: parser
     * - eliminated by: -
     */
    public case ValDef(int mods,
                       Name name,
                       Tree tpe,
                       Tree rhs) {
	assert !name.isTypeName();
	if (!tpe.isType())
	    throw unexpected("ValDef expects type as tpe", tpe);
	if (!rhs.isTerm())
	    throw unexpected("ValDef expects term as rhs", rhs);
    }

    /**
     * Value declaration with patterns (val)
     * - introduced by: parser
     * - eliminated by: desugarizer
     */
    public case PatDef(int mods,
                       Tree pat,
                       Tree rhs) {
	if (!rhs.isTerm())
	    throw unexpected("PatDef expects term as rhs", rhs);
    }

    /**
     * Function declaration (def)
     * - introduced by: parser
     * - eliminated by: -
     */
    public case DefDef(int mods,
                       Name name,
                       TypeDef[] tparams,
                       ValDef[][] vparams,
                       Tree tpe,
                       Tree rhs) {
	assert !name.isTypeName();
	if (!tpe.isType())
	    throw unexpected("DefDef expects type as tpe", tpe);
	if (!rhs.isTerm())
	    throw unexpected("DefDef expects term as rhs", rhs);
    }

    /**
     * Type declaration
     * - introduced by: parser
     * - eliminated by: ersure // !!! could/should be don earlier?)
     */
    public case TypeDef(int mods,
                        Name name,
			TypeDef[] tparams,
                        Tree rhs) {
	assert name.isTypeName();
	if (!rhs.isType())
	    throw unexpected("TypeDef expects type as rhs", rhs);
    }

    /**
     * Import declaration
     * - introduced by: parser
     * - eliminated by: !!! ?
     */
    public case Import(Tree expr, Name[] selectors) {
	if (!expr.isTerm())
	    throw unexpected("Import expects term", expr);
    }

    /**
     * Case declaration
     * - introduced by: parser
     * - eliminated by: !!! ?
     */
    public case CaseDef(Tree pat,
                        Tree guard,
                        Tree body) {
	if (!guard.isTerm())
	    throw unexpected("CaseDef expects term as guard", guard);
	if (!body.isTerm())
	    throw unexpected("CaseDef expects term as body", body);
    }

    /**
     * Instantiation templates
     * - introduced by: parser
     * - eliminated by: -
     */
    public case Template(Tree[] parents,
                         Tree[] body) {
	if (parents != null) {
	    for (int i = 0; i < parents.length; i++) {
		if (!parents[i].isTerm())
		    throw unexpected("Template requires terms as baseClasses", parents[i]);
	    }
	}
    }

    /**
     * Labelled expression - the symbols in the array (must be
     * Idents!) are those the label takes as argument
     * - introduced by: optimizer
     * - eliminated by: -
     */
    public case LabelDef(Tree[] params,Tree rhs) {
	for (int i = 0;i < params.length; i++) {
	    if (!(params[i] instanceof Ident))
		throw unexpected("LabelDef requires Idents", params[i]);
	}
    }

    /**
     * Block of expressions (semicolon separated expressions)
     * - introduced by: parser
     * - eliminated by: !!! ?
     */
    public case Block(Tree[] stats);

    /**
     * Tuple of expressions (comma separated expressions)
     * - introduced by: !!! ?
     * - eliminated by: !!! ?
     */
    public case Tuple(Tree[] trees) {
	if (trees != null) {
	    for (int i = 0; i < trees.length; i++) {
		if (!trees[i].isTerm())
		    throw unexpected("Tuple requires terms", trees[i]);
	    }
	}
    }

    /**
     * Visitor (a sequence of cases)
     * - introduced by: parser
     * - eliminated by: !!! ?
     */
    public case Visitor(CaseDef[] cases);

    /**
     * Anonymous function
     * - introduced by: parser
     * - eliminated by: !!! ?
     */
    public case Function(ValDef[] vparams,
                         Tree body) {
	if (!body.isTerm())
	    throw unexpected("Function body has to be a term", body);
    }

    /**
     * Assignment
     * - introduced by: parser
     * - eliminated by: !!! ?
     */
    public case Assign(Tree lhs,
                       Tree rhs) {
	if (!lhs.isTerm())
	    throw unexpected("lhs of Assign has to be a term", lhs);
	if (!rhs.isTerm())
	    throw unexpected("rhs of Assign has to be a term", rhs);
    }

    /**
     * Conditional expression
     * - introduced by: parser
     * - eliminated by: -
     */
    public case If(Tree cond,
                   Tree thenp,
                   Tree elsep) {
        assert cond.isTerm() &&
               thenp.isTerm() &&
               elsep.isTerm();
    }

    /**
     * Instantiation
     * - introduced by: parser
     * - eliminated by: -
     */
    public case New(Template templ);

    /**
     * Type annotation
     * - introduced by: parser
     * - eliminated by: !!! ? (could be done by analyzer ?)
     */
    public case Typed(Tree expr,
                      Tree tpe) {
	if (!expr.isTerm())
	    throw unexpected("Typed expects term as first argument", expr);
	if (!tpe.isType())
	    throw unexpected("Typed expects type as second argument", tpe);
    }

    /**
     * Type application
     * - introduced by: parser
     * - eliminated by: !!! ?
     */
    public case TypeApply(Tree fun,
                          Tree[] args) {
	if (!fun.isTerm()) {
	    new TextTreePrinter().print(fun).println().end();//debug
	    throw unexpected("TypeApply expects term as function", fun);
	}
	for (int i = 0; i < args.length; i++) {
	    if (!args[i].isType())
		throw unexpected("TypeApply expects types as arguments", args[i]);
	}
    }

    /**
     * Value application
     * - introduced by: parser
     * - eliminated by: -
     */
    public case Apply(Tree fun,
                      Tree[] args) {
	if (args != null) {
	    for (int i = 0; i < args.length; i++) {
		if (!args[i].isTerm())
		    throw unexpected("Apply expects terms as arguments", args[i]);
	    }
	}
    }

    /**
     * Super reference
     * - introduced by: parser
     * - eliminated by: -
     */
    public case Super(Tree tpe) {
	if (!tpe.isType()) {
	    throw unexpected("Super expects type", tpe);
	}
    }

    /**
     * Self reference
     * - introduced by: parser
     * - eliminated by: -
     */
    public case This(Tree qualifier) {
	if (!qualifier.isType())
	    throw unexpected("This expects type", qualifier);
    }

    /**
     * Designator
     * - introduced by: parser
     * - eliminated by: -
     */
    public case Select(Tree qualifier,
                       Name selector) {
	if (!qualifier.isTerm())
	    throw unexpected("Select expects term", qualifier);
    }

    /**
     * Identifier
     * - introduced by: parser
     * - eliminated by: -
     */
    public case Ident(Name name) {
        assert name != null;
    }

    /**
     * Literal
     * - introduced by: parser
     * - eliminated by: -
     */
    public case Literal(Object value);

    /**
     * Singleton type
     * - introduced by: parser
     * - eliminated by: !!! ? (could be done by analyzer ?)
     */
    public case SingletonType(Tree ref) {
	if (!ref.isTerm())
	    throw unexpected("SingletonType expects term", ref);
    }

    /**
     * Type selection
     * - introduced by: parser
     * - eliminated by: !!! ? (could be done by analyzer ?)
     */
    public case SelectFromType(Tree qualifier,
			       Name selector) {
	if (!qualifier.isType())
	    throw unexpected("SelectFromType expects type", qualifier);
	assert selector.isTypeName();
    }

    /**
     * Function type
     * - introduced by: parser
     * - eliminated by: !!! ? (could be done by analyzer ?)
     */
    public case FunType(Tree[] argtpes,
                        Tree restpe) {
	for (int i = 0; i < argtpes.length; i++)
	    if (!argtpes[i].isType())
		throw unexpected("FunType requires types as args", argtpes[i]);
	if (!restpe.isType())
	    throw unexpected("FunType requires type as result", restpe);
    }

    /**
     * Object type (~ Template)
     * - introduced by: parser
     * - eliminated by: !!! ? (could be done by analyzer ?)
     */
    public case CompoundType(Tree[] parents,
                             Tree[] refinements) {
	if (parents != null) {
	    assert parents.length > 0;
	    for (int i = 0; i < parents.length; i++) {
		if (!parents[i].isType())
		    throw unexpected("CompoundType requires types as parents", parents[i]);
	    }
	}
    }

    /**
     * Applied type
     * - introduced by: parser
     * - eliminated by: !!! ? (could be done by analyzer ?)
     */
    public case AppliedType(Tree tpe, Tree[] args) {
	assert tpe.isType() : this;
	for (int i = 0; i < args.length; i++) assert args[i].isType() : args[i];
    }

    /**
     * Covariant type
     * - introduced by: parser
     * - eliminated by: !!! ? (could be done by analyzer ?)
     */
    public case CovariantType(Tree tpe) {
        assert tpe.isType();
    }

    //########################################################################
    // Public Methods - queries

    /** Returns true if this tree is a term. */
    public boolean isTerm() {
	switch(this) {
        case Bad():
	case Empty:
	case Tuple(_):
	case If(_, _, _):
	case Typed(_, _):
	case Apply(_, _):
	case TypeApply(_, _):
	case Visitor(_):
	case New(_):
	case Literal(_):
	case LabelDef(_,_):
	case Block(_):
	case Function(_, _):
	case Assign(_, _):
	case Super(_):
	case This(_):
	    return true;
	case Ident(Name name):
	    return !name.isTypeName();
	case Select(_, Name name):
	    return !name.isTypeName();
	default:
	    return false;
	}
    }

    /** Returns true if this tree is a type. */
    public boolean isType() {
	switch(this) {
        case Bad():
	case Empty:
	case SingletonType(_):
	case SelectFromType(_, _):
	case CompoundType(_, _):
	case FunType(_, _):
	case AppliedType(_, _):
	case CovariantType(_):
	    return true;
	case Ident(Name name):
	    return name.isTypeName();
	case Select(_, Name name):
	    return name.isTypeName();
	default:
	    return false;
	}
    }

    //########################################################################
    // Public Methods - tree type

    /** Get the type of the node. */
    public Type type() {
        assert type != null : this;
        return type;
    }

    /** Set the type of the node. */
    public Tree setType(Type type) {
	assert !(type instanceof Type.LazyType) : symbol();
        this.type = type;
	return this;
    }

    /** Get types attached to array of nodes. */
    public static Type[] typeOf(Tree[] trees) {
	Type[] tps = new Type[trees.length];
	for (int i = 0; i < trees.length; i++)
	     tps[i] = trees[i].type();
	return tps;
    }

    //########################################################################
    // Public Methods - tree symbol

    /** Has this tree a symbol field? */
    public boolean hasSymbol() {
        return false;
    }

    /** Get symbol attached to the node, if any. */
    public Symbol symbol () {
        return null;
    }

    /** Set symbol attached to the node, if possible. */
    public Tree setSymbol(Symbol sym) {
        throw Debug.abort("no settable symbol for node", this);
    }

    /** Get symbols attached to array of nodes. */
    public static Symbol[] symbolOf(Tree[] trees) {
	Symbol[] syms = new Symbol[trees.length];
	for (int i = 0; i < trees.length; i++)
	     syms[i] = trees[i].symbol();
	return syms;
    }

    /** Tells if the tree defines a symbol. */
    public boolean definesSymbol() {
	return false;
    }

    //########################################################################
    // Public Methods - tree to string

    /**
     * Get string corresponding to this tree only implemented for
     * prefix trees, maybe we should generalize this; the PatternMatch
     * phase needs support for Apply, so this case got added
     */
    public String toString() {
	switch (this) {
	case This(Tree qual):
	    return (qual == Tree.Empty) ? "this" : qual + ".this";
	case Select(Tree qual, Name name):
	    return qual + "." + name;
	case Ident(Name name):
	    return name.toString();
        case Apply(Tree fn, Tree[] args):
            String res = fn + "(";
            if ((args == null) || (args.length == 0))
                return res + ")";
            res += args[0].toString();
            for (int i = 1; i < args.length; i++)
                res += ", " + args[i];
            return res + ")";
        case Literal(Object value):
            if (value instanceof String)
                return "\"" + value + "\"";
            else
                return value.toString();
        case Import(Tree expr, _):
            return "import " + expr;
	case Empty:
	    return "<empty>";
	default:
	    return super.toString();
	}
    }

    //########################################################################
    // Private Methods

    private static Error unexpected(String message, Tree actual) {
        return Debug.abort(message + "; found", actual);
    }

    //########################################################################
    // Public Extended Classes

    public static class ExtBad extends Bad {
        private Symbol symbol;

        public ExtBad() {
            super();
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }
    }

    public static class ExtClassDef extends ClassDef {
        private Symbol symbol;

        public ExtClassDef(int mods, Name name, TypeDef[] tparams,
            ValDef[][] vparams, Tree tpe, Template impl)
        {
            super(mods, name, tparams, vparams, tpe, impl);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }

	public boolean definesSymbol() {
	    return true;
	}
    }

    public static class ExtModuleDef extends ModuleDef {
        private Symbol symbol;

        public ExtModuleDef(int mods, Name name, Tree tpe, Template impl)
        {
            super(mods, name, tpe, impl);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }

	public boolean definesSymbol() {
	    return true;
	}
    }

    public static class ExtValDef extends ValDef {

	public static final ValDef[] EMPTY_ARRAY = new ValDef[0];
	public static final ValDef[][] EMPTY_ARRAY_ARRAY = new ValDef[0][0];

        private Symbol symbol;

        public ExtValDef(int mods, Name name, Tree tpe, Tree rhs)
        {
            super(mods, name, tpe, rhs);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }

	public boolean definesSymbol() {
	    return true;
	}
    }

    public static class ExtDefDef extends DefDef {
        private Symbol symbol;

        public ExtDefDef(int mods, Name name, TypeDef[] tparams,
                         ValDef[][] vparams, Tree tpe, Tree rhs)
        {
            super(mods, name, tparams, vparams, tpe, rhs);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }

	public boolean definesSymbol() {
	    return true;
	}
    }

    public static class ExtTypeDef extends TypeDef {
        private Symbol symbol;

	public static final TypeDef[] EMPTY_ARRAY = new TypeDef[0];

        public ExtTypeDef(int mods, Name name, TypeDef[] tparams, Tree rhs)
        {
            super(mods, name, tparams, rhs);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }

	public boolean definesSymbol() {
	    return true;
	}
    }

    public static class ExtImport extends Import {
        private Symbol symbol;

        public ExtImport(Tree expr, Name[] selectors) {
            super(expr, selectors);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }
    }

    public static class ExtLabelDef extends LabelDef {
	private Symbol symbol;

	public ExtLabelDef(Tree[] params,Tree rhs) {
	    super(params,rhs);
	}

	public boolean hasSymbol() {
	    return true;
	}

	public Symbol symbol() {
	    return symbol;
	}

	public Tree setSymbol(Symbol symbol) {
	    this.symbol = symbol;
	    return this;
	}

	public boolean definesSymbol() {
	    return true;
	}
    }

    public static class ExtSelect extends Select {
        private Symbol symbol;

        public ExtSelect(Tree qualifier, Name selector) {
            super(qualifier, selector);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }
    }

    public static class ExtSelectFromType extends SelectFromType {
        private Symbol symbol;

        public ExtSelectFromType(Tree qualifier, Name selector) {
            super(qualifier, selector);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }
    }

    public static class ExtIdent extends Ident {
        private Symbol symbol;

        public ExtIdent(Name name) {
            super(name);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }
    }

    public static class ExtTemplate extends Template {
        private Symbol symbol;

        public ExtTemplate(Tree[] parents, Tree[] body) {
	    super(parents, body);
        }

        public boolean hasSymbol() {
            return true;
        }

        public Symbol symbol() {
            return symbol;
        }

        public Tree setSymbol(Symbol symbol) {
            this.symbol = symbol;
            return this;
        }

	public boolean definesSymbol() {
	    return true;
	}
    }

    //########################################################################
}
