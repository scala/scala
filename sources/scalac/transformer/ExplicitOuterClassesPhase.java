/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;
import scalac.ast.GenTransformer;
import scalac.ast.Tree;
import scalac.ast.Tree.Ident;
import scalac.ast.Tree.Template;
import scalac.symtab.Modifiers;
import scalac.symtab.Symbol;
import scalac.symtab.TermSymbol;
import scalac.symtab.Type;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Names;

/**
 * This phase does the following:
 *
 * - In every nested class, adds to each of its constructor a new
 *   value parameter that contains a link to the outer class.
 *
 * - In every nested type, adds to each of its constructor a new type
 *   parameter for every type parameter appearing in outer types.
 *
 * - In every class, adds a forwarding "super" method for every method
 *   that is accessed via "super" in a nested class.
 *
 * - Replaces all prefixes of TypeRefs by localThisTypes.
 *
 * - Adds all missing qualifiers.
 */
public class ExplicitOuterClassesPhase extends Phase {

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ExplicitOuterClassesPhase(Global global,PhaseDescriptor descriptor){
        super(global, descriptor);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given type for the given symbol. */
    public Type transformInfo(Symbol symbol, Type type) {
        if (symbol.isPackage()) return type;
        // if (!symbol.isJava() && symbol.isConstructor()) // !!!
        //System.out.println("!!! " + Debug.show(symbol) + ": " + type + " -> " + typeTransformer.apply(type));
        type = typeTransformer.apply(type);
        if (symbol.isJava()) return type;
        if (symbol.isConstructor()) {
            Symbol[] tparams = type.typeParams();
            Symbol[] vparams = type.valueParams();
            Type result = type.resultType();
            // Add outer value link
            if (hasOuterValueLink(symbol)) {
                int flags = Modifiers.PARAM | Modifiers.SYNTHETIC;
                Name name = Names.OUTER(symbol);
                Symbol vlink = new TermSymbol(symbol.pos, name, symbol, flags);
                vlink.setInfo(getOuterClass(symbol).typeOfThis()); // !!!
                vparams = Symbol.cloneArray(1, vparams);
                vparams[0] = vlink;
            }
            // Add outer type links
            if (hasOuterTypeLinks(symbol)) {
                Symbol[] oldtparams = getOuterClass(symbol).nextTypeParams();
                Symbol[] tlinks = Symbol.cloneArray(oldtparams);
                for (int i = 0; i < tlinks.length; i++) {
                    tlinks[i] = oldtparams[i].cloneSymbol(symbol);
                    tlinks[i].name = Names.OUTER(symbol, oldtparams[i]);
                }
                tparams = Symbol.concat(tlinks, tparams);
                result = Type.getSubst(oldtparams, tlinks, true).apply(result);
            }
            type = Type.MethodType(vparams, result);
            if (tparams.length != 0) type = Type.PolyType(tparams, type);
        } else {
            Symbol owner = symbol.owner().isConstructor()
                ? symbol.owner()
                : symbol.enclClass();
            //System.out.println("!!! debug2 = " + Debug.show(symbol) + " - " + Debug.show(owner) + " --- " + (owner == Symbol.NONE) + " -- #" + owner.name + "#");
            //if (onwer.isJava() && owner != Symbol.NONE && owner.name.length() > 0) // !!!
            if (owner.isType() || owner.isConstructor())
            type = getOuterTypeSubst(owner, true).apply(type);
        }

        /*

        String s1 = Debug.show(symbol);
        String s2 = symbol.info().toString();
        symbol.updateInfo(type);
        global.nextPhase();
        String s3 = type.toString();
        global.prevPhase();
        System.out.println("!!! symbol = " + s1);
        System.out.println("!!! type   = " + s2 + " -- " + System.identityHashCode(s2));
        System.out.println("!!! new    = " + s3 + " -- " + System.identityHashCode(s3));
        System.out.println("!!!");

        */

        return type;
    }

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        treeTransformer.apply(units);
    }

    //########################################################################
    // Private Methods - Outer class

    /** Returns the outer class of the given type or constructor. */
    private Symbol getOuterClass(Symbol symbol) {
        assert symbol.isType() || symbol.isConstructor(): Debug.show(symbol);
        return symbol.owner();
    }

    /** Has the given class or constructor an outer value link? */
    private boolean hasOuterValueLink(Symbol symbol) {
        assert symbol.isClass() || symbol.isConstructor(): Debug.show(symbol);
        return !symbol.isJava() && getOuterClass(symbol).isClass();
    }

    /** Returns the outer link of the given class or constructor. */
    private Symbol getOuterValueLink(Symbol symbol) {
        if (!hasOuterValueLink(symbol)) return Symbol.NONE;
        if (symbol.isClass()) symbol = symbol.primaryConstructor();
        return symbol.nextValueParams()[0];
    }

    /** Has the given type or constructor outer type links? */
    private boolean hasOuterTypeLinks(Symbol symbol) {
        assert symbol.isType() || symbol.isConstructor(): Debug.show(symbol);
        if (symbol.isJava()) return false;
        Symbol outer = getOuterClass(symbol);
        return outer.isType() && outer.nextTypeParams().length != 0;
    }

    /** Returns the type substitution for the given class or constructor. */
    private Type.Map getOuterTypeSubst(Symbol symbol, boolean update) {
        if (!hasOuterTypeLinks(symbol)) return Type.IdMap;
        Symbol[] oldtparams = getOuterTypeParams(symbol);
        Symbol[] newtparams = symbol.nextTypeParams();
        Symbol[] tlinks = new Symbol[oldtparams.length];
        for (int i = 0; i < tlinks.length; i++) tlinks[i] = newtparams[i];
        return Type.getSubst(oldtparams, tlinks, update);
    }

    /** Returns the outer type parameters of the given class or constructor. */
    private Symbol[] getOuterTypeParams(Symbol symbol) {
        Symbol outer = getOuterClass(symbol);
        Symbol[] tparams = Symbol.cloneArray(outer.nextTypeParams());
        for (int i = tparams.length; i != 0; outer = getOuterClass(outer)) {
            Symbol[] symbols = outer.typeParams();
            for (int j = symbols.length; j != 0; ) tparams[--i] = symbols[--j];
        }
        return tparams;
    }

    /**
     * Extracts from given prefix the outer type arguments for the
     * given class or constructor.
     */
    private Type[] getOuterTypeArgs(Type prefix, Symbol symbol) {
        if (!hasOuterTypeLinks(symbol)) return Type.EMPTY_ARRAY;
        Symbol outer = getOuterClass(symbol);
        global.nextPhase();
        Type[] targs = prefix.baseType(outer).widen().typeArgs();
        global.prevPhase();
        assert targs.length == outer.nextTypeParams().length:
            "\nsymbol = " + Debug.show(symbol) +
            "\nprefix = " + prefix;
        return targs;
    }

    //########################################################################
    // Private Class - Type transformer

    /** The type transformer */
    private final Type.Map typeTransformer = new Type.MapOnlyTypes() {
        public Type apply(Type type) {
            switch (type) {
            case TypeRef(Type prefix, Symbol symbol, Type[] targs):
                if (!symbol.owner().isType()) break;
                prefix = apply(prefix);
                targs = map(targs);
                targs = Type.concat(getOuterTypeArgs(prefix, symbol), targs);
                if (symbol.isClassType()) {
                    // !!! add assertions ?
                    prefix = Type.localThisType;
                } else {
                    // !!! replace outer ThisTypes by SingleTypes to outer link
                }
                return Type.TypeRef(prefix, symbol, targs);
            }
            return map(type);
        }
    };

    //########################################################################
    // Private Class - Tree transformer

    /** The tree transformer */
    private final GenTransformer treeTransformer = new GenTransformer(global) {

        /** The current context */
        private Context context;

        /** Transforms the given tree. */
        public Tree transform(Tree tree) {
            switch (tree) {

            case ClassDef(_, _, _, _, _, Template impl):
                Symbol clasz = tree.symbol();
                context = new Context(context, clasz, new HashMap());
                Tree[] parents = transform(impl.parents);
                Tree[] body = transform(impl.body);
                body = Tree.concat(body, genSuperMethods());
                context = context.outer;
                if (context != null) clasz.flags |= Modifiers.STATIC;
                return gen.ClassDef(clasz, parents, impl.symbol(), body);

            case DefDef(_, _, _, _, _, Tree rhs):
                Symbol method = tree.symbol();
                Context backup = context;
                if (method.isConstructor())
                    context = context.getConstructorContext(method);
                else
                    context.inMethod = true;
                rhs = transform(rhs);
                context.inMethod = false;
                context = backup;
                return gen.DefDef(method, rhs);

            case AbsTypeDef(_, _, _, _):
            case AliasTypeDef(_, _, _, _):
                // eliminate // !!!
                return Tree.Empty;

            case Typed(Tree expr, Tree tpe):
                // eliminate // !!!
                return transform(expr);

            case Apply(Tree vfun, Tree[] vargs):
                switch (vfun) {
                case TypeApply(Tree tfun, Tree[] targs):
                    if (!tfun.symbol().isConstructor()) break;
                    return transform(tree, vargs, vfun, targs, tfun);
                default:
                    if (!vfun.symbol().isConstructor()) break;
                    return transform(tree, vargs, vfun, Tree.EMPTY_ARRAY,vfun);
                }
                return super.transform(tree);

            case This(_):
                return genOuterRef(tree.pos, tree.symbol());

            case Select(Tree qualifier, _):
                Symbol symbol = tree.symbol();
                switch (qualifier) {
                case Super(_, _):
                    Symbol clasz = qualifier.symbol();
                    if (clasz == context.clasz) {
                        qualifier = gen.Super(tree.pos, qualifier.symbol());
                    } else {
                        qualifier = genOuterRef(qualifier.pos, clasz);
                        symbol = getSuperMethod(clasz, symbol);
                    }
                    break;
                default:
                    qualifier = transform(qualifier);
                    break;
                }
                return gen.Select(tree.pos, qualifier, symbol);

            case Ident(_):
                Symbol symbol = tree.symbol();
                Symbol owner = symbol.owner();
                if (owner.isClass()) {
                    // !!! A this node is missing here. This should
                    // never happen if all trees were correct.
                    Tree qualifier = genOuterRef(tree.pos, owner);
                    return gen.Select(qualifier, symbol);
                }
                if (owner.isPrimaryConstructor()) {
                    Symbol clasz = owner.constructorClass();
                    if (clasz != context.clasz || context.inMethod) {
                        Tree qualifier = genOuterRef(tree.pos, clasz);
                        return gen.Select(qualifier, symbol);
                    }
                }
                return gen.Ident(tree.pos, symbol);

            case TypeTerm():
                Type type = typeTransformer.apply(tree.getType());
                if (context != null) type = context.subst.apply(type);
                return gen.TypeTerm(tree.pos, type);

            default:
                return super.transform(tree);
            }
        }

        /* Add outer type and value arguments to constructor calls. */
        private Tree transform(Tree vapply, Tree[] vargs, Tree tapply,
            Tree[] targs, Tree tree)
        {
            Symbol symbol = tree.symbol();
            targs = transform(targs);
            vargs = transform(vargs);
            switch (transform(tree)) {
            case Select(Tree qualifier, _):
                if (hasOuterValueLink(symbol)) {
                    vargs = Tree.cloneArray(1, vargs);
                    vargs[0] = qualifier;
                }
                Type[] types = getOuterTypeArgs(qualifier.getType(), symbol);
                if (types.length != 0) {
                    targs = Tree.cloneArray(types.length, targs);
                    for (int i = 0; i < types.length; i++)
                        targs[i] = gen.mkType(tapply.pos, types[i]);
                }
            }
            tree = gen.Ident(tree.pos, symbol);
            if (targs.length != 0) tree = gen.TypeApply(tapply.pos,tree,targs);
            return gen.Apply(vapply.pos, tree, vargs);
        }

        /**
         * Returns the forwarding "super" method of the given class
         * that invokes the given method. If the symbol does not yet
         * exist, it is created and added to the class members.
         */
        private Symbol getSuperMethod(Symbol clasz, Symbol method) {
            Context context = this.context;
            for (; context.clasz != clasz; context = context.outer)
                assert context.outer != null : Debug.show(clasz);
            Symbol forward = (Symbol)context.supers.get(method);
            if (forward == null) {
                Name name = Names.SUPER(method);
                int flags = Modifiers.PRIVATE | Modifiers.FINAL;
                forward = new TermSymbol(method.pos, name, clasz, flags);
                forward.setInfo(method.nextType().cloneType(method, forward));
                context.supers.put(method, forward);
                clasz.nextInfo().members().enter(forward);
                assert Debug.log("created forwarding method: ", forward);
            }
            return forward;
        }

        /** Generates the trees of the forwarding "super" methods. */
        private Tree[] genSuperMethods() {
            if (context.supers.size() == 0) return Tree.EMPTY_ARRAY;
            Tree[] trees = new Tree[context.supers.size()];
            Iterator entries = context.supers.entrySet().iterator();
            for (int i = 0; i < trees.length; i++) {
                Map.Entry entry = (Map.Entry)entries.next();
                Symbol method = (Symbol)entry.getKey();
                Symbol forward = (Symbol)entry.getValue();
                int pos = forward.pos;
                Tree[] targs = gen.mkTypeRefs(pos, forward.nextTypeParams());
                Tree[] vargs = gen.mkRefs(pos, forward.nextValueParams());
                Tree fun = gen.Select(gen.Super(pos, context.clasz), method);
                trees[i] = gen.DefDef(forward, gen.mkApplyTV(fun,targs,vargs));
            }
            return trees;
        }

        /** Returns a tree referencing the given outer class. */
        private Tree genOuterRef(int pos, Symbol clasz) {
            if (context.clasz == clasz) return gen.This(pos, clasz);
            Tree tree = context.inMethod
                ? gen.Select(gen.This(pos, context.clasz), context.link)
                : gen.Ident(pos, context.link);
            for (Context context = this.context.outer;;context =context.outer){
                assert context != null: Debug.show(clasz);
                if (context.clasz == clasz) return tree;
                tree = gen.Select(tree, context.link);
            }
        }

    };

    //########################################################################
    // Private Class - Tree transformer context

    /** This class represents the tree transformation context. */
    private class Context {

        /** The outer context */
        public final Context outer;
        /** The current class symbol */
        public final Symbol clasz;
        /** The super methods (maps invoked to forwarding methods) */
        public final Map/*<Symbol,Symbol>*/ supers;
        /** The outer type parameter substitution */
        public final Type.Map subst;
        /** The link to the outer class */
        public final Symbol link;
        /**  True if in a method of current class */
        public boolean inMethod;

        /** Initializes this instance. */
        public Context(Context outer, Symbol symbol, Map supers) {
            this.outer = outer;
            this.clasz = symbol.constructorClass();
            this.supers = supers;
            this.subst = getOuterTypeSubst(symbol, false);
            this.link = getOuterValueLink(symbol);
            this.inMethod = false;
        }

        /** Returns a context for the given constructor. */
        public Context getConstructorContext(Symbol constructor) {
            assert constructor.constructorClass() == clasz;
            return new Context(outer, constructor, supers);
        }

    }

    //########################################################################
}
