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
import scalac.symtab.AbsTypeSymbol;
import scalac.symtab.Modifiers;
import scalac.symtab.Scope;
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
// !!! needs to be cleaned
public class ExplicitOuterClassesPhase extends Phase {

    //########################################################################
    // Private Fields

    /** A map from constructor symbols to type contexts */
    private final Map/*<Symbol,TypeContext>*/ contexts = new HashMap();

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ExplicitOuterClassesPhase(Global global,PhaseDescriptor descriptor){
        super(global, descriptor);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        treeTransformer.apply(units);
    }

    private boolean show = false; // !!!

    /** Applies this phase to the given type for the given symbol. */
    public Type transformInfo(Symbol symbol, Type type) {
        if (show && !symbol.isPackage()) System.out.println("!!! <<< transformInfo - symbol: " + Debug.show(symbol));
        if (show && !symbol.isPackage()) System.out.println("!!! <<< transformInfo - type  : " + Debug.show(type));
        if (symbol.isPackage()) return type; // !!!
        TypeContext context = getTypeContextFor(symbol);
        if (symbol.isConstructor() && symbol.constructorClass().isClassType()) { // !!! isClassType -> isClass ?
            Symbol clasz = symbol.constructorClass();
            Symbol[] tparams = type.typeParams();
            Symbol[] vparams = type.valueParams();
            Type result = type.resultType();
            result = context.transformer.apply(result);
            if (context.vlink != null) {
                vparams = Symbol.cloneArray(1, vparams);
                vparams[0] = context.vlink;
            }
            Type prefix = clasz.owner().thisType();
            Type[] args = Symbol.type(tparams);
            // !!! use getNewTypeArgs ?
            Type self = Type.typeRef(prefix, clasz, args);
            Type s = self;
            self = context.transformer.apply(self);
            tparams = Type.symbol(self.typeArgs());
            for (int i = 0; i < tparams.length; i++)
                assert tparams[i].isParameter() && tparams[i].owner() == symbol:
                    Debug.show(symbol, " -- ", clasz, " -- ", self);
            type = Type.MethodType(vparams, result);
            if (tparams.length != 0) type = Type.PolyType(tparams, type);
        } else {
            Type t = type;
            type = context.transformer.apply(type);
            assert type != null: Debug.show(symbol) + " -- " + t;
        }
        if (show && !symbol.isPackage()) System.out.println("!!! >>> transformInfo - symbol: " + Debug.show(symbol));
        if (show && !symbol.isPackage()) System.out.println("!!! >>> transformInfo - type  : " + Debug.show(type));
        return type;
    }

    //########################################################################
    // Private Methods

    /** Returns the type context for the given symbol. */
    private TypeContext getTypeContextFor(Symbol symbol) {
        while (!symbol.isClassType() && !(symbol.isConstructor() && symbol.constructorClass().isClassType())) // !!! isClassType -> isClass ?
            symbol = symbol.owner();
        if (symbol.isClassType())
            symbol = symbol.primaryConstructor();
        TypeContext context = (TypeContext)contexts.get(symbol);
        if (context == null) {
            context = createTypeContext(symbol);
            contexts.put(symbol, context);
        }
        return context;
    }

    /** Creates the context for the given constructor. */
    private TypeContext createTypeContext(Symbol constructor) {
        Symbol clasz = constructor.constructorClass();

        // get outer contexts
        TypeContext[] outers;
        if (clasz.isRoot()) {
            outers = new TypeContext[0];
        } else {
            TypeContext outer = getTypeContextFor(clasz.owner());
            outers = new TypeContext[1 + outer.outers.length];
            outers[0] = outer;
            for (int i = 1; i < outers.length; i++)
                outers[i] = outer.outers[i - 1];
        }

        // create outer type links
        Symbol[] tlinks = new Symbol[outers.length];
        int tflags = Modifiers.PARAM | Modifiers.COVARIANT | Modifiers.SYNTHETIC | Modifiers.STABLE;
        for (int i = 0; i < outers.length; i++) {
            if (outers[i].isStable) continue;
            Name tname = Names.OUTER(constructor, outers[i].clasz);
            tlinks[i] = new AbsTypeSymbol(constructor.pos, tname, constructor, tflags);
            tlinks[i].setInfo(outers[i].clasz.type());
        }

        // create outer value link
        Symbol vlink = null;
        if (outers.length > 0 && (outers[0].vlink != null || !outers[0].isStable)) {
            int index = 0;
            while (outers[index].isStable) index++;
            int vflags = Modifiers.PARAM | Modifiers.SYNTHETIC;
            Name vname = Names.OUTER(constructor);
            vlink = new TermSymbol(constructor.pos, vname, constructor, vflags);
            vlink.setInfo(outers[index].clasz.thisType());
        }

        // create new type parameters
        Map tparams = new HashMap();
        for (int o = 0; o < outers.length; o++) {
            Symbol[] oldtparams = outers[o].oldtparams;
            for (int i = 0; i < oldtparams.length; i++) {
                Symbol oldtparam = oldtparams[i];
                Symbol newtparam = oldtparam.cloneSymbol(constructor);
                newtparam.name = Names.OUTER(constructor, oldtparam);
                tparams.put(oldtparam, newtparam.type());
            }
        }
        // !!! duplicated code
        Symbol[] oldtparams = constructor.typeParams();
        for (int i = 0; i < oldtparams.length; i++) {
            Symbol oldtparam = oldtparams[i];
            Symbol newtparam = oldtparam;
            tparams.put(oldtparam, newtparam.type());
        }

        return new TypeContext(clasz, outers, tlinks, vlink, constructor.typeParams(), tparams);
    }


    /** !!! */
    // !!! where is not used
    // !!! prefix is an old type
    // !!! args are old types
    // !!! returns old types
    private Type[] getNewArgsOf(TypeContext where, Type prefix, Symbol clasz, Type[] args) {
        TypeContext context = getTypeContextFor(clasz);
        int vlinks = 0; // !!!
        for (int i = 0; i < context.outers.length; i++)
            if (!context.outers[i].isStable) vlinks++;
        Type[] types = new Type[context.transformer.tparams.size() + vlinks];
        int p = types.length;
        for (int i = args.length; 0 < i; ) types[--p] = args[--i];
        for (int o = 0; o < context.outers.length - 1; o++) {
            if (!context.outers[o].isStable)
                types[--p] = prefix;
            else if (context.outers[o].clasz.isPackage()) break;
            Type base = prefix.baseType(context.outers[o].clasz);
            assert base.symbol() == context.outers[o].clasz:
                prefix + " -- " + Debug.show(clasz) + " -- " + context.outers[o].clasz + " -- " + base;
            prefix = base.prefix();
            args = base.typeArgs();
            for (int i = args.length; 0 < i; ) types[--p] = args[--i];
        }
        // !!! assert p == 0: p;
        for (int i = 0; i < types.length; i++) { // !!!
            assert types[i] != null:
                "\nprefix = " + prefix +
                "\nclasz  = " + Debug.show(clasz) +
                "\nargs   = " + Debug.show(args) +
                "\ntypes  = " + Debug.show(types) +
                "\ncontext= " + context;
        }
        return types;
    }

    //########################################################################
    // Private Class - Type transformer context

    private class TypeContext {

        /** The context class */
        private final Symbol clasz;
        /** Is this context class stable? */
        private final boolean isStable;
        /** The outer contexts (from innermost to outermost) */
        private final TypeContext[] outers;
        /** The outer type links (null for stable outer contexts) */
        private final Symbol[] tlinks;
        /** The outer value link (null if all outer contexts are stable) */
        private final Symbol vlink;
        /** The old type parameters of the context class */
        private Symbol[] oldtparams;

        /** !!! */
        private TypeTransformer transformer; // !!! type

        /** !!! */
        public TypeContext(Symbol clasz, TypeContext[] outers, Symbol[] tlinks, Symbol vlink, Symbol[] oldtparams, Map tparams) {
            this.clasz = clasz;
            this.isStable = clasz.isPackage() || (clasz.isModuleClass() && vlink == null);
            this.outers = outers;
            this.tlinks = tlinks;
            this.vlink = vlink;
            this.oldtparams = oldtparams;
            this.transformer = new TypeTransformer(this, tparams);
        }

        /** !!! */
        public Type getTypeLink(int level) {
            if (tlinks.length <= level) return Type.localThisType;
            if (tlinks[level] != null) return tlinks[level].type();
            return Type.singleType(getTypeLink(level + 1), outers[level].clasz.module());
        }

        public String toString() {
            return
                "\nclasz    = " + Debug.show(clasz) +
                "\nisStable = " + isStable +
                "\ntlinks   = " + Debug.show(tlinks) +
                "\nvlink    = " + Debug.show(vlink) +
                "\noldparams= " + Debug.show(oldtparams) +
                "\ntparams  = " + Debug.show(transformer.tparams) +
                (outers.length > 0
                    ? ("\nouter    : " + "\n" + outers[0])
                    : "");
        }

    }

    //########################################################################
    // Private Class - Type transformer

    /** The type transformer */
    private final class TypeTransformer extends Type.MapOnlyTypes {

        private TypeContext context;
        private Map/*<Symbol,Type>*/ tparams;

        public TypeTransformer(TypeContext context, Map tparams) {
            this.context = context;
            this.tparams = tparams;
        }

        public Type apply(Type type) {
            switch (type) {
            case TypeRef(Type prefix, Symbol symbol, Type[] args):
                if (symbol.isParameter() && symbol.owner().isConstructor()) {
                    assert prefix.equals(Type.localThisType): type;
                    assert args.length == 0: type;
                    Object value = tparams.get(symbol);
                    return value != null ? (Type)value : type;
                }
                if (symbol.isClass()) {
                    args = map(getNewArgsOf(context, prefix, symbol, args));
                    prefix = Type.localThisType;
                    return Type.typeRef(prefix, symbol, args);
                }
                if (symbol.isPackage()) {
                    args = Type.EMPTY_ARRAY;
                    prefix = Type.localThisType;
                    return Type.typeRef(prefix, symbol, args);
                }
                return Type.typeRef(apply(prefix), symbol, map(args));
            case SingleType(Type prefix, Symbol symbol):
                return Type.singleType(apply(prefix), symbol);
            case ThisType(Symbol clasz):
                if (clasz == Symbol.NONE) return type;
                if (clasz == context.clasz) return type;
                for (int i = 0; i < context.outers.length; i++)
                    if (clasz == context.outers[i].clasz)
                        return context.getTypeLink(i);
                throw Debug.abort("illegal ThisType", type);
            case CompoundType(Type[] parents, Scope members):
                // !!! this case should not be needed
                return Type.compoundType(map(parents), members, type.symbol());
            default:
                return map(type);
            }
        }
    }

    //########################################################################
    // Private Class - Tree transformer

    /** The tree transformer */
    private final GenTransformer treeTransformer = new GenTransformer(global) {

        /** The current context */
        private Context context;

        /**  True if currently in a method */
        public boolean inMethod;

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
                return gen.ClassDef(clasz, parents, impl.symbol(), body);

            case DefDef(_, _, _, _, _, Tree rhs):
                Symbol method = tree.symbol();
                Context backup = context;
                if (method.isConstructor())
                    context = context.getConstructorContext(method);
                else
                    inMethod = true;
                rhs = transform(rhs);
                inMethod = false;
                context = backup;
                return gen.DefDef(method, rhs);

                // !!!
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
//                 Symbol clasz = tree.symbol();
//                 return clasz.isRoot() ? tree : genOuterRef(tree.pos, clasz);

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
                    if (clasz != context.clasz || inMethod) {
                        Tree qualifier = genOuterRef(tree.pos, clasz);
                        return gen.Select(qualifier, symbol);
                    }
                }
                return gen.Ident(tree.pos, symbol);

            case TypeTerm():
                Type type = context.context.transformer.apply(tree.getType());
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
            vargs = transform(vargs);
            switch (transform(tree)) {
            case Select(Tree qualifier, _):
                if (getTypeContextFor(symbol).vlink != null) {
                    vargs = Tree.cloneArray(1, vargs);
                    vargs[0] = qualifier;

                    Type prefix;
                    // !!! this is done to avoid types like "vlink.type"
                    switch (tree) {
                    case Select(Tree qualifier1, _):
                        prefix = qualifier1.getType();
                        break;
                    default:
                        throw Debug.abort("illegal case", tree);
                    }
                    Type[] newtargs = getNewArgsOf(context.context, prefix, symbol, Tree.typeOf(targs));
                    targs = Tree.cloneArray(newtargs.length - targs.length, targs);
                    for (int i = 0; i < newtargs.length; i++)
                        targs[i] = gen.mkType(tapply.pos, newtargs[i]);

                }
            }
            targs = transform(targs);
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
                Tree[] vargs = gen.mkLocalRefs(pos, forward.nextValueParams());
                Tree fun = gen.Select(gen.Super(pos, context.clasz), method);
                trees[i] = gen.DefDef(forward, gen.mkApplyTV(fun,targs,vargs));
            }
            return trees;
        }

        /** Returns a tree referencing the given outer class. */
        private Tree genOuterRef(int pos, Symbol clasz) {
            if (context.clasz == clasz) return gen.This(pos, clasz);
            TypeContext tcontext = null;
            for (int i = 0; i < context.context.outers.length; i++)
                if (context.context.outers[i].clasz == clasz)
                    tcontext = context.context.outers[i];
            assert tcontext != null: Debug.show(clasz, " -- ", context.clasz);
            if (tcontext.isStable) {
                if (!clasz.owner().isPackage()) {
                    Tree qualifier = genOuterRef(pos,tcontext.outers[0].clasz);
                    return gen.Select(pos, qualifier, clasz.module());
                } else {
                    assert clasz.owner().isPackage(): Debug.show(clasz);
                    return gen.Ident(pos, clasz.module());
                }
            } else {
                assert context.context.vlink != null:
                    Debug.show(clasz, " -- ", context.clasz);
                Tree tree = inMethod
                    ? gen.Select(gen.This(pos, context.clasz), context.context.vlink)
                    : gen.Ident(pos, context.context.vlink);
                Context context = this.context;
                while (true) {
                    context = context.outer;
                    assert context != null:
                        Debug.show(clasz, " -- ", this.context.clasz);
                    while (context.context.isStable) {
                        context = context.outer;
                        assert context != null:
                            Debug.show(clasz, " -- ", this.context.clasz);
                    }
                    if (context.clasz == clasz) return tree;
                    tree = gen.Select(tree, context.context.vlink);
                }
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

        public final TypeContext context;

        /** Initializes this instance. */
        public Context(Context outer, Symbol symbol, Map supers) {
            this.context = getTypeContextFor(symbol);
            this.outer = outer;
            this.clasz = symbol.constructorClass();
            this.supers = supers;
        }

        /** Returns a context for the given constructor. */
        public Context getConstructorContext(Symbol constructor) {
            assert constructor.constructorClass() == clasz;
            return new Context(outer, constructor, supers);
        }

    }

    //########################################################################
}
