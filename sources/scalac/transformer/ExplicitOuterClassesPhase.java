/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.transformer;

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;
import scalac.ast.GenTransformer;
import scalac.ast.Tree;
import scalac.ast.Tree.Template;
import scalac.symtab.Modifiers;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
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

    /** A map from constructor symbols to type transformers */
    private final Map/*<Symbol,TypeTransformer>*/ transformers = new HashMap();

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ExplicitOuterClassesPhase(Global global,PhaseDescriptor descriptor){
        super(global, descriptor);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation unit. */
    public void apply(CompilationUnit unit) {
        treeTransformer.apply(unit);
    }

    /** Applies this phase to the given type for the given symbol. */
    public Type transformInfo(Symbol symbol, Type type) {
        if (symbol.isConstructor()) {
            Symbol clasz = symbol.constructorClass();
            if (clasz.isClass() && !clasz.isCompoundSym())
                return transformInfo(clasz, symbol, type);
        }
        return getTypeTransformerFor(symbol).apply(type);
    }

    //########################################################################
    // Private Methods

    /**
     * Computes and returns the new type of the constructor. As a side
     * effect, creates and stores the type transformer corresponding
     * to this constructor.
     */
    private Type transformInfo(Symbol clasz, Symbol constructor, Type type) {
        Symbol[] tparams = type.typeParams();
        Symbol[] vparams = type.valueParams();
        int depth = getClassDepth(clasz);

        Map/*<Symbol,Type>*/ table = new HashMap();
        table.put(clasz, clasz.thisType());
        for (int i = 0; i < tparams.length; i++)
            table.put(tparams[i], tparams[i].type());

        Symbol[] owners = new Symbol[depth];
        Symbol[][] tparamss = new Symbol[depth][];

        Symbol vlink = null;
        if (depth > 0) {
            int count = depth;
            Symbol owner = clasz.owner();
            for (int i = depth - 1; i >= 0; i--) {
                owners[i] = owner;
                tparamss[i] = owner.typeParams();
                count += tparamss[i].length;
                owner = owner.owner();
            }

            // create outer value link
            vparams = Symbol.cloneArray(1, vparams);
            int vflags = Modifiers.SYNTHETIC;
            Name vname = Names.OUTER(constructor);
            vlink = constructor.newVParam(constructor.pos, vflags, vname);
            vlink.setInfo(clasz.owner().thisType());
            vparams[0] = vlink;

            int o = 0;
            tparams = Symbol.cloneArray(count, tparams);
            for (int i = 0; i < depth; i++) {
                // create new type parameters
                for (int j = 0; j < tparamss[i].length; j++) {
                    Symbol oldtparam = tparamss[i][j];
                    Symbol newtparam = oldtparam.cloneSymbol(constructor);
                    newtparam.name = Names.OUTER(constructor, oldtparam);
                    table.put(oldtparam, newtparam.type());
                    tparams[o++] = newtparam;
                }
                // create outer type links
                int tflags = Modifiers.PARAM | Modifiers.COVARIANT | Modifiers.SYNTHETIC | Modifiers.STABLE;
                Name tname = Names.OUTER(constructor, owners[i]);
                Symbol tlink = constructor.newTParam(
                    constructor.pos, tflags, tname, owners[i].typeOfThis());
                table.put(owners[i], tlink.type());
                tparams[o++] = tlink;
            }

        }

        transformers.put(constructor, new TypeTransformer(table));
        type = Type.typeRef(Type.NoPrefix, clasz, Symbol.type(tparams));
        type = Type.MethodType(vparams, type);
        if (tparams.length > 0) type = Type.PolyType(tparams, type);
        return type;
    }

    /** Returns the type transformer for the given symbol. */
    private TypeTransformer getTypeTransformerFor(Symbol symbol) {
        while (true) {
            Symbol test = symbol;
            if (test.isConstructor()) test = test.constructorClass();
            // !!! isClassType -> isClass ?
            if (test.isClassType() && !test.isCompoundSym()) break;
            symbol = symbol.owner();
        }
// !!!
//         while (!symbol.isClassType() && !(symbol.isConstructor() && symbol.constructorClass().isClassType())) // !!! isClassType -> isClass ?
//             symbol = symbol.owner();
        if (symbol.isClassType())
            symbol = symbol.primaryConstructor();
        if (symbol.constructorClass().isPackageClass())
            return topLevelTypeTransformer;
        TypeTransformer context = (TypeTransformer)transformers.get(symbol);
        if (context == null) {
            symbol.nextInfo();
            context = (TypeTransformer)transformers.get(symbol);
            assert context != null: Debug.show(symbol);
        }
        return context;
    }

    //########################################################################
    // Private Functions

    /**
     * Returns the depth of the specified class. The depth of a class
     * is:
     * - -1 for a package class
     * - 0 for a top-level class
     * - the depth of the enclosing class plus 1 for an inner class
     */
    private static int getClassDepth(Symbol clasz) {
        assert clasz.isClass() || clasz.isPackageClass(): Debug.show(clasz);
        int depth = -1;
        while (!clasz.isPackageClass()) { clasz = clasz.owner(); depth++; }
        return depth;
    }

    /**
     * Returns the type arguments of the flattened version of the
     * specified type reference. This functions takes and returns
     * non-transformed types.
     */
    private static Type[] getFlatArgs(Type prefix, Symbol clasz, Type[] args) {
        int depth = getClassDepth(clasz);
        if (depth <= 0) return args;
        Type[] prefixes = new Type[depth];
        Type[][] argss = new Type[depth][];
        int count = collect(prefix, clasz, prefixes, argss);
        args = Type.cloneArray(count, args);
        for (int i = 0, o = 0; i < depth; i++) {
            for (int j = 0; j < argss[i].length; j++)
                args[o++] = argss[i][j];
            args[o++] = prefixes[i];
        }
        return args;
    }
    // where
    private static int collect(Type prefix, Symbol clasz, Type[] prefixes,
        Type[][] argss)
    {
        int count = prefixes.length;
        for (int i = prefixes.length - 1; i >= 0; i--) {
            prefixes[i] = prefix;
            Symbol owner = clasz.owner();
            Type base = prefix.baseType(owner);
            switch (base) {
            case TypeRef(Type type, Symbol symbol, Type[] args):
                assert symbol == owner: Debug.show(base);
                count += args.length;
                argss[i] = args;
                prefix = type;
                clasz = owner;
                continue;
            default:
                throw Debug.abortIllegalCase(base);
            }
        }
        return count;
    }

    //########################################################################
    // Private Class - Type transformer

    /** The type transformer for top-level types */
    private static final TypeTransformer topLevelTypeTransformer =
        new TypeTransformer(Collections.EMPTY_MAP);

    /** The type transformer */
    private static final class TypeTransformer extends Type.MapOnlyTypes {

        private final Map/*<Symbol,Type>*/ tparams;

        public TypeTransformer(Map tparams) {
            this.tparams = tparams;
        }

        public Type apply(Type type) {
            switch (type) {
            case TypeRef(Type prefix, Symbol symbol, Type[] args):
                if (symbol.isParameter() && symbol.owner().isConstructor()) {
                    assert prefix == Type.NoPrefix: type;
                    assert args.length == 0: type;
                    Object value = tparams.get(symbol);
                    return value != null ? (Type)value : type;
                }
                if (symbol.isClass() && !symbol.isCompoundSym()) {
                    args = map(getFlatArgs(prefix, symbol, args));
                    prefix = Type.NoPrefix;
                    return Type.typeRef(prefix, symbol, args);
                }
                if (symbol.isPackageClass()) {
                    args = Type.EMPTY_ARRAY;
                    prefix = Type.NoPrefix;
                    return Type.typeRef(prefix, symbol, args);
                }
                return Type.typeRef(apply(prefix), symbol, map(args));
            case SingleType(Type prefix, Symbol symbol):
                if (symbol.owner().isPackageClass())
                    return Type.singleType(Type.NoPrefix, symbol);
                return Type.singleType(apply(prefix), symbol);
            case ThisType(Symbol clasz):
                Object value = tparams.get(clasz);
                if (value != null) return (Type)value;
                assert clasz.isCompoundSym() || clasz.isPackageClass():
                    Debug.show(clasz);
                return type;
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

        /** The current method */
        private Symbol method;

        /** Transforms the given type. */
        public Type transform(Type type) {
            return context.transformer.apply(type);
        }

        /** Transforms the given tree. */
        public Tree transform(Tree tree) {
            if (global.debug) global.log("transforming " + tree);//debug
            switch (tree) {

            case ClassDef(_, _, _, _, _, Template impl):
                Symbol clasz = tree.symbol();
                context = new Context(context, clasz, new HashMap(), new HashMap());
                Tree[] parents = transform(impl.parents);
                Tree[] body = transform(impl.body);
                body = Tree.concat(body, genAccessMethods(false));
                body = Tree.concat(body, genAccessMethods(true));
                if (context.vfield != null) {
                    body = Tree.cloneArray(1, body);
                    body[0] = gen.ValDef(
                        context.vfield,
                        gen.Ident(context.vfield.pos, context.vparam));
                }
                context = context.outer;
                return gen.ClassDef(clasz, parents, impl.symbol(), body);

            case DefDef(_, _, _, _, _, Tree rhs):
                Symbol method = tree.symbol();
                Context backup = context;
                if (method.isConstructor())
                    context = context.getConstructorContext(method);
                this.method = method;
                rhs = transform(rhs);
                this.method = null;
                context = backup;
                return gen.DefDef(method, rhs);

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
                if (symbol.owner().isStaticOwner()) // !!! qualifier ignored
                    return gen.mkGlobalRef(tree.pos, symbol);
                Symbol access;
                switch (qualifier) {
                case Super(_, _):
                    Symbol clasz = qualifier.symbol();
                    if (clasz == context.clasz) {
                        access = symbol;
                        qualifier = gen.Super(tree.pos, qualifier.symbol());
                    } else {
                        access = getAccessSymbol(symbol, clasz);
                        qualifier = genOuterRef(qualifier.pos, clasz);
                    }
                    break;
                default:
                    access = getAccessSymbol(symbol, null);
                    qualifier = transform(qualifier);
                    break;
                }
                tree = gen.Select(tree.pos, qualifier, access);
                if (access != symbol && !symbol.isMethod())
                    tree = gen.mkApply__(tree);
                return tree;

            default:
                return super.transform(tree);
            }
        }

        /* Add outer type and value arguments to constructor calls. */
        private Tree transform(Tree vapply, Tree[] vargs, Tree tapply,
            Tree[] targs, Tree tree)
        {
            switch (tree) {
            case Select(Tree qualifier, _):
                Symbol symbol = tree.symbol();
                Symbol clasz = symbol.constructorClass();
                if (getClassDepth(clasz) > 0) {
                    Type[] types = Tree.typeOf(targs);
                    types = getFlatArgs(qualifier.type(), clasz, types);
                    targs = gen.mkTypes(tapply.pos, types);
                    vargs = Tree.cloneArray(1, vargs);
                    vargs[0] = qualifier;
                } else {
                    assert !containsValue(qualifier): tree;
                }
                tree = gen.Ident(tree.pos, symbol);
                if (targs.length != 0)
                    tree = gen.TypeApply(tapply.pos, tree, transform(targs));
                return gen.Apply(vapply.pos, tree, transform(vargs));
            default:
                throw Debug.abortIllegalCase(tree);
            }
        }

        /**
         * Returns the symbol to access the specified member from the
         * current context. If "svper" is non null, the member is
         * selected from the superclass of the specified class. The
         * returned symbol may be the same as the given one.
         */
        private Symbol getAccessSymbol(Symbol member, Symbol svper) {
            if (member.isPublic() && svper == null) return member;
            Context context = this.context;
            for (; context != null; context = context.outer)
                if (svper != null
                    ? context.clasz == svper
                    : member.isPrivate()
                    ? context.clasz == member.owner()
                    // !!! This is incorrect without static access methods
                    : context.clasz.isSubClass(member.owner())) break;
            assert context != null: Debug.show(this.context, member);
            if (context == this.context) return member;
            Map table = svper != null ? context.supers : context.selfs;
            Symbol access = (Symbol)table.get(member);
            if (access == null) {
                // !!! generate static access methods ?
                Name name = Names.ACCESS(member, svper != null);
                access = context.clasz.newAccessMethod(context.clasz.pos,name);
                global.nextPhase();
                Type info = member.isMethod()
                    ? member.info().cloneType(member, access)
                    : Type.MethodType(Symbol.EMPTY_ARRAY, member.info());
                access.setInfo(info);
                global.prevPhase();
                table.put(member, access);
                context.clasz.nextInfo().members().enter(access);
                assert Debug.log("created access method: ", access);
            }
            return access;
        }

        /** Generates the trees of the access methods. */
        private Tree[] genAccessMethods(boolean withSuper) {
            Map table = withSuper ? context.supers : context.selfs;
            if (table.size() == 0) return Tree.EMPTY_ARRAY;
            Tree[] trees = new Tree[table.size()];
            Iterator entries = table.entrySet().iterator();
            for (int i = 0; i < trees.length; i++) {
                Map.Entry entry = (Map.Entry)entries.next();
                Symbol member = (Symbol)entry.getKey();
                Symbol access = (Symbol)entry.getValue();
                int pos = access.pos;
                Tree qualifier = withSuper
                    ? gen.Super(pos, context.clasz)
                    : gen.This(pos, context.clasz);
                Tree select = gen.Select(qualifier, member);
                Tree[] targs = gen.mkTypeRefs(pos, access.nextTypeParams());
                Tree[] vargs = gen.mkLocalRefs(pos, access.nextValueParams());
                Tree body = member.isMethod()
                    ? gen.mkApplyTV(select, targs, vargs)
                    : select;
                trees[i] = gen.DefDef(access, body);
            }
            return trees;
        }

        /** Returns a tree referencing the given outer class. */
        private Tree genOuterRef(int pos, Symbol clasz) {
            if (context.clasz == clasz) return gen.This(pos, clasz);
            Tree tree = method == null || method.isConstructor()
                ? gen.Ident(pos, context.vparam)
                : gen.Select(gen.This(pos, context.clasz),context.getVField());
            for (Context c = context.outer;; c = c.outer) {
                assert c != null: Debug.show(clasz, context.clasz);
                if (c.clasz == clasz) return tree;
                Symbol access = getAccessSymbol(c.getVField(), null);
                tree = gen.Apply(gen.Select(tree, access));
            }
        }

        /** Tests whether the tree contains some value computation. */
        private boolean containsValue(Tree tree) {
            switch (tree) {
            case This(_):
                return !tree.symbol().isPackageClass();
            case Select(Tree qualifier, _):
                return containsValue(qualifier) || tree.symbol().isValue();
            default:
                return false;
            }
        }

    };

    //########################################################################
    // Private Class - Tree transformer context

    /** This class represents the tree transformation context. */
    private class Context {

        /** The outer context */
        public final Context outer;
        /** The context class */
        public final Symbol clasz;
        /** The context type transformer */
        public final TypeTransformer transformer;
        /** The self access methods (maps members to accessors) */
        public final Map/*<Symbol,Symbol>*/ selfs;
        /** The super access methods (maps members to accessors) */
        public final Map/*<Symbol,Symbol>*/ supers;
        /** The context outer paramater (null if none) */
        public final Symbol vparam;
        /** The context outer field (null if none or not yet used) */
        private Symbol vfield;

        /** Initializes this instance. */
        public Context(Context outer, Symbol symbol, Map selfs, Map supers) {
            this.outer = outer;
            this.clasz = symbol.constructorClass();
            this.transformer = getTypeTransformerFor(symbol);
            this.selfs = selfs;
            this.supers = supers;
            this.vparam = outer != null ? symbol.nextValueParams()[0] : null;
        }

        /** Returns a context for the given constructor. */
        public Context getConstructorContext(Symbol constructor) {
            assert constructor.constructorClass() == clasz;
            return new Context(outer, constructor, selfs, supers);
        }

        /**
         * Returns the outer value field. The field is created on the
         * fly if it does not yet exist.
         */
        private Symbol getVField() {
            assert outer != null: Debug.show(clasz);
            if (vfield == null) {
                int flags =
                    Modifiers.SYNTHETIC | Modifiers.PRIVATE | Modifiers.STABLE;
                vfield = clasz.newField(clasz.pos, flags, Names.OUTER(clasz));
                vfield.setInfo(outer.clasz.thisType());
                clasz.members().enterNoHide(vfield);
            }
            return vfield;
        }

    }

    //########################################################################
}
