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
import scalac.ast.Tree.Ident;
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
    public void apply(CompilationUnit[] units) {
        treeTransformer.apply(units);
    }

    private boolean show = false; // !!!

    /** Applies this phase to the given type for the given symbol. */
    public Type transformInfo(Symbol symbol, Type type) {
        if (show && !symbol.isPackageClass()) System.out.println("!!! <<< transformInfo - symbol: " + Debug.show(symbol));
        if (show && !symbol.isPackageClass()) System.out.println("!!! <<< transformInfo - type  : " + Debug.show(type));
        if (symbol.isPackage()) return type; // !!!
        if (symbol.isPackageClass()) return type; // !!!
        if (symbol.isConstructor() && symbol.constructorClass().isPackageClass()) return type; // !!!
        TypeContext context = getTypeContextFor(symbol);
        if (symbol.isConstructor() && symbol.constructorClass().isClassType() && !symbol.constructorClass().isCompoundSym()) { // !!! isClassType -> isClass ?
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
                    Debug.show(symbol, clasz, self);
            type = Type.MethodType(vparams, result);
            if (tparams.length != 0) type = Type.PolyType(tparams, type);
        } else {
            Type t = type;
            type = context.transformer.apply(type);
            assert type != null: Debug.show(symbol) + " -- " + t;
        }
        if (show && !symbol.isPackageClass()) System.out.println("!!! >>> transformInfo - symbol: " + Debug.show(symbol));
        if (show && !symbol.isPackageClass()) System.out.println("!!! >>> transformInfo - type  : " + Debug.show(type));
        return type;
    }

    //########################################################################
    // Private Methods

    /** Returns the type context for the given symbol. */
    private TypeContext getTypeContextFor(Symbol symbol) {
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
        if (symbol.constructorClass().isPackageClass()) return new TypeContext(null, null, null, null, null, Collections.EMPTY_MAP); // !!!
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

        Map/*<Symbol,Type>*/ tparams = new HashMap();
        tparams.put(clasz, Type.ThisType(clasz));

        // get outer contexts
        TypeContext outer = clasz.owner().isPackageClass()
            ? null
            : getTypeContextFor(clasz.owner());

        // create outer type links
        Symbol[] tlinks = new Symbol[outer == null ? 0 : outer.depth + 1];
        int tflags = Modifiers.PARAM | Modifiers.COVARIANT | Modifiers.SYNTHETIC | Modifiers.STABLE;
        for (TypeContext o = outer; o != null; o = o.outer) {
            Name tname = Names.OUTER(constructor, o.clasz);
            tlinks[o.depth] = constructor.newTParam(
                constructor.pos, tflags, tname, o.clasz.typeOfThis());
            tparams.put(o.clasz, tlinks[o.depth].type());
        }

        // create outer value link
        Symbol vlink = null;
        if (outer != null) {
            int vflags = Modifiers.SYNTHETIC;
            Name vname = Names.OUTER(constructor);
            vlink = constructor.newVParam(constructor.pos, vflags, vname);
            vlink.setInfo(outer.clasz.thisType());
        }

        // create new type parameters
        for (TypeContext o = outer; o != null; o = o.outer) {
            Symbol[] oldtparams = o.oldtparams;
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

        return new TypeContext(clasz, outer, tlinks, vlink, constructor.typeParams(), tparams);
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

    /** Tests whether the class is enclosed in the specified class. */
    private static boolean isEnclosedIn(Symbol clasz, Symbol outer) {
        for (; !clasz.isRoot(); clasz = clasz.owner())
            if (clasz == outer) return true;
        return false;
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
    // Private Class - Type transformer context

    private class TypeContext {

        /** The context class */
        private final Symbol clasz;
        /** The outer context */
        private final TypeContext outer;
        /** The outer type links (null for stable outer contexts) */
        private final Symbol[] tlinks;
        /** The outer value link (null if all outer contexts are stable) */
        private final Symbol vlink;

        private final int depth;

        /** The old type parameters of the context class */
        private Symbol[] oldtparams;


        /** !!! */
        private TypeTransformer transformer; // !!! type

        /** !!! */
        public TypeContext(Symbol clasz, TypeContext outer, Symbol[] tlinks, Symbol vlink, Symbol[] oldtparams, Map tparams) {
            this.clasz = clasz;
            this.outer = outer;
            this.tlinks = tlinks;
            this.vlink = vlink;
            this.depth = outer == null ? 0 : outer.depth + 1;
            this.oldtparams = oldtparams;
            this.transformer = new TypeTransformer(tparams);
        }

        /** !!! */
        public Type getTypeLink(int level) {
            assert tlinks[level] != null: level + " - " + Debug.show(clasz); // !!! useless ?
            return tlinks[level].type();
        }

        public String toString() {
            return
                "\nclasz    = " + Debug.show(clasz) +
                "\ntlinks   = " + Debug.show(tlinks) +
                "\nvlink    = " + Debug.show(vlink) +
                "\noldparams= " + Debug.show(oldtparams) +
                "\ntparams  = " + Debug.show(transformer.tparams) +
                "\nouter    : " + "\n" + outer;
        }

    }

    //########################################################################
    // Private Class - Type transformer

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

        /** Transforms the given type. */
        public Type transform(Type type) {
            return context.context.transformer.apply(type);
        }

        /** Transforms the given tree. */
        public Tree transform(Tree tree) {
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
                        gen.Ident(context.vfield.pos, context.context.vlink));
                }
                context = context.outer;
                return gen.ClassDef(clasz, parents, impl.symbol(), body);

            case DefDef(_, _, _, _, _, Tree rhs):
                Symbol method = tree.symbol();
                Context backup = context;
                if (method.isConstructor())
                    context = context.getConstructorContext(method);
                context.method = method;
                rhs = transform(rhs);
                context.method = null;
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
                    if (clasz != context.clasz) {
                        Tree qualifier = genOuterRef(tree.pos, clasz);
                        return gen.Select(qualifier, symbol);
                    }
                }
                return gen.Ident(tree.pos, symbol);

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
            TypeContext tcontext = null;
            for (TypeContext o = context.context; o != null; o = o.outer)
                if (o.clasz == clasz)
                    tcontext = o;
            assert tcontext != null: Debug.show(clasz, context.clasz);
            { // !!!
                assert context.context.vlink != null:
                    Debug.show(clasz, context.clasz);
                Tree tree = context.method == null || context.method.isConstructor()
                    ? gen.Ident(pos, context.context.vlink)
                    : gen.Select(gen.This(pos, context.clasz), context.getVField());
                Context context = this.context;
                while (true) {
                    context = context.outer;
                    assert context != null:
                        Debug.show(clasz, this.context.clasz);
                    if (context.clasz == clasz) return tree;
                    Symbol access = getAccessSymbol(context.getVField(), null);
                    assert access != context.getVField(): Debug.show(access) + " - " + Debug.show(this.context.clasz);
                    tree = gen.Apply(gen.Select(tree, access));
                }
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
        /** The current class symbol */
        public final Symbol clasz;
        /** The self access methods (maps members to accessors) */
        public final Map/*<Symbol,Symbol>*/ selfs;
        /** The super access methods (maps members to accessors) */
        public final Map/*<Symbol,Symbol>*/ supers;

        // !!!
        public final TypeContext context;

        /** The outer value field (null if not yet created) */
        private Symbol vfield;

        /** !!! The current method */
        public Symbol method;

        /** Initializes this instance. */
        public Context(Context outer, Symbol symbol, Map selfs, Map supers){
            this.context = getTypeContextFor(symbol);
            this.outer = outer;
            this.clasz = symbol.constructorClass();
            this.selfs = selfs;
            this.supers = supers;
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
