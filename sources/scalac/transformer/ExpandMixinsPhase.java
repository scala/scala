/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ExpandMixinsPhase.java,v 1.8 2002/05/02 10:59:35 schinz Exp $
// $Id$

package scalac.transformer;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.LinkedList;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.Unit;
import scalac.ast.Tree;
import scalac.ast.Tree.Template;
import scalac.ast.TreeGen;
import scalac.ast.TreeInfo;
import scalac.ast.TreeList;
import scalac.ast.Traverser;
import scalac.ast.Transformer;
import scalac.ast.GenTransformer;
import scalac.ast.GenTreeCloner;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;

import scalac.util.Name;
import scalac.util.Names;
import scalac.symtab.Modifiers;
import scalac.symtab.Scope.SymbolIterator;
import scalac.symtab.SymbolCloner;
import scalac.symtab.SymbolSubstTypeMap;

// TODO do not copy hidden members which are not accessible via
// "super".

/**
 * A phase to expand mixins using code copying. We assume that links
 * to outer classes have been made explicit by a previous phase.
 */
// !!! needs to be cleaned
public class ExpandMixinsPhase extends Phase {

    //########################################################################
    // Private Fields

    /** A map from classes to their interface */
    private final Map/*<Symbol,Symbol>*/ interfaces;

    /** A map from classes to their type transformer */
    private final Map/*<Symbol,TypeTransformer>*/ transformers;

    /** A map from classes to their expanded body */
    private final Map/*<Symbol,Tree[]>*/ expansions;

    /** A map from classes to their original (unexpanded) body */
    private final Map/*<Symbol,Tree[]>*/ bodies;


    /** A traverser that collects class definitions */
    private final Traverser collector;

    /** A transformer that expands classes that have mixins */
    private final GenTransformer expander;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ExpandMixinsPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
        Phase addinterfaces = global.PHASE.ADDINTERFACES.phase();
        this.interfaces = ((AddInterfacesPhase)addinterfaces).classToIFace;
        this.transformers = new HashMap();
        this.expansions = new HashMap();
        this.bodies = new HashMap();
        this.collector = new TreeCollector();
        this.expander = new TreeExpander(global);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given compilation units. */
    public void apply(Unit[] units) {
        collector.traverse(units);
        expander.apply(units);
    }

    /** Applies this phase to the given type for the given symbol. */
    public Type transformInfo(Symbol symbol, Type type) {
        Symbol s = symbol;
        while (true) {
            if (symbol.isJava()) return type;
            if (symbol.isPackage()) return type;
            if (symbol.isInterface()) return type;
            if (symbol.isCompoundSym()) return type; // !!! check
            if (symbol.isClass()) {
                // !!! System.out.println(Debug.show("!!! ", s, " -> ", symbol, " - ", getTypeExpander(symbol).clasz, " : " + type));
                return getTypeExpander(symbol).apply(type);
            }
            symbol = symbol.isConstructor()
                ? symbol.constructorClass()
                : symbol.owner();
        }
    }

    //########################################################################
    // Private Methods

    //########################################################################
    // Private Class - Tree collector

    /** A tree traverser that collects class definitions. */
    private class TreeCollector extends Traverser {
        public void traverse(Tree tree) {
            switch(tree) {
            case ClassDef(_, _, _, _, _, Template(_, Tree[] body)):
                Symbol clasz = tree.symbol();
                if (!clasz.isInterface()) bodies.put(clasz, body);
                traverse(body);
                return;
            case PackageDef(_, Template(_, Tree[] body)):
                traverse(body);
                return;
            }
        }
    }

    //########################################################################
    // Private Class - Tree expander

    /**
     * A tree transformer that expands class definitions and removes
     * compiled units.
     */
    private class TreeExpander extends GenTransformer {
        public TreeExpander(Global global) {
            super(global);
        }
        public void apply(Unit unit) {
            if (unit.mixinOnly) {
                assert Debug.log("removing unit " + unit);
                unit.body = Tree.EMPTY_ARRAY;
            } else {
                super.apply(unit);
            }
        }
        public Tree transform(Tree tree) {
            switch (tree) {
            case ClassDef(_, _, _, _, _, _):
                Symbol clasz = tree.symbol();
                if (clasz.isInterface()) return super.transform(tree);
                return gen.ClassDef(clasz, getExpandedBody(clasz));
            default:
                return super.transform(tree);
            }
        }
    }

    //########################################################################
    // Private Class - Tree inliner

    /**
     * A Tree cloner that clones mixin bodies. It assumes that these
     * bodies have already been expanded. References to mixin value
     * parameters are replaced by references to newly-created inlined
     * fields. The symbol of Super and This nodes is replaced by the
     * symbol of the generated class. The symbol of super members is
     * rebound if required. Note that as the input tree has already
     * been expanded, it contains only super member to the first
     * supertype of the mixin, all other super members have been
     * removed during the expansion of the input tree.
     */
    private static class TreeInliner extends GenTreeCloner {
        private final Symbol clasz;
        private final Type supertype;
        private boolean initializer;
        public TreeInliner(Global global, TypeTransformer transformer) {
            super(global, transformer, transformer.cloner);
            // !!! global.nextPhase();
            this.clasz = transformer.clasz;
            // !!! global.prevPhase();
            this.supertype = clasz.nextInfo().parents()[0];
        }
        public Symbol getSymbolFor(Tree tree) {
            switch (tree) {
            case Select(Super(_, _), _):
                if (tree.symbol().isInitializer()) return tree.symbol(); // !!!
                assert supertype.symbol().isSubClass(tree.symbol().owner()):
                    tree + " -- " + Debug.show(clasz);
                global.nextPhase();
                Symbol symbol = tree.symbol().overridingSymbol(supertype);
                global.prevPhase();
                assert !symbol.isNone(): tree + " -- " + Debug.show(clasz);
                return symbol;
            case Super(_, _):
            case This(_):
                return clasz;
            default:
                return super.getSymbolFor(tree);
            }
        }
        public Tree transform(Tree tree) {
            switch (tree) {
            case DefDef(_, _, _, _, _, _):
                if (getSymbolFor(tree).isInitializer()) initializer = true;
                tree = super.transform(tree);
                initializer = false;
                return tree;
            case Apply(Select(Super(_, _), _), _):
                if (TreeInfo.methSymbol(tree).isInitializer() && !initializer)
                    return Tree.Empty;
                break;
            }
            if (tree.hasSymbol() && tree.symbol().isParameter()) {
                Symbol symbol = getSymbolFor(tree);
                if (!symbol.isParameter()) {
                    assert tree instanceof Tree.Ident: tree;
                    return gen.Select(gen.This(tree.pos, clasz),  symbol);
                }
            }
            return super.transform(tree);
        }
    }

    //########################################################################
    // Private Class - Class tree expander

    /**
     * A tree transformer that expands a class definitions. Super
     * members are rebound and/or rewritten into normal members.
     */
    private class ClassTreeExpander extends TreeExpander {
        private final Symbol clasz;
        private final Type[] parents;
        private final Map clones;
        public ClassTreeExpander(Global global, TypeTransformer transformer) {
            super(global);
            this.clasz = transformer.clasz;
            this.parents = clasz.parents();
            this.clones = transformer.cloner.clones;
        }
        private Symbol getSuperMember(Symbol member) {
            for (int i = 0; i < parents.length; i++) {
                global.nextPhase();
                if (!parents[i].symbol().isSubClass(member.owner())) {
                    global.prevPhase(); // !!!
                    continue;
                }
                Symbol override = member.overridingSymbol(parents[i]);
                global.prevPhase();
                assert !override.isNone():
                    Debug.show(member, " -- ", clasz, " -- ", parents[i]);
                if (i == 0) return override;
                Symbol clone = (Symbol)clones.get(override);
                assert clone != null:
                    Debug.show(member, " -- ", override, " -- ", clasz);
                return clone;
            }
            // !!! double loop
            for (int i = 0; i < parents.length; i++) {
                if (!parents[i].symbol().isSubClass(member.owner())) continue;
                //global.nextPhase();
                Symbol override = member.overridingSymbol(parents[i]);
                //global.prevPhase();
                assert !override.isNone():
                    Debug.show(member, " -- ", clasz, " -- ", parents[i]);
                if (i == 0) return override;
                Symbol clone = (Symbol)clones.get(override);
                assert clone != null:
                    Debug.show(member, " -- ", override, " -- ", clasz);
                return clone;
            }
            throw Debug.abort(Debug.show(member, " -- ", clasz));
        }
        public Tree transform(Tree tree) {
            switch (tree) {
            case Select(Super(_, _), _):
                Symbol symbol = getSuperMember(tree.symbol());
                Tree qualifier = symbol.owner() == clasz
                    ? gen.This(tree.pos, clasz)
                    : gen.Super(tree.pos, clasz);
                return gen.Select(tree.pos, qualifier, symbol);
            default:
                return super.transform(tree);
            }
        }
    }















    /** Returns the type transformer to use for the given class. */
    private TypeTransformer getTypeExpander(Symbol clasz) { // !!!
        assert clasz.isClass() && !clasz.isInterface(): Debug.show(clasz);
        TypeTransformer transformer =(TypeTransformer)transformers.get(clasz);
        if (transformer == null) {
            transformer = new TypeTransformer(clasz);
            transformers.put(clasz, transformer);
        }
        return transformer;
    }

    /** Returns the expanded body of the given clasz. */
    private Tree[] getExpandedBody(Symbol clasz) {
        Tree[] body = (Tree[])expansions.get(clasz);
        if (body == null) {
            body = (Tree[])bodies.remove(clasz);
            assert body != null: Debug.show(clasz);
            TypeTransformer transformer = getTypeExpander(clasz);
            TreeList list = new TreeList();
            assert Debug.log("expanding tree ", clasz);
            Type[] parents = clasz.parents();
            TreeInliner inliner = new TreeInliner(global, transformer);
            for (int i = 1; i < parents.length; i++) {
                Symbol mixin = parents[i].symbol();
                if (mixin.isInterface()) continue;
                assert Debug.log("expanding tree ", clasz, ": inlining ", mixin);
                list.append(inliner.transform(getExpandedBody(mixin)));
            }
            ClassTreeExpander expander = new ClassTreeExpander(global, transformer);
            list.append(expander.transform(body));
            body = list.toArray();
            expansions.put(clasz, body);
        }
        return body;
    }

    //########################################################################
    // Private Class - Collector

    private class TypeTransformer extends Type.MapOnlyTypes {

        public final Symbol clasz;
        public final SymbolCloner cloner;
        public final Map/*<Symbol,Type>*/ inlines;

        public TypeTransformer(Symbol clasz) {
            this.clasz = clasz;
            this.cloner = new SymbolCloner();
            this.inlines = new HashMap();
            initialize();
        }

        private void initialize() {
            Type[] parents = clasz.parents();
            // !!! parents.length > 0
            if (parents.length > 0) parents[0].symbol().nextInfo(); // force
            assert Debug.log("expanding type ", clasz);
            for (int i = parents.length - 1; 0 < i; i--) {
                switch (parents[i]) {
                case TypeRef(Type prefix, Symbol mixin, Type[] args):
                    if (mixin.isInterface()) continue;
                    mixin.nextInfo(); // force
                    assert Debug.log("expanding type ", clasz, ": inlining ", mixin);
                    cloner.owners.put(mixin, clasz);
                    cloner.owners.put(mixin.primaryConstructor(), clasz);
                    // map mixin type parameters to mixin type arguments
                    Symbol[] tparams = mixin.typeParams();
                    for (int j = 0; j < tparams.length; j++)
                        inlines.put(tparams[j], args[j]);

                    createMixedInMemberSymbols(mixin.nextInfo().members());
                }
            }
            Set clones = new HashSet(cloner.clones.keySet());
//             cloner.clones.putAll(getTypeTransformerFor(parents[0].symbol()).cloner.clones); // !!! build closure

            // !!! remove ?
            for (Iterator i = cloner.clones.values().iterator(); i.hasNext();){
                // !!! for (Iterator i = clones.iterator(); i.hasNext();){
                Symbol clone = (Symbol)i.next();
                clone.setInfo(apply(clone.info()));
            }
        }

        private void createMixedInMemberSymbols(Scope symbols) {
            Scope scope = clasz.members();
            for (SymbolIterator i = symbols.iterator(true); i.hasNext();) {
                Symbol member = i.next();
                boolean shadowed = member.isPrivate() || member.isInitializer()
                    || member.overridingSymbol(clasz.thisType()) != member;
                assert Debug.log("expanding type ", clasz, ": cloning ", member);
                Symbol clone = cloner.cloneSymbol(member);
                if (shadowed) clone.name = Names.MIXIN(member);
                if (shadowed) clone.flags &= ~Modifiers.OVERRIDE;
                if (shadowed) clone.flags &= ~Modifiers.ACCESSFLAGS;
                if (shadowed) clone.flags |=  Modifiers.PRIVATE;
                scope.enterOrOverload(clone);
            }
        }


        public Type apply(Type type) {
            switch (type) {
            case TypeRef(Type prefix, Symbol symbol, Type[] args):
                Type inline = (Type)inlines.get(symbol);
                if (inline != null) return inline;
                return map(type);
            case SingleType(Type prefix, Symbol symbol):
                // !!! prefix = apply(prefix);
                // !!! symbol = prefix.rebind(symbol);
                // !!! commented out because of following example:
                // class Bar {
                //   val b: Bar = null;
                //   class Linker { def b: Bar.this.b.type = Bar.this.b; }
                // }
                Symbol clone = (Symbol)cloner.clones.get(symbol);
                if (clone != null) symbol = clone;
                return Type.singleType(prefix, symbol);
            case ThisType(Symbol symbol):
                if (symbol.isNone()) return type;
                return clasz.thisType();
            case CompoundType(Type[] parents, Scope members):
                if (type.symbol() != clasz) return map(type);
                if (parents.length <= 1) return type;
                Symbol iface = (Symbol)interfaces.get(clasz);
                if (iface == null) {
                    int i = 1;
                    while (i < parents.length && parents[i].symbol().isInterface()) i++;
                    if (i == parents.length) return type;
                    parents = Type.cloneArray(parents);
                    while (i < parents.length) {
                        if (!parents[i].symbol().isInterface())
                            parents[i] = Type.typeRef(parents[i].prefix(), (Symbol)interfaces.get(parents[i].symbol()), parents[i].typeArgs());
                        i++;
                    }
                    return Type.compoundType(parents, members, clasz);
                }
                assert parents[parents.length - 1].symbol() == iface: type;
                if (parents.length == 2) return type;
                for (int i = 1; i < parents.length - 1; i++) {
                    Symbol mixin = parents[i].symbol();
                    Symbol imix = mixin.isInterface()
                        ? mixin
                        : (Symbol)interfaces.get(mixin);
                    assert iface.isSubClass(imix): type+" - "+i;
                }
                parents = new Type[] {parents[0], parents[parents.length - 1]};
                return Type.compoundType(parents, members, clasz);
            default:
                return map(type);
            }
        }

    }

    //########################################################################
}
