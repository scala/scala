/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ExpandMixins.java,v 1.24 2002/11/11 16:08:50 schinz Exp $
// $Id$

package scalac.transformer;

import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Iterator;

import scalac.Global;
import scalac.ast.Tree;
import scalac.ast.Tree.Template;
import scalac.ast.TreeList;
import scalac.ast.TreeGen;
import scalac.ast.GenTreeCloner;
import scalac.ast.TreeSymbolCloner;
import scalac.ast.Transformer;
import scalac.symtab.Modifiers;
import scalac.symtab.Scope;
import scalac.symtab.Scope.SymbolIterator;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolCloner;
import scalac.symtab.SymbolSubstTypeMap;
import scalac.symtab.Type;
import scalac.util.Name;
import scalac.util.Debug;

public class ClassExpander {

    //########################################################################
    // Private Fields

    /** The global environment */
    private final Global global;

    /** A tree generator */
    private final TreeGen gen;

    /** The expanding class */
    private final Symbol clasz;

    /** The parents of the expanding class */
    private final Type[] parents;

    /** The members of the expanding class */
    private final Scope members;

    /** The template of the expanding class */
    private final Template template;

    /** The body of the expanding class */
    private final LinkedList/*<Tree>*/ body;

    /** The type map to apply to inlined symbols and trees */
    private final SymbolSubstTypeMap map;

    /** The symbol cloner to clone inlined symbols */
    private final SymbolCloner cloner;

    /** The state of this class (used to prevent misuses) */
    private int state;

    //########################################################################
    // Public Constructors

    public ClassExpander(Global global, Symbol clasz, Template template) {
        this.global = global;
        this.gen = global.treeGen;
        this.clasz = clasz;
        this.parents = Type.cloneArray(clasz.parents());
        this.members = clasz.members(); // !!! .cloneScope();
        this.template = gen.Template(template.pos, template.symbol(),
            Tree.cloneArray(template.parents), template.body);
        this.body = new LinkedList();
        this.map = new SymbolSubstTypeMap();
        this.cloner = new SymbolCloner(
            global.freshNameCreator, new HashMap(), map.getSymbols());
        this.state = parents.length;

        this.mixinMemberCloner = new MixinMemberCloner(this);
        this.superFixer = new SuperFixer(global);
    }

    //########################################################################
    // Public Methods

    // !!! remove this.parents
    /** Inlines the ith mixin with given type, interface and body. */
    public void inlineMixin(int i, Type type, Symbol iface, Template impl) {
        assert 0 < i && i < state : "state = " + state + ", i = " + i;
        switch (parents[i]) {
        case TypeRef(Type prefix, Symbol mixin, Type[] args):
            // relpace "This/Super(mixin)" by "This/Super(clasz)"
            map.insertSymbol(mixin, clasz);
            // owner of inlined value parameters and constructor is "clasz"
            cloner.owners.put(mixin.primaryConstructor(), clasz);
            // map mixin type parameters to mixin type arguments
            map.insertType(type.symbol().typeParams(), type.typeArgs());
            Tree.Apply constr = (Tree.Apply)template.parents[i];
            Symbol[] vparams = mixin.valueParams();
            inlineMixinVParams(vparams, constr.args, 0);
            createMixedInMemberSymbols(mixin.nextInfo().members());
            inlineMixedInCode(impl, vparams.length);
            parents[i] = Type.TypeRef(prefix, iface, args);
            template.parents[i] = gen.mkPrimaryConstr(constr.pos, parents[i]);
            state = i;
            return;
        default:
            throw Debug.abort("invalid base class type", parents[i]);
        }
    }

    public Template getTemplate() {
        assert 0 < state : "state = " + state;
        Transformer superFixer = new Transformer(global) {
            public Tree transform(Tree tree) {
                switch (tree) {
                case Select(Super(_, _), _):
                    Symbol symbol = map.lookupSymbol(tree.symbol());
                    if (symbol != null) {
                        Tree qualifier = gen.This(tree.pos, clasz);
                assert qualifier.type().baseType(symbol.owner()) != Type.NoType: tree; // !!!
                        return gen.Select(qualifier, symbol);
                    }
                }
                return super.transform(tree);
            }
        };
        body.addAll(Arrays.asList(this.superFixer.transform(
superFixer.transform(template.body))));
        template.body = (Tree[])body.toArray(new Tree[body.size()]);
        // !!! *1 fix ExpandMixinsPhase.transformInfo and remove next line
        state = 0;
        return template;
    }

    //########################################################################
    // Private Methods

    private void inlineMixinVParams(Symbol[] params, Tree[] args, int fstPos) {
        for (int i = 0; i < params.length; i++) {
            Symbol member = cloner.cloneSymbol(params[i], true);
            member.flags &= ~Modifiers.PARAM;
            member.flags |= Modifiers.PRIVATE;
            members.enter(member);
        }
        // !!! remove double loop
        // We need two loops because parameters may appear in their types.
        for (int i = 0; i < params.length; i++) {
            Symbol member = map.lookupSymbol(params[i]);
            member.setType(map.apply(member.type()));
            body.add(fstPos + i, gen.ValDef(member, args[i]));
        }
    }

    private void createMixedInMemberSymbols(Scope symbols) {
        // The map names is used to implement an all or nothing
        // strategy for overloaded symbols.
        Map/*<Name,Name>*/ names = new HashMap();
        for (SymbolIterator i = symbols.iterator(true); i.hasNext();) {
            Symbol member = i.next();
            Name name = (Name)names.get(member.name);
            boolean shadowed = name == null &&
                members.lookup(member.name) != Symbol.NONE;
            Symbol clone = cloner.cloneSymbol(member, shadowed);
            if (name != null)
                clone.name = name;
            else
                names.put(member.name, clone.name);
            if (clone.name != member.name) clone.flags &= ~Modifiers.OVERRIDE;
            clone.setType(clone.type().cloneTypeNoSubst(cloner));
            members.enterOrOverload(clone);
        }
        // We need two loops because members may appear in their types.
        for (SymbolIterator i = symbols.iterator(true); i.hasNext();) {
            Symbol member = map.lookupSymbol(i.next());
            if (member == null) continue;
            member.setType(map.applyParams(member.type()));
        }
    }

    private void inlineMixedInCode(Template mixin, int fstPos) {
        cloner.owners.put(mixin.symbol(), template.symbol());
        int pos = 0;
        for (int i = 0; i < mixin.body.length; i++) {
            Tree tree = mixin.body[i];
            // Inline local code and members whose symbol has been cloned.
            if (!tree.definesSymbol() ||
                map.lookupSymbol(tree.symbol()) != null) {
                body.add(fstPos + pos, mixinMemberCloner.transform(tree));
                ++pos;
            }
        }
    }

    private final GenTreeCloner mixinMemberCloner;

    private class MixinMemberCloner extends GenTreeCloner {
        public MixinMemberCloner(ClassExpander expander) {
            super(expander.global, expander.map, expander.cloner);
        }

        public Symbol getSymbolFor(Tree tree) {
            switch (tree) {
            case Select(Super(_, _), _):
                // !!! check
                global.nextPhase();
                Symbol symbol = tree.symbol().overridingSymbol(parents[0]);
                global.prevPhase();
                assert !symbol.isNone(): tree;
                return symbol;
            case Super(_, _):
            case This(_):
                return clasz;
            default:
                return super.getSymbolFor(tree);
            }
        }

    }

    private final Transformer superFixer;

    private class SuperFixer extends Transformer {
        private final Type parent;

        public SuperFixer(Global global) {
            super(global);
            this.parent = clasz.parents()[0];
        }
        public Tree transform(Tree tree) {
            if (tree.definesSymbol() && tree.symbol().owner().isClass())
                return tree;
            switch (tree) {
            case Select(Super(_, _), _):
                Tree qualifier = ((Tree.Select)tree).qualifier;
                qualifier = gen.Super(qualifier.pos, clasz);
                Symbol symbol = tree.symbol().overridingSymbol(parent);
                assert !symbol.isNone(): tree + " -- " + parent  + " -- " + Debug.show(clasz.parents()) + " -- " + Debug.show(clasz);
                return gen.Select(tree.pos, qualifier, symbol);
            default:
                return super.transform(tree);
            }
        }
    }

    //########################################################################
}
