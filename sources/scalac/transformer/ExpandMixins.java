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
import scalac.ast.TreeCloner;
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
        this.members = clasz.members().cloneScope();
        this.template = gen.Template(template.pos, template.symbol(),
            Tree.cloneArray(template.parents), template.body);
        this.body = new LinkedList();
        this.map = new SymbolSubstTypeMap();
        this.cloner = new SymbolCloner(
            global.freshNameCreator, new HashMap(), map.getSymbols());
        this.state = parents.length;
    }

    //########################################################################
    // Public Methods

    public void inlineMixin(int i, Type type, Symbol iface, Template impl) {
        assert 0 < i && i < state : "state = " + state + ", i = " + i;
        switch (parents[i]) {
        case TypeRef(Type prefix, Symbol mixin, Type[] args):
            map.insertSymbol(mixin, clasz);
            cloner.owners.put(mixin.primaryConstructor(), clasz);
            inlineMixinTParams(type);
            Tree.Apply constr = (Tree.Apply)template.parents[i];
            Symbol[] vparams = mixin.valueParams();
            inlineMixinVParams(vparams, constr.args, 0);
            handleMixinInterfaceMembers(mixin);
            inlineMixinMembers(mixin.nextInfo().members(), impl, vparams.length);
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
                    if (symbol != null)
                        return gen.Select(gen.This(tree.pos, clasz), symbol);
                }
                return super.transform(tree);
            }
        };
        body.addAll(Arrays.asList(superFixer.transform(template.body)));
        template.body = (Tree[])body.toArray(new Tree[body.size()]);
        // !!! *1 fix ExpandMixinsPhase.transformInfo and remove next line
        clasz.updateInfo(Type.compoundType(parents, members, clasz));
        state = 0;
        return template;
    }

    //########################################################################
    // Private Methods

    private void inlineMixinTParams(Type type) {
        switch (type) {
        case TypeRef(Type prefix, Symbol symbol, Type[] args):
            map.insertType(symbol.typeParams(), args);
            return;
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    private void inlineMixinVParams(Symbol[] params, Tree[] args, int fstPos) {
        for (int i = 0; i < params.length; i++) {
            Symbol member = cloner.cloneSymbol(params[i], true);
            member.flags &= ~Modifiers.PARAM;
            member.flags |= Modifiers.PRIVATE;
            members.enter(member);
        }
        // We need two loops because parameters may appear in their types.
        for (int i = 0; i < params.length; i++) {
            Symbol member = map.lookupSymbol(params[i]);
            member.setType(map.apply(member.type()));
            body.add(fstPos + i, gen.ValDef(member, args[i]));
        }
    }

    // !!! This is just rapid fix. Needs to be reviewed.
    private void handleMixinInterfaceMembers(Symbol mixin) {
        Type[] parents = mixin.info().parents();
        //assert parents.length == 2: Debug.show(mixin) +" -- "+ mixin.info();
        for (int i = 1; i < parents.length; i++)
            handleMixinInterfaceMembersRec(parents[i].symbol());
    }
    private void handleMixinInterfaceMembersRec(Symbol interfase) {
        handleMixinInterfaceMembersAux(interfase.nextInfo().members());
        Type[] parents = interfase.parents();
        for (int i = 0; i < parents.length; i++) {
            Symbol clasz = parents[i].symbol();
            if (clasz.isInterface()) handleMixinInterfaceMembersRec(clasz);
        }
    }
    private void handleMixinInterfaceMembersAux(Scope symbols) {
        for (SymbolIterator i = symbols.iterator(true); i.hasNext();) {
            Symbol member = i.next();
            if (member.kind != scalac.symtab.Kinds.TYPE) continue;
            // !!! use same trick as in erasure?
            //Symbol clone = member.cloneSymbol(clasz);
            //clone.setInfo(clasz.thisType().memberInfo(member));
            //Symbol subst = clasz.thisType().memberType(clone).symbol();
            Symbol subst = clasz.thisType().memberType(member).symbol();
            if (subst == member) continue;
            Symbol subst1 = map.lookupSymbol(member);
            assert subst1 == null || subst1 == subst:
                Debug.show(member," -> ",subst," + ",subst1);
            if (subst1 == null) map.insertSymbol(member, subst);
        }
    }

    private void inlineMixinMembers(Scope symbols, Template mixin, int fstPos) {
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
        cloner.owners.put(mixin.symbol(), template.symbol());
        final Set clones = new HashSet();
        TreeSymbolCloner mixinSymbolCloner = new TreeSymbolCloner(cloner) {
            public Symbol cloneSymbol(Symbol symbol) {
                Symbol clone = super.cloneSymbol(symbol);
                clones.add(clone);
                return clone;
            }
        };
        TreeCloner mixinTreeCloner = new TreeCloner(global, map) {
            public Tree transform(Tree tree) {
                switch (tree) {
                case New(Template template):
                    assert template.parents.length == 1 : tree;
                    assert template.body.length == 0 : tree;
                    Tree apply = template.parents[0];
                    switch (apply) {
                    case Apply(Tree clasz, Tree[] args):
                        args = transform(args);
                        apply = gen.Apply(apply.pos, clasz, args);
                        return gen.New(tree.pos, apply);
                    default:
                        throw Debug.abort("illegal case", tree);
                    }
                case Select(Super(_, _), _):
                    Symbol sym = tree.symbol();
                    Symbol newSym = sym.overridingSymbol(parents[0]);
                    if (newSym != Symbol.NONE)
                        return gen.Select(tree.pos,
                                          gen.Super(tree.pos, newSym.owner()),
                                          newSym);
                    else
                        return super.transform(tree);
                default:
                    return super.transform(tree);
                }
            }
        };
        int pos = 0;
        for (int i = 0; i < mixin.body.length; i++) {
            Tree tree = mixin.body[i];
            // Inline local code and members whose symbol has been cloned.
            if (!tree.definesSymbol() ||
                map.lookupSymbol(tree.symbol()) != null) {
                mixinSymbolCloner.traverse(tree);
                for (Iterator j = clones.iterator(); j.hasNext();) {
                    Symbol clone = (Symbol)j.next();
                    clone.setType(map.apply(clone.type()));
                }
                clones.clear();
                body.add(fstPos + pos, mixinTreeCloner.transform(tree));
                ++pos;
            }
        }
    }

    //########################################################################
}
