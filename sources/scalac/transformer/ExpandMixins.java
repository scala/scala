/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: ExpandMixins.java,v 1.24 2002/11/11 16:08:50 schinz Exp $
// $Id$

package scalac.transformer;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
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
    private final TreeList body;

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
        this.template = gen.make.Template(template.pos, template.symbol(),
            Tree.cloneArray(template.parents), template.body);
        this.template.setType(Type.compoundType(parents, members, clasz));
        this.body = new TreeList();
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
            inlineMixinVParams(mixin.valueParams(), constr.args);
            inlineMixinMembers(impl);
            parents[i] = Type.TypeRef(prefix, iface, args);
            template.parents[i] = gen.mkParentConstr(constr.pos, parents[i]);
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
                case Select(Super(_), _):
                    Symbol symbol = map.lookupSymbol(tree.symbol());
                    if (symbol != null)
                        return gen.Select(gen.This(tree.pos, clasz), symbol);
                }
                return super.transform(tree);
            }
        };
        body.append(superFixer.transform(template.body));
        template.body = body.toArray();
        // !!! *1 fix ExpandMixinsPhase.transformInfo and remove next line
        clasz.updateInfo(template.type());
        state = 0;
        return template;
    }

    //########################################################################
    // Private Methods

    private void inlineMixinTParams(Type type) {
        switch (type) {
        case TypeRef(Type prefix, Symbol symbol, Type[] args):
            map.insertType(symbol.typeParams(), args);
            if (prefix.symbol() != symbol.owner())
                inlineMixinTParams(prefix.baseType(symbol.owner()));
            return;
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    private void inlineMixinVParams(Symbol[] params, Tree[] args) {
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
            body.append(gen.ValDef(args[i].pos, member, args[i]));
        }
    }

    private void inlineMixinMembers(Template mixin) {
        Scope symbols = mixin.type().members();
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
                default:
                    return super.transform(tree);
                }
            }
        };
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
                body.append(mixinTreeCloner.transform(tree));
            }
        }
    }

    //########################################################################
}
