/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

// TODO find a good way to change symbol flags (PRIVATE, DEFERRED,
// INTERFACE). In particular find how to make sure that the
// modifications are not made too early, for example before the symbol
// is cloned.

package scalac.transformer;

import java.util.Map;
import java.util.HashMap;

import scalac.Global;
import scalac.ast.GenTransformer;
import scalac.ast.Tree;
import scalac.ast.Tree.Template;
import scalac.ast.TreeGen;
import scalac.ast.TreeList;
import scalac.symtab.Type;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolSubstTypeMap;
import scalac.util.Debug;

/**
 * Add, for each class, an interface with the same name, to be used
 * later by mixin expansion. More specifically:
 *
 * - at the end of the name of every class, the string "$class" is
 *   added,
 *
 * - an interface with the original name of the class is created, and
 *   contains all directly bound members of the class (as abstract
 *   members),
 *
 * - the interface is added to the mixin base classes of the class.
 *
 * @author Michel Schinz
 * @version 1.0
 */
public class AddInterfaces extends GenTransformer {

    //#########################################################################
    // Private Fields

    /** The AddInterface phase */
    private final AddInterfacesPhase phase;

    /** The current class (null is none) */
    private Symbol clasz;

    /** The current member (null is none) */
    private Symbol member;

    /** The current class substitution (null is none) */
    private Type.Map classSubst;

    /** The current parameter substitution (null is none) */
    private SymbolSubstTypeMap paramSubst;

    //#########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public AddInterfaces(Global global, AddInterfacesPhase phase) {
        super(global);
        this.phase = phase;
    }

    //#########################################################################
    // Public Methods

    /** Transforms the given symbol. */
    public Symbol getSymbolFor(Tree tree) {
        switch (tree) {
        case Create(_, _):
            return phase.getClassSymbol(tree.symbol());
        case Return(_):
            return member;
        case This(_):
        case Super(_, _):
            return clasz;
        case Select(Super(_, _), _):
            Symbol symbol = tree.symbol();
            if (symbol.isInitializer()) return getClassMember(symbol);
            return getClassMember(symbol, true);
        case Select(_, _):
            Symbol symbol = tree.symbol();
            if (symbol.isInitializer()) return getClassMember(symbol);
            return symbol;
        case Ident(_):
            Symbol symbol = tree.symbol();
            if (symbol.isInitializer()) return getClassMember(symbol);
            if (symbol.isParameter()) return getClassVParam(symbol);
            return symbol;
        default:
            return tree.symbol();
        }
    }

    /** Transforms the given type. */
    public Type transform(Type type) {
        if (classSubst != null) type = classSubst.apply(type);
        if (paramSubst != null) type = paramSubst.apply(type);
        return type;
    }

    /** Transforms the given trees. */
    public Tree[] transform(Tree[] trees) {
        if (member != null) return super.transform(trees);
        TreeList list = new TreeList();
        for (int i = 0; i < trees.length; i++) template(list, trees[i]);
        return list.toArray();
    }

    /** Transforms the given tree. */
    public Tree transform(Tree tree) {
        switch (tree) {
        case ValDef(_, _, _, _):
        case LabelDef(_, _, _):
            Symbol symbol = tree.symbol();
            if (symbol.owner() != member) {
                symbol.setOwner(member);
                symbol.updateInfo(transform(symbol.info()));
            }
            return super.transform(tree);
        case Select(Tree qualifier, _):
            Type prefix = qualifier.type();
            qualifier = transform(qualifier);
            Symbol symbol = getSymbolFor(tree);
            if (symbol.isJava() && !symbol.owner().isInterface()) {
                if (qualifier.type().widen().symbol().isInterface()) {
                    Type baseType = prefix.baseType(symbol.owner());
                    assert baseType != Type.NoType: tree;
                    qualifier = gen.mkAsInstanceOf(qualifier, baseType);
                }
            }
            return gen.Select(tree.pos, qualifier, symbol);
        default:
            return super.transform(tree);
        }
    }

    //#########################################################################
    // Private Methods

    /** Transforms the given template and adds it to given list. */
    private void template(TreeList trees, Tree tree) {
        switch (tree) {
        case Empty:
            return;
        case PackageDef(_, _):
            trees.append(super.transform(tree));
            return;
        case ClassDef(_, _, _, _, _, Template(_, Tree[] body)):
            TreeList list = new TreeList(transform(body));
            this.clasz = tree.symbol();
            Map methods = new HashMap();
            if (phase.needInterface(clasz)) {
                Symbol clone = phase.getClassSymbol(clasz);
                trees.append(getClassTree(clasz, list, methods));
                list = new TreeList();
                this.classSubst = new Type.SubstThisMap(clasz, clone);
                this.paramSubst = phase.getClassSubst(clone);
                this.clasz = clone;
            }
            for (int i = 0; i < body.length; i++) member(methods, body[i]);
            trees.append(getClassTree(clasz, list, methods));
            assert methods.isEmpty(): Debug.show(methods.keySet().toArray());
            this.paramSubst = null;
            this.classSubst = null;
            this.clasz = null;
            return;
        case DefDef(_, _, _, _, _, _):
        case ValDef(_, _, _, _):
            return;
        default:
            throw Debug.abort("illegal tree", tree);
        }
    }

    /**
     * Transforms the given class member. Methods with a non-empty
     * body are added to the given method map. All other members are
     * dropped.
     */
    private void member(Map methods, Tree tree) {
        switch (tree) {
        case ClassDef(_, _, _, _, _, _):
            return;
        case DefDef(_, _, _, _, _, Tree rhs):
            if (rhs == Tree.Empty) return;
            Symbol symbol = tree.symbol();
            this.member = getClassMember(symbol);
            if (member != symbol) {
                paramSubst.insertSymbol(
                    symbol.typeParams(), member.nextTypeParams());
                paramSubst.insertSymbol(
                    symbol.valueParams(), member.nextValueParams());
            }
            methods.put(member, gen.DefDef(member, transform(rhs)));
            if (member != symbol) {
                paramSubst.removeSymbol(symbol.valueParams());
                paramSubst.removeSymbol(symbol.typeParams());
            }
            this.member = null;
            return;
        case ValDef(_, _, _, Tree rhs):
            assert rhs == Tree.Empty: tree;
            return;
        default:
            throw Debug.abort("illegal tree", tree);
        }
    }

    /**
     * Returns the tree of the given class whose body is built by
     * adding to the given body the class members. Non-abstract
     * methods are removed from the given method map. All other
     * members are generated from their symbol.
     */
    private Tree getClassTree(Symbol clasz, TreeList body, Map methods) {
        Scope members = clasz.nextInfo().members();
        for (Scope.SymbolIterator i = members.iterator(true); i.hasNext(); ) {
            Symbol member = i.next();
            if (!member.isTerm()) continue;
            body.append(getMemberTree(member, methods));
        }
        return gen.ClassDef(clasz, body.toArray());
    }

    /**
     * Returns the tree of the given member. Non-abstract methods are
     * removed from the given method map. All other members are
     * generated from their symbol.
     */
    private Tree getMemberTree(Symbol member, Map methods) {
        if (!member.isMethod()) return gen.ValDef(member, Tree.Empty);
        if (member.isDeferred()) return gen.DefDef(member, Tree.Empty);
        Tree method = (Tree)methods.remove(member);
        assert method != null: Debug.show(member);
        return method;
    }

    /** Returns the symbol of given parameter in current class. */
    private Symbol getClassVParam(Symbol vparam) {
        if (paramSubst == null) return vparam;
        Symbol clone = (Symbol)paramSubst.lookupSymbol(vparam);
        assert clone != null: Debug.show(vparam, " - ", clasz, " - ", member);
        return clone;
    }

    /** Returns the symbol of given member in current class. */
    private Symbol getClassMember(Symbol member) {
        return getClassMember(member, false);
    }
    // !!! Try to remove version with lazy argument. It is currently
    // needed for super calls to abstract method (possible in mixins).
    private Symbol getClassMember(Symbol member, boolean lazy) {
        Symbol owner = member.owner();
        assert owner.isClass(): Debug.show(member);
        if (!phase.needInterface(owner)) return member;
        Symbol clasz = phase.getClassSymbol(owner);
        Symbol clone = (Symbol)phase.getClassMemberMap(clasz).get(member);
        assert clone != null || lazy: Debug.show(member, " not in ", clasz);
        return clone != null ? clone : member;
    }

    //#########################################################################
}
