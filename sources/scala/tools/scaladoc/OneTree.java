/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import ch.epfl.lamp.util.Position;

import scalac.Global;
import scalac.ast.Tree;
import scalac.ast.Tree.*;
import scalac.symtab.Kinds;
import scalac.symtab.Modifiers;
import scalac.symtab.Symbol;
import scalac.symtab.TermSymbol;
import scalac.util.*;

/**
 * This class is used to merge several compilation units into one
 * single tree by creating nodes for java packages. It addionally
 * removes any node which is not a declaration.
 */
public class OneTree {

    /**
     * Global compiler environment.
     */
    protected Global global;

    /**
     * Root node of the unique tree.
     */
    protected Node.PackageNode rootNode;

    /**
     * ..
     */
    OneTree(Global global) {
	super();
	this.global = global;
	rootNode = Node.PackageNode(global.definitions.ROOT, new LinkedList());
    }

    /**
     * Main function.
     */
    public static Tree apply(Global global) {
	return new OneTree(global).enterNodes();
    }

    /**
     * Returns the symbol in the unique tree corresponding to the given symbol.
     *
     * @param sym
     */
    public static Symbol symbol(Symbol sym) {
	if (sym.isRoot())
	    return sym.moduleClass();
	else if (sym.isPackage() && (sym.kind == Kinds.CLASS))
	    return sym.module();
 	else if (sym.isModuleClass())
 	    return sym.module();
	else return sym;
    }


    /**
     * Enters all the nodes in the unique tree.
     */
    protected Tree enterNodes() {
	for (int i = 0; i < global.units.length; i++)
	    enter(global.units[i].body);
	return rootNode.toTree(global);
    }

    /**
     * Transforms a Java package symbol into a tree definition.
     *
     * @param sym
     * @param body
     * @param global
     */
    protected static Tree packageSymbolTree(Symbol sym, Tree[] body, Global global) {
	if (sym.isRoot())
	    return global.treeGen.ClassDef(sym.moduleClass(), Tree.EMPTY_ARRAY, Symbol.NONE, body);
	else {
	    Template t = global.make.Template(Position.NOPOS,
					      Tree.EMPTY_ARRAY,
					      body);
	    return global.make.ModuleDef(Position.NOPOS, sym.module(), Tree.Empty, t);
	}
    }

    /**
     * Intermediate datastructure representing the unique tree.
     */
    protected static class Node {
	case PackageNode(Symbol sym, List decls); // package
	case TreeNode(Tree tree); // tree (class or object)

	/** Lookups for a child node in a package node. */
	Node.PackageNode lookup(Symbol sym) {
	    Iterator it = ((Node.PackageNode) this).decls.iterator();
	    while (it.hasNext()) {
		Node node = (Node) it.next();
		switch(node) {
		case PackageNode(Symbol sym1, List decls1):
		    if (sym == sym1)
			return (Node.PackageNode) node;
		    break;
		}
	    }
	    return null;
	}

	/** Adds a child node to a package node.
	 */
	void add(Node node) {
	    ((Node.PackageNode) this).decls.add(node);
	}

	/** Transforms a node into a tree.
	 */
	static Tree[] nodeToTree(Node[] nodes, Global global) {
	    Tree[] trees = new Tree[nodes.length];
	    for(int i = 0; i < nodes.length; i++)
		trees[i] = nodes[i].toTree(global);
	    return trees;
	}
	Tree toTree(Global global) {
	    Tree res = null;
	    switch(this) {
	    case TreeNode(Tree tree):
		res = tree;
		break;
	    case PackageNode(Symbol sym, List decls):
		res = packageSymbolTree(sym,
					nodeToTree((Node[]) decls.toArray(new Node[decls.size()]),
						   global),
					global);
		break;
	    }
	    return res;
	}
    }

    /**
     * Returns the node associated with this symbol, creates it if it
     * does not exist.
     *
     * @param sym
     */
    protected Node.PackageNode getNode(Symbol sym) {
	if (sym.isRoot())
	    return rootNode;
	else {
	    Node.PackageNode parent = getNode(sym.owner());
	    Node.PackageNode thisnode = parent.lookup(sym);
	    if (thisnode == null) {
		thisnode = Node.PackageNode(sym, new LinkedList());
		parent.add(thisnode);
	    }
	    return thisnode;
	}
    }

    /**
     * Enter a global declaration in the unique tree.
     *
     * @param body
     */
    protected void enter(Tree[] body) {
	for(int i = 0; i< body.length; i++)
	    enter(body[i]);
    }
    protected void enter(Tree tree) {
	if (tree instanceof PackageDef)
	    enter(((PackageDef) tree).impl.body);
	else if ((tree instanceof ClassDef || tree instanceof ModuleDef) &&
		 tree.symbol().owner().isPackage())
	    //getNode(tree.symbol().owner()).add(Node.TreeNode(tree));
	    getNode(tree.symbol().owner()).add(Node.TreeNode(keepDeclarations(tree, new LinkedList())));
    }

    /** Do we keep this symbol ?
     */
    protected boolean keep(Tree tree) {
	return tree.hasSymbol() &&
	    !tree.symbol().isPrivate() &&
	    !ScalaSearch.isGenerated(tree.symbol());
    }

    /**
     * Removes nodes which are not declarations in a tree.
     *
     * @param tree
     * @param ownerDeclList
     */
    protected Tree keepDeclarations(Tree tree, List ownerDeclList) {
	if (!keep(tree) && !(tree instanceof Template))
	    return null;
	switch (tree) {
	case ClassDef(int mods, Name name, AbsTypeDef[] tparams, ValDef[][] vparams, Tree tpe, Template impl):
	    List declList = new LinkedList();
	    Template impl1 = (Template) keepDeclarations(impl, declList);
	    //Tree tree1 = global.make.ClassDef(tree.pos, mods, name, tparams, vparams, tpe, impl1);
	    Tree tree1 = global.treeGen.ClassDef(tree.symbol(), impl1.parents, TermSymbol.newLocalDummy(tree.symbol()), impl1.body);
	    ownerDeclList.add(tree1);
	    return tree1;
	case ModuleDef(int mods, Name name, Tree tpe, Template impl):
	    List declList = new LinkedList();
	    Template impl1 = (Template) keepDeclarations(impl, declList);
	    assert impl1 != null: "Null impl";
	    Tree tree1 = global.make.ModuleDef(tree.pos, tree.symbol(), tpe, impl1);
	    ownerDeclList.add(tree1);
	    return tree1;
	case ValDef(int mods, Name name, Tree tpe, Tree rhs):
	    ownerDeclList.add(tree);
	    return tree;
	case DefDef(int mods, Name name, AbsTypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs):
	    ownerDeclList.add(tree);
	    return tree;
	case AbsTypeDef(_, _, _, _):
	case AliasTypeDef(_, _, _, _):
	    ownerDeclList.add(tree);
	    return tree;
        case Template(Tree[] parents, Tree[] body):
	    for(int i = 0; i < body.length; i++)
		keepDeclarations(body[i], ownerDeclList);
	    Tree[] ownerDecls = (Tree[]) ownerDeclList.toArray(new Tree[ownerDeclList.size()]);
	    return global.make.Template(tree.pos, parents, ownerDecls);
	default:
	    return tree;
	}
    }

}
