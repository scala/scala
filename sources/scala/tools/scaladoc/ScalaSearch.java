/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import ch.epfl.lamp.util.Pair;

import scalac.Global;
import scalac.ast.Tree;
import scalac.ast.Tree.*;
import scalac.symtab.Scope;
import scalac.symtab.Scope.SymbolIterator;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.NameTransformer;

/**
 * This class contains functions to retrieve information from a Scala
 * library.
 */
public class ScalaSearch {

    /**
     * Character which ends a name to indicates it is a class symbol
     * name.
     */
    public static final char classChar = ',';

    /**
     * Character which ends a name to indicates it is an object symbol
     * name.
     */
    public static final char objectChar = '.';

    /**
     * Returns the list of owners of a symbol (including itself).
     *
     * @param sym
     */
    public static Symbol[] getOwners(Symbol sym) {
        List l = getOwnersAux(sym);
        return (Symbol[]) l.toArray(new Symbol[l.size()]);
    }

    // where
    private static List getOwnersAux(Symbol sym) {
	if (sym == Symbol.NONE)
	    return new LinkedList();
	else {
	    List ownerStaticPath = getOwnersAux(sym.owner());
	    sym = sym.isModuleClass() ? sym.module() : sym;
	    ownerStaticPath.add(sym);
	    return ownerStaticPath;
	}
    }

    /**
     * Gives a string representation of this symbol.
     *
     * @param sym
     * @param showOwners
     */
    public static String getSymbolName(Symbol sym, boolean showOwners) {
        return (showOwners) ? getOwnersString(sym) : sym.nameString();
    }

    /**
     * Returns a string representation of the path leading to this
     * symbol.
     *
     * @param sym
     * @return the string representation of this symbol
     */
    public static String getOwnersString(Symbol sym) {
	Symbol[] elements = getOwners(sym);
	StringBuffer buff = new StringBuffer();
        // we ignore elements[0] which contains the root symbol
	for (int i = 1; i < elements.length; i++) {
            if (i > 1) buff.append(objectChar);
	    buff.append(elements[i].nameString());
	}
	return buff.toString();
    }

    /**
     * Class representing functions on trees.
     */
    public static abstract class TreeFun {
	abstract void apply(Tree tree);
    }

    /**
     * Apply a function to all definition nodes in a tree.
     *
     * @param trees
     * @param fun
     */
    public static void foreach(Tree[] trees, TreeFun fun) {
	for (int i = 0; i < trees.length; i++)
	    foreach(trees[i], fun);
    }

    /**
     * ..
     *
     * @param trees
     * @param fun
     */
    public static void foreach(Tree tree, TreeFun fun) {
	switch(tree) {
	case ClassDef(int mods, Name name, AbsTypeDef[] tparams, ValDef[][] vparams, Tree tpe, Template impl):
	    fun.apply(tree);
	    foreach(impl, fun);
	    break;
        case ModuleDef(int mods, Name name, Tree tpe, Template impl):
	    fun.apply(tree);
	    foreach(impl, fun);
	    break;
        case ValDef(int mods, Name name, Tree tpe, Tree rhs):
	    fun.apply(tree);
	    break;
        case DefDef(int mods, Name name, AbsTypeDef[] tparams, ValDef[][] vparams, Tree tpe, Tree rhs):
	    fun.apply(tree);
	    break;
	case AbsTypeDef(_, _, _, _):
        case AliasTypeDef(_, _, _, _):
	    fun.apply(tree);
	    break;
        case Template(Tree[] parents, Tree[] body):
	    foreach(body, fun);
	    break;
	default:
	}
    }

    /**
     * Use the simple name of symbols to order them.
     */
    public static Comparator symAlphaOrder = new Comparator() {
	    public int compare(Object o1, Object o2) {
		Symbol symbol1 = (Symbol) o1;
		Symbol symbol2 = (Symbol) o2;
		String name1 = symbol1.nameString();
 		String name2 = symbol2.nameString();
		return name1.compareTo(name2);
	    }
	    public boolean equals(Object o) {
		return false;
	    }
	};

    /**
     * Use the fully qualified name of symbols to order them.
     */
    public static Comparator symPathOrder = new Comparator() {
	    public int compare(Object o1, Object o2) {
		Symbol symbol1 = (Symbol) o1;
		Symbol symbol2 = (Symbol) o2;
		String name1 = symbol1.fullName().toString();
 		String name2 = symbol2.fullName().toString();
		return name1.compareTo(name2);
	    }
	    public boolean equals(Object o) {
		return false;
	    }
	};

    /**
     * Use the simple name of trees to order them.
     */
    public static Comparator alphaOrder = new Comparator() {
	    public int compare(Object o1, Object o2) {
		Tree tree1 = (Tree) o1;
		Tree tree2 = (Tree) o2;
		assert tree1.hasSymbol() : tree1;
		assert tree2.hasSymbol() : tree2;
		String name1 = NameTransformer.decode(tree1.symbol().name).toString();
 		String name2 = NameTransformer.decode(tree2.symbol().name).toString();
		return name1.compareTo(name2);
	    }
	    public boolean equals(Object o) {
		return false;
	    }
	};

    /**
     * Use the fully qualified name of trees to order them.
     */
    public static Comparator fullPathOrder = new Comparator() {
        public int compare(Object o1, Object o2) {
            Tree tree1 = (Tree) o1;
            Tree tree2 = (Tree) o2;
            assert tree1.hasSymbol();
            assert tree2.hasSymbol();
            String name1 = tree1.symbol().fullName().toString();
            String name2 = tree2.symbol().fullName().toString();
            return name1.compareTo(name2);
        }
        public boolean equals(Object o) {
            return false;
        }
    };

    /**
     * Use the non qualified name of trees to order them.
     */
    public static Comparator pathOrder = new Comparator() {
        public int compare(Object o1, Object o2) {
            Tree tree1 = (Tree) o1;
            Tree tree2 = (Tree) o2;
            assert tree1.hasSymbol();
            assert tree2.hasSymbol();
            String name1 = tree1.symbol().nameString();
            String name2 = tree2.symbol().nameString();
            return name1.compareTo(name2);
        }
        public boolean equals(Object o) {
            return false;
        }
    };

    /**
     * Returns the sorted list of packages in the root tree.
     *
     * @param root
     */
    public static Tree[] getSortedPackageList(Tree root) {
	final List packagesAcc = new LinkedList();
	foreach(root,
		new TreeFun() {
		    void apply(Tree tree) {
			if (tree.hasSymbol() && tree.symbol().isPackage())
			    packagesAcc.add(tree);
		    }
		});
	Tree[] packages = (Tree[]) packagesAcc.toArray(new Tree[packagesAcc.size()]);
	Arrays.sort(packages, fullPathOrder);
	return packages;
    }

    /**
     * Returns a pair consisting of the sorted list of classes
     * and the sorted list of objects in the root tree.
     *
     * @param root
     */
    public static Tree[][] getSortedPackageMemberList(Tree root) {
	final List objectsAcc = new LinkedList();
	final List traitsAcc = new LinkedList();
	final List classesAcc = new LinkedList();
	foreach(root,
		new TreeFun() {
		    void apply(Tree tree) {
			if (tree.hasSymbol()) {
                            Symbol sym = tree.symbol();
                            if (sym.owner().isPackage()) {
                                if (sym.isTrait())
                                    traitsAcc.add(tree);
                                else if (sym.isClass())
			            classesAcc.add(tree);
			        else if (sym.isModule() && !sym.isPackage())
			            objectsAcc.add(tree);
                            }
                        }
		    }
		});
        Tree[] objects = (Tree[]) objectsAcc.toArray(new Tree[objectsAcc.size()]);
	Tree[] traits  = (Tree[]) traitsAcc.toArray(new Tree[traitsAcc.size()]);
	Tree[] classes = (Tree[]) classesAcc.toArray(new Tree[classesAcc.size()]);
        Arrays.sort(objects, pathOrder);
	Arrays.sort(traits, pathOrder);
	Arrays.sort(classes, pathOrder);
	return new Tree[][]{ objects, traits, classes };
    }

    /**
     * Returns a pair consisting of the sorted list of classes
     * and the sorted list of objects in the root tree.
     *
     * @param root
     */
    public static Pair containersSortedList(Tree root) {
	final List classesAcc = new LinkedList();
	final List objectsAcc = new LinkedList();
	foreach(root,
		new TreeFun() {
		    void apply(Tree tree) {
			if (tree.hasSymbol()) {
                            Symbol sym = tree.symbol();
                            if (sym.isClass())
			        classesAcc.add(tree);
			    else if (sym.isModule() && !sym.isPackage())
			        objectsAcc.add(tree);
                        }
		    }
		});
	Tree[] classes = (Tree[]) classesAcc.toArray(new Tree[classesAcc.size()]);
	Tree[] objects = (Tree[]) objectsAcc.toArray(new Tree[objectsAcc.size()]);
	Arrays.sort(classes, pathOrder);
	Arrays.sort(objects, pathOrder);
	return new Pair(classes, objects);
    }

    /**
     * Returns a hashtable which maps each class symbol to the list of
     * its direct sub-classes or sub-modules. We also keep track of
     * the exact involved type.
     * Result type = Map<Symbol, List<Pair<Symbol, Type>>
     *
     * @param root
     */
    public static Map subTemplates(Tree root) {
	final Map subs = new HashMap();
	foreach(root, new TreeFun() { void apply(Tree tree) {
	    if (tree.hasSymbol()) {
		Symbol sym = tree.symbol();
		if (sym.isClass() || sym.isModule()) {
		    Type[] parents = sym.moduleClass().parents();
		    for (int i = 0; i < parents.length; i++) {
			Symbol parentSymbol = parents[i].symbol();
			List subList = (List) subs.get(parentSymbol);
			if (subList == null) {
			    subList = new LinkedList();
			    subs.put(parentSymbol, subList);
			}
			subList.add(new Pair(sym, parents[i]));
		    }
		}
	    }
	}});
	return subs;
    }

    /**
     * Returns the list of characters with the sorted list of
     * members starting with a character.
     * Result type = Pair<Character[], Map<Character, Tree[]>>
     *
     * @param root
     */
    public static Pair index(Tree root) {
	final Map index = new HashMap();
	// collecting
	foreach(root, new TreeFun() { void apply(Tree tree) {
	    if (tree.hasSymbol() /*&& !tree.symbol().isGenerated()*/) {
		Symbol sym = tree.symbol();
		String name = NameTransformer.decode(sym.name).toString();
		if (name.length() > 0) {
		    char ch = Character.toUpperCase(name.charAt(0));
		    Character unicode = new Character(ch);
		    List symList = (List) index.get(unicode);
		    if (symList == null) {
			symList = new LinkedList();
			index.put(unicode, symList);
		    }
		    symList.add(tree);
		}
	    }
	}});
	// sorting
	Character[] chars = (Character[]) index.keySet()
	    .toArray(new Character[index.keySet().size()]);
	Arrays.sort(chars);
	for (int i = 0; i < chars.length; i++) {
	    Character car = chars[i];
	    List treeList = (List) index.get(car);
	    Tree[] trees = (Tree[]) treeList.toArray(new Tree[treeList.size()]);
	    Arrays.sort(trees, alphaOrder);
	    index.put(car, trees);
	}
	return new Pair(chars, index);
    }

    /**
     * "Try" to determine if a symbol has been generated by the
     * compiler.
     *
     * @param sym
     */
    public static boolean isGenerated(Symbol sym) {
	return
	    (sym.isSynthetic() && !sym.isRoot()) ||
	    (sym.isGenerated() &&
	     NameTransformer.decode(sym.name).toString().equals(sym.name.toString())) ||
	    NameTransformer.decode(sym.name).toString().endsWith("_=");
    }

    /**
     * Finds all local and inherited members of a given class or
     * object.
     *
     * @param sym
     */
    public static Symbol[] findMembers(Symbol sym) {
	Type thistype = sym.thisType();
	Name[] names = potentialMemberNames(thistype); // potentialMembers
	List/*<Symbol>*/ members = new LinkedList(); // actual members
	for (int i = 0; i < names.length; i++) {
	    Symbol member = thistype.lookup(names[i]);
	    if (member != Symbol.NONE)
		if (!member.isConstructor())
		    members.add(member);
	}
	List unloadedMembers = new LinkedList();
	Iterator it = members.iterator();
	while (it.hasNext()) {
	    Symbol[] alts = ((Symbol) it.next()).alternativeSymbols();
	    for (int i = 0; i < alts.length; i++)
		unloadedMembers.add(alts[i]);
	}
	return (Symbol[]) unloadedMembers.toArray(new Symbol[unloadedMembers.size()]);
    }

    // where
    protected static Name[] potentialMemberNames(Type tpe) {
	List names = new LinkedList();
	potentialMemberNames(tpe, names);
	return (Name[]) names.toArray(new Name[names.size()]);
    }

    // where
    protected static void potentialMemberNames(Type tpe, List/*<Name>*/ names) {
	// local members
	Scope.SymbolIterator it = tpe.members().iterator();
	while (it.hasNext()) {
	    Name name = ((Symbol) it.next()).name;
	    if (!names.contains(name))
		names.add(name);
	}
	// inherited members
	Type[] parents = tpe.parents();
	for (int i = 0; i < parents.length; i++)
	    potentialMemberNames(parents[i], names);
    }

    /**
     * Groups symbols with respect to their owner and sort the owners
     * by name.
     *
     * @param syms
     */
    public static Pair/*<Symbol[], Map<Symbol, Symbol[]>>*/ groupSymbols(Symbol[] syms) {
	Map/*<Symbol, List>*/ groups = new HashMap();
	for (int i = 0; i < syms.length; i++) {
	    List group = (List) groups.get(syms[i].owner());
	    if (group == null) {
		group = new LinkedList();
		groups.put(syms[i].owner(), group);
	    }
	    group.add(syms[i]);
	}
	Symbol[] owners = (Symbol[]) groups.keySet().toArray(new Symbol[groups.keySet().size()]);
	Arrays.sort(owners, symPathOrder);
	for (int i = 0; i < owners.length; i++) {
	    List groupList = (List) groups.get(owners[i]);
	    Symbol[] group = (Symbol[]) groupList.toArray(new Symbol[groupList.size()]);
	    Arrays.sort(group, symAlphaOrder);
	    groups.put(owners[i], group);
	}
	return new Pair(owners, groups);
    }

    // where
    public static Symbol lookup(Symbol root, String path) {
	if (path.equals(""))
	    return root;
	else {
	    int classEnd = path.indexOf(classChar);
	    int objectEnd = path.indexOf(objectChar);
	    if (classEnd > 0 && (objectEnd == -1 || objectEnd > classEnd)) {
		String name = path.substring(0, classEnd);
		String rest = path.substring(classEnd + 1);
		Symbol member = root.moduleClass().lookup(Name.fromString(name).toTypeName());
		if (member.isClass())
		    return lookup(member, rest);
		else
		    return Symbol.NONE;
	    }
	    else if (objectEnd > 0 && (classEnd == -1 || classEnd > objectEnd)) {
		String name = path.substring(0, objectEnd);
		String rest = path.substring(objectEnd + 1);
		Symbol member = root.moduleClass().lookup(Name.fromString(name).toTermName());
		if (member.isModule())
		    return lookup(member, rest);
		else
		    return Symbol.NONE;
	    }
	    else
		throw Debug.abort("illegal path", path);
	}
    }

}
