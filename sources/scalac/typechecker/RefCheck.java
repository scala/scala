/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */
// todo: Any cannot be inherited.
// todo: check that only stable defs override stable defs
// todo: admit vardefs w/o rhs
// ClassTemplate   ::= [extends Constr {with Constr}]
//                     [with `(' TemplateStatSeq `)']
// todo: pretty printer prints wrong precedence for `new' (-> lambdalift.scala).

package scalac.typechecker;

import java.util.HashMap;
import scalac.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.symtab.*;
import Tree.*;

/** Check that no forward reference to a term symbol extends beyond a value definition.
 */
public class RefCheck extends Transformer implements Modifiers, Kinds {

    public RefCheck(Global global, RefCheckPhase descr) {
        super(global, descr);
    }

    private Unit unit;
    private Scope[] scopes = new Scope[4];
    private int[] maxindex = new int[4];
    private int[] refpos = new int[4];
    private Symbol[] refsym = new Symbol[4];
    private int level;
    private HashMap symIndex = new HashMap();

    void pushLevel() {
	level++;
	if (level == scopes.length) {
	    Scope[] scopes1 = new Scope[scopes.length * 2];
	    int[] maxindex1 = new int[scopes.length * 2];
	    int[] refpos1 = new int[scopes.length * 2];
	    Symbol[] refsym1 = new Symbol[scopes.length * 2];
	    System.arraycopy(scopes, 0, scopes1, 0, scopes.length);
	    System.arraycopy(maxindex, 0, maxindex1, 0, scopes.length);
	    System.arraycopy(refpos, 0, refpos1, 0, scopes.length);
	    System.arraycopy(refsym, 0, refsym1, 0, scopes.length);
	    scopes = scopes1;
	    maxindex = maxindex1;
	    refpos = refpos1;
	    refsym = refsym1;
	}
	scopes[level] = new Scope(scopes[level - 1]);
	maxindex[level] = Integer.MIN_VALUE;
    }

    void popLevel() {
	scopes[level] = null;
	level --;
    }

    public void apply(Unit unit) {
	this.unit = unit;
	level = 0;
	scopes[0] = new Scope();
	maxindex[0] = Integer.MIN_VALUE;
	unit.body = transformStats(unit.body);
	scopes[0] = null;
	symIndex.clear();
    }

    /** compensate for renaming during addition of access functions
     */
    Name normalize(Name name) {
	return (name.endsWith(Name.fromString("$")))
	    ? name.subName(0, name.length() - 1)
	    : name;
    }

    void enterSyms(Tree[] stats) {
	for (int i = 0; i < stats.length; i++) {
	    enterSym(stats[i], i);
	}
    }

    void enterSym(Tree stat, int index) {
	Symbol sym = null;
	switch (stat) {
	case ClassDef(_, _, _, _, _, _):
	    sym = stat.symbol().constructor();
	    break;
	case DefDef(_, _, _, _, _, _):
	case ModuleDef(_, _, _, _):
	case ValDef(_, _, _, _):
	    sym = stat.symbol();
	}
	if (sym != null && sym.isLocal()) {
	    scopes[level].enter(sym);
	    symIndex.put(sym, new Integer(index));
	}
    }

    public Tree[] transformStats(Tree[] stats) {
	pushLevel();
	enterSyms(stats);
	int i = 0;
	while (i < stats.length) {
	    Tree[] newstat = transformStat(stats[i], i);
	    if (newstat != null) {
		Tree[] newstats = new Tree[stats.length + newstat.length - 1];
		System.arraycopy(stats, 0, newstats, 0, i);
		System.arraycopy(newstat, 0, newstats, i, newstat.length);
		System.arraycopy(stats, i + 1, newstats, i + newstat.length,
				 stats.length - i - 1);
		i = i + newstat.length;
		stats = newstats;
	    } else {
		i = i + 1;
	    }
	}
	popLevel();
	return stats;
    }

    /** The main checking functions
     */
    public Tree[] transformStat(Tree tree, int index) {
	Tree resultTree;
	switch (tree) {
	case ModuleDef(int mods, Name name, Tree tpe, Tree.Template templ):
	    Symbol sym = tree.symbol();
            // local modules are not yet supported but the interpreter
            // accepts modules at the console level
	    if (sym.isLocal()) unit.error("local modules are not yet supported");

	    Tree[] result = new Tree[2];
	    result[0] = make.ClassDef(
		tree.pos,
		mods | FINAL | MODUL,
		name.toTypeName(),
		new Tree.TypeDef[0],
		new Tree.ValDef[][]{{}},
		Tree.Empty,
		templ)
		.setSymbol(sym.moduleClass()).setType(tree.type);
	    result[1] = make.ValDef(
		tree.pos,
		mods | MODUL,
		name,
		(tpe == Tree.Empty) ? gen.mkType(tree.pos, sym.type()) : tpe,
		gen.New(
		    tree.pos,
		    sym.type().prefix(),
		    sym.moduleClass(),
		    Tree.EMPTY_ARRAY))
		.setSymbol(sym).setType(tree.type);
	    return transform(result);

	case ValDef(int mods, Name name, Tree tpe, Tree rhs):
	    Symbol sym = tree.symbol();
	    resultTree = transform(tree);
	    //todo: handle variables
	    if (sym.isLocal() && index <= maxindex[level]) {
		if (Global.instance.debug)
		    System.out.println(refsym[level] + ":" + refsym[level].type());
		unit.error(
		    refpos[level],
		    "forward reference extends over definition of value " +
		    normalize(name));
	    }
	    break;
	default:
	    resultTree = transform(tree);
	}
	return (resultTree == tree) ? null : new Tree[]{resultTree};
    }

    public Tree transform(Tree tree) {
	switch (tree) {
	case Template(Tree[] bases, Tree[] body):
	    Tree[] bases1 = transform(bases);
	    Tree[] body1 = transformStats(body);
	    return copy.Template(tree, bases1, body1);
	case Block(Tree[] stats):
	    Tree[] stats1 = transformStats(stats);
	    return copy.Block(tree, stats1);
	case Ident(Name name):
	    Scope.Entry e = scopes[level].lookupEntry(name);
	    Symbol sym = tree.symbol();
	    assert sym != null : name;
	    if (sym.isLocal() && sym == e.sym) {
		int i = level;
		while (scopes[i] != e.owner) i--;
		int symindex = ((Integer) symIndex.get(tree.symbol())).intValue();
		if (maxindex[i] < symindex) {
		    refpos[i] = tree.pos;
		    refsym[i] = e.sym;
		    maxindex[i] = symindex;
		}
	    }
	    return tree;
	default:
	    return super.transform(tree);
	}
    }
}

