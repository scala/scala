/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import scala.tools.util.debug.Debugger;
import scala.tools.util.debug.ToStringDebugger;

import scalac.Global;
import scalac.ast.Tree;
import scalac.symtab.Modifiers;
import scalac.symtab.Scope;
import scalac.symtab.Symbol;
import scalac.symtab.Type;

/**
 * Debugging class, used e.g. to obtain string representations of
 * compiler data structures that are not "pretty-printed" and thus
 * easier to relate to the source code.
 *
 * All methods are static to be easily useable in any context.
 *
 * @author Michel Schinz
 * @version 1.0
 */
public class Debug extends scala.tools.util.debug.Debug {

    //########################################################################
    // Private Initialization

    /**
     * Forces the initialization of this class. Returns the boolean
     * value true so that it can be invoked from an assert statement.
     */
    public static boolean initialize() {
        // nothing to do, everything is done in the static initializer
        return true;
    }

    static {
        addDebugger(new ToStringDebugger(Tree.class));
        addDebugger(new ToStringDebugger(Type.class));
        addDebugger(SymbolDebugger.object);
        addDebugger(ScopeDebugger.object);
    }

    //########################################################################
    // Public Methods - Logging

    public static boolean log(Object a) {
        return logAll(new Object[] {a});
    }

    public static boolean log(Object a, Object b) {
        return logAll(new Object[] {a, b});
    }

    public static boolean log(Object a, Object b, Object c) {
        return logAll(new Object[] {a, b, c});
    }

    public static boolean log(Object a, Object b, Object c, Object d) {
        return logAll(new Object[] {a, b, c, d});
    }

    public static boolean logAll(Object[] args) {
        return Global.instance.log(showAll(args, null));
    }

    //########################################################################
    // showTree

    private static void append(StringBuffer buf, Name[] names) {
        for (int i = 0; i < names.length; i++) {
            if (i > 0) buf.append(",");
            append(buf, names[i]);
        }
    }

    private static void append(StringBuffer buf, Name name) {
        buf.append("\"" + name + '"');
    }

    private static void append(StringBuffer buf, String str) {
        buf.append("\"" + str + '"');
    }

    private static void append(StringBuffer buf, Tree[] trees, boolean showType) {
        buf.append('[');
        for (int i = 0; i < trees.length; i++) {
            if (i > 0) buf.append(',');
            append(buf, trees[i], showType);
        }
        buf.append(']');
    }

    private static void append(StringBuffer buf, Tree[][] trees) {
        for (int i = 0; i < trees.length; i++) {
            buf.append('[');
            append(buf, trees[i]);
            buf.append(']');
        }
    }

    private static void append(StringBuffer buf, Tree tree, boolean showType) {
        switch (tree) {
        case Empty:
            buf.append("Empty(");
            break;
        case Attributed(Tree attribute, Tree definition):
            buf.append("Attributed(");
            append(buf, attribute);
            buf.append(',');
            append(buf, definition);
            break;
        case DocDef(String comment, Tree definition):
            buf.append("DocDef(");
            append(buf, comment);
            buf.append(',');
            append(buf, definition);
            break;
        case ClassDef(int mods, Name name, Tree.AbsTypeDef[] tparams,
                      Tree.ValDef[][] vparams, Tree tpe, Tree.Template impl):
            buf.append("ClassDef(");
            Modifiers.Helper.toString(buf, mods);
            buf.append(',');
            append(buf, name);
            buf.append(',');
            append(buf, tparams);
            buf.append(',');
            append(buf, vparams);
            buf.append(',');
            append(buf, tpe);
            buf.append(',');
            append(buf, impl);
            break;
        case PackageDef(Tree packaged, Tree.Template impl):
            buf.append("PackageDef(");
            append(buf, packaged);
            buf.append(',');
            append(buf, impl);
            break;
        case ModuleDef(int mods, Name name, Tree tpe, Tree.Template impl):
            buf.append("ModuleDef(");
            Modifiers.Helper.toString(buf, mods);
            buf.append(',');
            append(buf, name);
            buf.append(',');
            append(buf, tpe);
            buf.append(',');
            append(buf, impl);
            break;
        case ValDef(int mods, Name name, Tree tpe, Tree rhs):
            buf.append("ValDef(");
            Modifiers.Helper.toString(buf, mods);
            buf.append(',');
            append(buf, name);
            buf.append(',');
            append(buf, tpe, showType);
            buf.append(',');
            append(buf, rhs, showType);
            break;
        case PatDef(int mods, Tree pat, Tree rhs):
            buf.append("PatDef(");
            Modifiers.Helper.toString(buf, mods);
            buf.append(',');
            append(buf, pat);
            buf.append(',');
            append(buf, rhs);
            break;
        case DefDef(int mods, Name name, Tree.AbsTypeDef[] tparams,
                    Tree.ValDef[][] vparams, Tree tpe, Tree rhs):
            buf.append("DefDef(");
            Modifiers.Helper.toString(buf, mods);
            buf.append(',');
            append(buf, name);
            buf.append(',');
            append(buf, tparams);
            buf.append(',');
            append(buf, vparams);
            buf.append(',');
            append(buf, tpe);
            buf.append(',');
            append(buf, rhs);
            break;
        case AbsTypeDef(int mods, Name name, Tree rhs, Tree lobound):
            buf.append("AbsTypeDef(");
            Modifiers.Helper.toString(buf, mods);
            buf.append(',');
            append(buf, name);
            buf.append(',');
            append(buf, rhs);
            buf.append(',');
            append(buf, lobound);
            break;
        case AliasTypeDef(int mods, Name name, Tree.AbsTypeDef[] tparams, Tree rhs):
            buf.append("AliasTypeDef(");
            Modifiers.Helper.toString(buf, mods);
            buf.append(',');
            append(buf, name);
            buf.append(',');
            append(buf, tparams);
            buf.append(',');
            append(buf, rhs);
            break;
        case Import(Tree expr, Name[] selectors):
            buf.append("Import(");
            append(buf, expr);
            buf.append(",");
            append(buf, selectors);
            break;
        case CaseDef(Tree pat, Tree guard, Tree body):
            buf.append("CaseDef(");
            buf.append(",");
            append(buf, pat);
            buf.append(",");
            append(buf, guard);
            buf.append(",");
            append(buf, body);
            break;
        case Template(Tree[] parents, Tree[] body):
            buf.append("Template(");
            append(buf, parents);
            buf.append(',');
            append(buf, body);
            break;
        case LabelDef(Name name, Tree.Ident[] params, Tree rhs):
            buf.append("LabelDef(");
            buf.append(",");
            append(buf, name);
            buf.append(',');
            append(buf, params);
            buf.append(',');
            append(buf, rhs);
            break;
        case Block(Tree[] stats, Tree expr):
            buf.append("Block(");
            append(buf, stats);
            buf.append(',');
            append(buf, expr);
            break;
        case Sequence(Tree[] trees):
            buf.append("Sequence(");
            buf.append(',');
            append(buf, trees);
            break;
        case Alternative(Tree[] trees):
            buf.append("Alternative(");
            buf.append(',');
            append(buf, trees);
            break;
        case Bind(Name name, Tree rhs):
            buf.append("Bind(");
            append(buf, name);
            buf.append(',');
            append(buf, rhs);
            break;
        case Visitor(Tree.CaseDef[] cases):
            buf.append("ClassDef(");
            append(buf, cases);
            break;
        case Function(Tree.ValDef[] vparams, Tree body):
            buf.append("Function(");
            append(buf, vparams);
            buf.append(',');
            append(buf, body);
            break;
        case Assign(Tree lhs, Tree rhs):
            buf.append("Assign(");
            append(buf, lhs);
            buf.append(',');
            append(buf, rhs);
            break;
        case If(Tree cond, Tree thenp, Tree elsep):
            buf.append("If(");
            append(buf, cond);
            buf.append(',');
            append(buf, thenp);
            buf.append(',');
            append(buf, elsep);
            break;
        case Switch(Tree test, int[] tags, Tree[] bodies, Tree otherwise):
            buf.append("Switch(");
            buf.append(',');
            append(buf, test);
            buf.append(',');
            buf.append(tags); // TODO
            buf.append(',');
            append(buf, bodies);
            buf.append(",");
            append(buf, otherwise);
            break;
        case Return(Tree expr):
            buf.append("Return(");
            append(buf, expr, showType);
            buf.append(')');
            break;
        case Throw(Tree expr):
            buf.append("Throw(");
            append(buf, expr, showType);
            break;
        case New(Tree init):
            buf.append("New(");
            append(buf, init, showType);
            break;
        case Create(Tree qualifier, Tree[] targs):
            buf.append("Create(");
            append(buf, qualifier);
            buf.append(',');
            append(buf, targs);
            break;
        case Typed(Tree expr, Tree tpe):
            buf.append("Typed(");
            append(buf, expr, showType);
            buf.append(",");
            append(buf, tpe, showType);
            break;
        case TypeApply(Tree fun, Tree[] args):
            buf.append("TypeApply(");
            append(buf, fun, showType);
            buf.append(',');
            append(buf, args, showType);
            break;
        case Apply(Tree fun, Tree[] args):
            buf.append("Apply(");
            append(buf, fun, showType);
            buf.append(',');
            append(buf, args, showType);
            break;
        case Super(Name qualifier, Name mixin):
            buf.append("Super(");
            append(buf, qualifier);
            buf.append(',');
            append(buf, mixin);
            break;
        case This(Name qualifier):
            buf.append("This(");
            append(buf, qualifier);
            break;
        case Select(Tree qualifier, Name selector):
            buf.append("Select(");
            append(buf, qualifier, showType);
            buf.append(',');
            append(buf, selector);
            break;
        case Ident(Name name):
            buf.append("Ident(");
            append(buf, name);
            break;
        case Literal(scalac.atree.AConstant value):
            buf.append("Literal(" + value);
            break;
        case TypeTerm():
            buf.append("TypeTerm(");
            break;
        case SingletonType(Tree ref):
            buf.append("SingletonType(");
            append(buf, ref, showType);
            break;
        case SelectFromType(Tree qualifier, Name selector):
            buf.append("SelectFromType(");
            append(buf, qualifier, showType);
            buf.append(',');
            append(buf, selector);
            break;
        case FunType(Tree[] argtpes, Tree restpe):
            buf.append("FunType(");
            append(buf, argtpes);
            buf.append(',');
            append(buf, restpe);
            break;
        case CompoundType(Tree[] parents, Tree[] refinements):
            buf.append("CompoundType(");
            append(buf, parents);
            buf.append(',');
            append(buf, refinements);
            break;
        case AppliedType(Tree tpe, Tree[] args):
            buf.append("AppliedType(");
            append(buf, tpe);
            buf.append(',');
            append(buf, args);
            break;
        case Try(Tree block, Tree catcher, Tree finalizer):
            buf.append("Try(");
            append(buf, block);
            buf.append(',');
            append(buf, catcher);
            buf.append(',');
            append(buf, finalizer);
            break;
        default:
            buf.append(tree.getClass().getName() + "(");
        }
        buf.append(')');
        if (showType) buf.append(":" + tree.type);
    }

    public static String showTree(Tree tree, boolean showType) {
        StringBuffer buf = new StringBuffer();
        append(buf, tree, showType);
        return buf.toString();
    }

    public static String showTree(Tree tree) {
        return showTree(tree, false);
    }

    //########################################################################
}

/** This class implements a debugger for symbols. */
public class SymbolDebugger implements Debugger {

    //########################################################################
    // Public Constants

    /** The unique instance of this class. */
    public static final SymbolDebugger object = new SymbolDebugger();

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected SymbolDebugger() {}

    //########################################################################
    // Public Methods

    public boolean canAppend(Object object) {
        return object instanceof Symbol;
    }

    public void append(StringBuffer buffer, Object object) {
        Symbol symbol = (Symbol)object;
        if (!symbol.isNone() && !symbol.owner().isRoot() && !symbol.isRoot()) {
            Debug.append(buffer, symbol.owner());
            buffer.append(".");
        }
        buffer.append(symbol.name);
        if (Global.instance.uniqid) {
            buffer.append('#');
            buffer.append(symbol.id);
        }
        if (symbol.isConstructor()) {
            buffer.append('(');
            buffer.append(symbol.constructorClass().name);
            buffer.append(')');
        }
    }

    //########################################################################
}

/** This class implements a debugger for scopes. */
public class ScopeDebugger implements Debugger {

    //########################################################################
    // Public Constants

    /** The unique instance of this class. */
    public static final ScopeDebugger object = new ScopeDebugger();

    //########################################################################
    // Protected Constructors

    /** Initializes this instance. */
    protected ScopeDebugger() {}

    //########################################################################
    // Public Methods

    public boolean canAppend(Object object) {
        return object instanceof Scope;
    }

    public void append(StringBuffer buffer, Object object) {
        Scope scope = (Scope)object;
        buffer.append('{');
        for (Scope.SymbolIterator i = scope.iterator(); i.hasNext();) {
            Debug.append(buffer, i.next());
            if (i.hasNext()) buffer.append(',');
        }
        buffer.append('}');
    }

    //########################################################################
}
