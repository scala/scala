/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

import java.util.List;
import java.util.ArrayList;

import meta.java.Type;
import meta.scalac.Phase;

/** This class describes all tree nodes. */
public class Tree {

    //########################################################################
    // Private Fields

    private final TreeKind
        Any  = TreeKind.Any,
        Type = TreeKind.Type,
        Term = TreeKind.Term,
        Dual = TreeKind.Dual,
        Test = TreeKind.Test,
        None = TreeKind.None;

    private final Type
        t_int       = TreeType.INT,
        t_Object    = TreeType.Reference(null, "Object"),
        t_Global    = TreeType.Reference("scalac", "Global"),
        t_Unit      = TreeType.Reference("scalac", "Unit"),
        t_TreeGen   = TreeType.Reference("scalac.ast", "TreeGen"),
        t_Symbol    = TreeType.Reference("scalac.symtab", "Symbol"),
        t_Type      = TreeType.Reference("scalac.symtab", "Type"),
        t_Name      = TreeType.Name(Any),
        t_TypeName  = TreeType.Name(Type),
        t_TermName  = TreeType.Name(Term),
        t_TestName  = TreeType.Name(Test),
        t_Names     = TreeType.Array(t_Name),
        t_Tree      = getType(0),
        t_TypeTree  = getType(0, Type),
        t_TermTree  = getType(0, Term),
        t_Trees     = getType(1),
        t_TypeTrees = getType(1, Type),
        t_TermTrees = getType(1, Term);

    private final TreeFieldLink
        SymFlags = TreeFieldLink.SymFlags,
        SymName  = TreeFieldLink.SymName;

    private final TreeField
        tree_symbol = new TreeField(t_Symbol, "symbol");

    private final TreeSymbol
        NoSym  = TreeSymbol.NoSym,
        HasSym = TreeSymbol.HasSym(tree_symbol, false),
        DefSym = TreeSymbol.HasSym(tree_symbol, true);

    private final List list
        = new ArrayList();

    //########################################################################
    // Public Fields

    public final TreeNode
        n_Bad            = node("Bad"           , Any , HasSym),
        n_Empty          = node("Empty"         , Any , NoSym),
        n_ClassDef       = node("ClassDef"      , None, DefSym),
        n_PackageDef     = node("PackageDef"    , None, NoSym),
        n_ModuleDef      = node("ModuleDef"     , None, DefSym),
        n_ValDef         = node("ValDef"        , None, DefSym),
        n_PatDef         = node("PatDef"        , None, NoSym),
        n_DefDef         = node("DefDef"        , None, DefSym),
        n_TypeDef        = node("TypeDef"       , None, DefSym),
        n_Import         = node("Import"        , None, HasSym),
        n_CaseDef        = node("CaseDef"       , None, NoSym),
        n_Template       = node("Template"      , None, HasSym),
        n_LabelDef       = node("LabelDef"      , Term, DefSym),
        n_Block          = node("Block"         , Term, NoSym),
        n_Sequence       = node("Sequence"      , Term, NoSym),
        n_Alternative    = node("Alternative"   , Term, NoSym),
        n_Bind           = node("Bind"          , Term, DefSym),
        n_Visitor        = node("Visitor"       , Term, NoSym),
        n_Function       = node("Function"      , Term, NoSym),
        n_Assign         = node("Assign"        , Term, NoSym),
        n_If             = node("If"            , Term, NoSym),
        n_New            = node("New"           , Term, NoSym),
        n_Typed          = node("Typed"         , Term, NoSym),
        n_TypeApply      = node("TypeApply"     , Term, NoSym),
        n_Apply          = node("Apply"         , Term, NoSym),
        n_Super          = node("Super"         , Term, NoSym),
        n_This           = node("This"          , Term, NoSym),
        n_Select         = node("Select"        , Test, HasSym),
        n_Ident          = node("Ident"         , Test, HasSym),
        n_Literal        = node("Literal"       , Term, NoSym),
        n_TypeTerm       = node("TypeTerm"      , Type, NoSym),
        n_SingletonType  = node("SingletonType" , Type, NoSym),
        n_SelectFromType = node("SelectFromType", Type, HasSym),
        n_FunType        = node("FunType"       , Type, NoSym),
        n_CompoundType   = node("CompoundType"  , Type, NoSym),
        n_AppliedType    = node("AppliedType"   , Type, NoSym),
	n_Try            = node("Try"           , Term, NoSym),
	n_While          = node("While"         , Term, NoSym),
	n_DoUntil        = node("DoUntil"       , Term, NoSym);

    public final TreeNode[] nodes;
    public int arrays;

    //########################################################################
    // Public Constructors

    public Tree() {
        nodes = (TreeNode[])list.toArray(new TreeNode[list.size()]);

        n_Bad.
            setDescription("Representation for parser errors").
            setRange(Phase.PARSER, Phase.END);

        n_Empty.
            setDescription("A tree node for the absence of a tree").
            setRange(Phase.PARSER, Phase.UNKNOWN).
            noFields();

        n_ClassDef.
            setDescription("Class and data declaration").
            setRange(Phase.PARSER, Phase.END).
            addField(t_int, "mods", SymFlags).
            addField(t_TypeName, "name", SymName).
            addField(n_TypeDef.getType(1), "tparams").
            addField(n_ValDef.getType(2), "vparams").
            addField(t_TypeTree, "tpe").
            addField(n_Template.getType(0), "impl");

        n_PackageDef.
            setDescription("Package declaration").
            setRange(Phase.PARSER, Phase.UNKNOWN).
            addField(t_TermTree, "packaged").
            addField(n_Template.getType(0), "impl");

        n_ModuleDef.
            setDescription("Module declaration").
            setRange(Phase.PARSER, Phase.REFCHECK).
            addField(t_int, "mods", SymFlags).
            addField(t_TermName, "name", SymName).
            addField(t_TypeTree, "tpe").
            addField(n_Template.getType(0), "impl");

        n_ValDef.
            setDescription("Value declaration (var or let)").
            setRange(Phase.PARSER, Phase.END).
            addField(t_int, "mods", SymFlags).
            addField(t_TermName, "name", SymName).
            addField(t_TypeTree, "tpe").
            addField(t_TermTree, "rhs");


        n_PatDef.
            setDescription("Value declaration with patterns (val)").
            setRange(Phase.PARSER, Phase.DESUGARIZER).
            addField(t_int, "mods").
            addField(t_TermTree, "pat").
            addField(t_TermTree, "rhs");

        n_DefDef.
            setDescription("Function declaration (def)").
            setRange(Phase.PARSER, Phase.END).
            addField(t_int, "mods", SymFlags).
            addField(t_TermName, "name", SymName).
            addField(n_TypeDef.getType(1), "tparams").
            addField(n_ValDef.getType(2), "vparams").
            addField(t_TypeTree, "tpe").
            addField(t_TermTree, "rhs");

        n_TypeDef.
            setDescription("Type declaration").
            setRange(Phase.PARSER, Phase.ERASURE). // !!! could/should be removed earlier?)
            addField(t_int, "mods", SymFlags).
            addField(t_TypeName, "name", SymName).
            addField(t_TypeTree, "rhs").
	    addField(t_TypeTree, "lobound");

        n_Import.
            setDescription("Import declaration").
            setRange(Phase.START, Phase.ANALYZER).
            addField(t_TermTree, "expr").
            addField(t_Names, "selectors");

        n_CaseDef.
            setDescription("Case declaration").
            setRange(Phase.PARSER, Phase.UNKNOWN).
            addField(t_TermTree, "pat").
            addField(t_TermTree, "guard").
            addField(t_TermTree, "body");

        n_Template.
            setDescription("Instantiation templates").
            setRange(Phase.PARSER, Phase.END).
            addField(t_TermTrees, "parents").
            addField(t_Trees, "body");

        n_LabelDef.
            setDescription("Labelled expression - the symbols in the array (must be Idents!) are those the label takes as argument").
            setRange(Phase.OPTIMIZER, Phase.END).
            addField(n_Ident.getType(1), "params").
            addField(t_TermTree, "rhs");

        n_Block.
            setDescription("Block of expressions " +
                "(semicolon separated expressions)").
            setRange(Phase.PARSER, Phase.END).
            addField(t_Trees, "stats");

        n_Sequence.
            setDescription("Sequence of expressions (comma separated expressions)").
            setRange(Phase.PARSER, Phase.LAMBDALIFT).
            addField(t_TermTrees, "trees");

        n_Alternative.
            setDescription("Alternatives of expressions/patterns").
            setRange(Phase.PARSER, Phase.TRANSMATCH).
            addField(t_TermTrees, "trees");

        n_Bind.
            setDescription("Bind of a variable to a rhs pattern, possibly recursive").
            setRange(Phase.PARSER, Phase.TRANSMATCH).
            addField(t_TermName, "name", SymName).
            addField(t_TermTree, "rhs");


        n_Visitor.
            setDescription("Visitor (a sequence of cases)").
            setRange(Phase.PARSER, Phase.TRANSMATCH).
            addField(n_CaseDef.getType(1), "cases");


        n_Function.
            setDescription("Anonymous function").
            setRange(Phase.PARSER, Phase.ANALYZER).
            addField(n_ValDef.getType(1), "vparams").
            addField(t_TermTree, "body");

        n_Assign.
            setDescription("Assignment").
            setRange(Phase.PARSER, Phase.END).
            addField(t_TermTree, "lhs").
            addField(t_TermTree, "rhs");

        n_If.
            setDescription("Conditional expression").
            setRange(Phase.PARSER, Phase.END).
            addField(t_TermTree, "cond").
            addField(t_TermTree, "thenp").
            addField(t_TermTree, "elsep");

        n_New.
            setDescription("Instantiation").
            setRange(Phase.PARSER, Phase.END).
            addField(n_Template.getType(0), "templ");

        n_Typed.
            setDescription("Type annotation").
            setRange(Phase.PARSER, Phase.UNKNOWN). // !!! could be removed by analyzer?
            addField(t_TermTree, "expr").
            addField(t_TypeTree, "tpe");


        n_TypeApply.
            setDescription("Type application").
            setRange(Phase.PARSER, Phase.END).
            addField(t_TermTree, "fun").
            addField(t_TypeTrees, "args");

        n_Apply.
            setDescription("Value application").
            setRange(Phase.PARSER, Phase.END).
            addField(t_Tree, "fun"). // !!! should be t_TermTree
            addField(t_TermTrees, "args");

        n_Super.
            setDescription("Super reference").
            setRange(Phase.PARSER, Phase.END).
            addField(t_TypeTree, "qualifier");

        n_This.
            setDescription("Self reference").
            setRange(Phase.PARSER, Phase.END).
            addField(t_TypeTree, "qualifier");

        n_Select.
            setDescription("Designator").
            setRange(Phase.START, Phase.END).
            addField(t_TermTree, "qualifier").
            addField(t_TestName, "selector", SymName);

        n_Ident.
            setDescription("Identifier").
            setRange(Phase.START, Phase.END).
            addField(t_TestName, "name", SymName);

        n_Literal.
            setDescription("Literal").
            setRange(Phase.PARSER, Phase.END).
            addField(t_Object, "value");

        n_TypeTerm.
            setDescription("TypeTerm").
            setRange(Phase.PARSER, Phase.END);

        n_SingletonType.
            setDescription("Singleton type").
            setRange(Phase.PARSER, Phase.REFCHECK).
            addField(t_TermTree, "ref");

        n_SelectFromType.
            setDescription("Type selection").
            setRange(Phase.PARSER, Phase.REFCHECK).
            addField(t_TypeTree, "qualifier").
            addField(t_TypeName, "selector", SymName);

        n_FunType.
            setDescription("Function type").
            setRange(Phase.PARSER, Phase.REFCHECK).
            addField(t_TypeTrees, "argtpes").
            addField(t_TypeTree, "restpe");

        n_CompoundType.
            setDescription("Object type (~ Template)").
            setRange(Phase.PARSER, Phase.REFCHECK).
            addField(t_TypeTrees, "parents").
            addField(t_Trees, "refinements");

        n_AppliedType.
            setDescription("Applied type").
            setRange(Phase.PARSER, Phase.REFCHECK).
            addField(t_TypeTree, "tpe").
            addField(t_TypeTrees, "args");

	n_Try.
	    setDescription("Try Expression").
	    setRange(Phase.PARSER, Phase.END).
	    addField(t_TermTree, "block").
	    addField(t_TermTree, "catcher").
	    addField(t_TermTree, "finalizer");

	n_While.
	    setDescription("While Loop").
	    setRange(Phase.PARSER, Phase.END).
	    addField(t_TermTree, "cond").
	    addField(t_TermTree, "block");

	n_DoUntil.
	    setDescription("Do-Until Loop").
	    setRange(Phase.PARSER, Phase.END).
	    addField(t_TermTree, "block").
	    addField(t_TermTree, "cond");
    }

    //########################################################################
    // Public Functions

    public static boolean isTree(Type type) {
        switch (type) {
        case Array(Type item):
            return isTree(item);
        case TreeType.Tree(_):
        case TreeType.Node(_):
            return true;
        default:
            return false;
        }
    }

    //########################################################################
    // Public Methods

    public Type getType(int rank) {
        return getType(rank, Any);
    }

    public Type getType(int rank, TreeKind kind) {
        arrays = Math.max(arrays , rank);
        return rank==0 ? TreeType.Tree(kind) : TreeType.Array(getType(rank-1));
    }

    public String getFormal(String name) {
        return getType(0) + " " + name;
    }

    //########################################################################
    // Private Methods

    private TreeNode node(String name, TreeKind kind, TreeSymbol symbol) {
        TreeNode node = new TreeNode(name, kind, symbol);
        list.add(node);
        return node;
    }

    //########################################################################
}
