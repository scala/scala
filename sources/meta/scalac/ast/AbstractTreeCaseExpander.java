/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta.scalac.ast;

/** A base class for expanders that generate switches on tree nodes. */
public abstract class AbstractTreeCaseExpander extends AbstractTreeExpander {

    //########################################################################
    // Public Constructors

    public AbstractTreeCaseExpander() {
        writer.importType(tree.t_Debug);
    }

    //########################################################################
    // Public Methods

    public void printTreeSwitch() {
        writer.println("switch (tree) {");
        writer.println();
        printTreeCases();
        writer.println("default:");
        writer.indent();
        writer.print("throw ").print(tree.t_Debug).
            println(".abort(\"unknown tree\", tree);");
        writer.undent();
        writer.println("}");
    }

    public void printTreeCases() {
        for (int i = 0; i < tree.nodes.length; i++) {
            printTreeCase(tree.nodes[i]);
            writer.println();
        }
    }

    public void printTreeCase(TreeNode node) {
        printTreeCaseHeader(node);
        writer.println().indent();
        printTreeCaseBody(node);
        printTreeCaseFooter(node);
        writer.undent();
    }

    public void printTreeCaseHeader(TreeNode node) {
        node.printCase(writer, false);
    }

    public abstract void printTreeCaseBody(TreeNode node);

    public void printTreeCaseFooter(TreeNode node) {
        // do nothing
    }

    //########################################################################
}
