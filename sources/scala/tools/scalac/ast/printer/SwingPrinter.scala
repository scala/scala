/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$


import scalac.CompilationUnit;
import scalac.symtab._;
import scalac.ast.Tree;
import scalac.ast.printer._;

import scalac.{Global => scalac_Global, Phase};
import scalac.CompilationUnit;
import scalac.util.Name;
import scalac.util.TypeNames;

import scala.concurrent._;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.HashMap;

import javax.swing.tree._;
import javax.swing.event.TreeModelListener;
import javax.swing._;

import java.awt.{List => awtList, _};
import java.awt.event._;

package scala.tools.scalac.ast.printer {

/**
 * Java Swing pretty printer for Scala abstract syntax trees.
 *
 * @author Iulian Dragos
 * @version 0.2
 */
class SwingTreePrinter(global0: scalac_Global) extends TreePrinter {
  val global = global0;

  def begin() = ();
  def end()   = ();
  def flush() = ();


  /** print the whole program */
  def print(units: Array[CompilationUnit]): Unit = {
    val phase: Phase = global.currentPhase;

    val tm = new ASTTreeModel();

   for (val i <- Iterator.range(0, units.length))
     tm.addUnit(units(i));

    val frame = new WindowFrame();
    frame.setTreeModel(tm);

    val lock = new Lock();
    frame.createFrame(lock);

    // wait for the frame to be closed
    lock.acquire;
  }

  def print(tree: Tree): Unit = ();

}

/** Tree model for abstract syntax trees */
class ASTTreeModel extends TreeModel {
  var listeners: List[TreeModelListener] = Nil;
  var units: List[CompilationUnit] = Nil;

  def addUnit(c: CompilationUnit): Unit = units = c :: units;

  /** Add a listener to this tree */
  def addTreeModelListener(l : TreeModelListener): Unit = listeners = l :: listeners;

  /** Return the index'th child of parent */
  def getChild(parent: Any, index: Int): AnyRef = {
    packChildren(parent).drop(index).head;
  }

  /** Return the number of children this 'parent' has */
  def getChildCount(parent: Any): Int =
    packChildren(parent).length;

  /** Return the index of the given child */
  def getIndexOfChild(parent: Any, child: Any): Int =
    packChildren(parent).dropWhile(c => c != child).length;

  /** Return the root node */
  def getRoot(): AnyRef = units;

  /** Test whether the given node is a leaf */
  def isLeaf(node: Any): Boolean = packChildren(node).length == 0;

  def removeTreeModelListener(l: TreeModelListener): Unit =
    listeners remove (x => x == l);

  /** we ignore this message for now */
  def valueForPathChanged(path: TreePath, newValue: Any) = ();

  /** Return a list of objects for the current node. We have to
    * take care of the "top" of the tree, which is not made of case
    * classes of Tree. Therefore, instanceOf is used to differentatiate
    * between "true" tree nodes, and "logical" ones (CompilationUnit and
    * Program).
    *
    * @todo A type-safe version of this. However, new case classes of Tree
    *       cannot be derived in Scala because of "final" method getType of
    *       Tree, which overlaps with scala runtime types.
    */
  def packChildren(t: Any): List[AnyRef] =
    if (t.isInstanceOf[CompilationUnit]) {
      val cu = t.asInstanceOf[CompilationUnit];
      List.fromArray(cu.body)
    } else if (t.isInstanceOf[Tree])
      TreeInfo.packTreeChildren(t.asInstanceOf[Tree]);
      else if (t.isInstanceOf[List[CompilationUnit]])
	t.asInstanceOf[List[CompilationUnit]];
      else
	Nil;

}


/** a window that can host the Tree widget and provide methods for
  * displaying information */
class WindowFrame {
  val frame = new JFrame("Scala AST");
  val topPane = new JPanel(new BorderLayout());
  val topRightPane = new JPanel(new BorderLayout());
  val bottomPane = new JPanel(new BorderLayout());
  var splitPane: JSplitPane = _;
  var treeModel: TreeModel = _;

  val textArea: JTextArea = new JTextArea(20, 50);
  val infoPanel = new InfoPanel();


  /** Create a frame that displays the AST.
    *
    * @param lock The lock is used in order to stop the compilation thread
    * until the user is done with the tree inspection. Swing creates its
    * own threads when the frame is packed, and therefore execution
    * would continue. However, this is not what we want, as the tree and
    * especially symbols/types would change while the window is visible.
    */
  def createFrame(lock: Lock): Unit = {
    lock.acquire; // keep the lock until the user closes the window

    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

    frame.addWindowListener(new WindowAdapter() {
      /** Release the lock, so compilation may resume after the window is closed. */
      override def windowClosed(e: WindowEvent): Unit = lock.release;
    });

    val tree = new JTree(treeModel) {
      /** Return the string for a tree node. */
      override def convertValueToText(value: Any, sel: Boolean,
				      exp: Boolean, leaf: Boolean,
				      row: Int, hasFocus: Boolean) = {
        if (value.isInstanceOf[List[CompilationUnit]])
	  "Program"
	else if (value.isInstanceOf[CompilationUnit])
	  "CompilationUnit"
	else {
	  val Pair(cls, name) = TreeInfo.treeName(value.asInstanceOf[Tree]);
          if (name != TreeInfo.NO_NAME)
	    cls + "[" + name.toString() + "]";
	  else
	    cls;
	}
      }
    }

    tree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
      def valueChanged(e: javax.swing.event.TreeSelectionEvent): Unit = {
	textArea.setText(e.getPath().getLastPathComponent().toString());
	infoPanel.update(e.getPath().getLastPathComponent());
      }
    });

    val topSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, topPane, topRightPane);
    topPane.add(new JScrollPane(tree), BorderLayout.CENTER);

    topRightPane.add(new JScrollPane(infoPanel), BorderLayout.CENTER);

    bottomPane.add(new JScrollPane(textArea), BorderLayout.CENTER);
    textArea.setFont(new Font("monospaced", Font.PLAIN, 14));
    textArea.setEditable(false);

    splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topSplitPane, bottomPane);
    frame.getContentPane().add(splitPane);
    frame.pack();
    frame.setVisible(true);
  }

  def setTreeModel(tm: TreeModel): Unit = treeModel = tm;
}

/** Pannel that shows some information about the selected
  * tree node (like symbol, type, etc) */
class InfoPanel extends JPanel() {
  val symbolLine = Box.createHorizontalBox();
  val symbolAttLine = Box.createHorizontalBox();
  val symbolTypeLine = Box.createHorizontalBox();
  val treeTypeLine = Box.createHorizontalBox();

  val symLabel = new JLabel("");
  val attLabel = new JLabel("");
  val stypeLabel = new JLabel("");
  val ttypeLabel = new JLabel("");

  setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

  symbolLine.add(new JLabel("Symbol: "));
  symbolLine.add(symLabel);
  symbolLine.add(Box.createHorizontalGlue());

  symbolAttLine.add(new JLabel("Symbol attributes: "));
  symbolAttLine.add(attLabel);
  symbolAttLine.add(Box.createHorizontalGlue());

  symbolTypeLine.add(new JLabel("Symbol type: "));
  symbolTypeLine.add(stypeLabel);
  symbolTypeLine.add(Box.createHorizontalGlue());

  treeTypeLine.add(new JLabel("Tree type: "));
  treeTypeLine.add(ttypeLabel);
  treeTypeLine.add(Box.createHorizontalGlue());

  add(symbolLine);
  add(symbolAttLine);
  add(symbolTypeLine);
  add(treeTypeLine);

  def update(v: AnyRef): Unit =
    if (v.isInstanceOf[Tree]) {
      val t = v.asInstanceOf[Tree];

      symLabel.setText(TreeInfo.symbolText(t));
      stypeLabel.setText(TreeInfo.symbolTypeText(t));
      attLabel.setText(TreeInfo.symbolAttributes(t));
      ttypeLabel.setText(t.getType().toString());
    } else
      reset;

  def reset: Unit = {
    symLabel.setText("");
    stypeLabel.setText("");
    ttypeLabel.setText("");
    attLabel.setText("");
  }
}

}  // package

