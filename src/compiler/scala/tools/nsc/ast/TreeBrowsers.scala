/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package tools.nsc
package ast

import scala.language.implicitConversions

import java.awt.{List => _, _}
import java.awt.event._
import java.io.StringWriter

import javax.swing._
import javax.swing.event.TreeModelListener
import javax.swing.tree._

import scala.concurrent.Lock
import scala.text._

/**
 * Tree browsers can show the AST in a graphical and interactive
 * way, useful for debugging and understanding.
 *
 * @author Iulian Dragos
 * @version 1.0
 */
abstract class TreeBrowsers {
  val global: Global
  import global._
  import nme.EMPTY

  val borderSize = 10

  def create(): SwingBrowser = new SwingBrowser()

  /** Pseudo tree class, so that all JTree nodes are treated uniformly */
  case class ProgramTree(units: List[UnitTree]) extends Tree {
    override def toString: String = "Program"
  }

  /** Pseudo tree class, so that all JTree nodes are treated uniformly */
  case class UnitTree(unit: CompilationUnit) extends Tree {
    override def toString: String = unit.toString
  }

  /**
   * Java Swing pretty printer for Scala abstract syntax trees.
   */
  class SwingBrowser {
    def browse(pName: String, units: Iterator[CompilationUnit]): Unit =
      browse(pName, units.toList)

    /** print the whole program */
    def browse(pName: String, units: List[CompilationUnit]): Unit = {
      var unitList: List[UnitTree] = Nil

      for (i <- units)
        unitList = UnitTree(i) :: unitList
      val tm = new ASTTreeModel(ProgramTree(unitList))

      val frame = new BrowserFrame(pName)
      frame.setTreeModel(tm)

      val lock = new Lock()
      frame.createFrame(lock)

      // wait for the frame to be closed
      lock.acquire()
    }
  }

  /** Tree model for abstract syntax trees */
  class ASTTreeModel(val program: Tree) extends TreeModel {
    var listeners: List[TreeModelListener] = Nil

    /** Add a listener to this tree */
    def addTreeModelListener(l: TreeModelListener): Unit =
      listeners = l :: listeners

    /** Return the index'th child of parent */
    def getChild(parent: AnyRef, index: Int): AnyRef =
      packChildren(parent)(index)

    /** Return the number of children this 'parent' has */
    def getChildCount(parent: AnyRef): Int =
      packChildren(parent).length

    /** Return the index of the given child */
    def getIndexOfChild(parent: AnyRef, child: AnyRef): Int =
      packChildren(parent) indexOf child

    /** Return the root node */
    def getRoot(): AnyRef = program

    /** Test whether the given node is a leaf */
    def isLeaf(node: AnyRef): Boolean = packChildren(node).isEmpty

    def removeTreeModelListener(l: TreeModelListener): Unit =
      listeners = listeners filterNot (_ == l)

    /** we ignore this message for now */
    def valueForPathChanged(path: TreePath, newValue: AnyRef) = ()

    /**
     * Return a list of children for the given node.
     */
    def packChildren(t: AnyRef): List[AnyRef] = TreeInfo.children(t.asInstanceOf[Tree])
  }




  /**
   * A window that can host the Tree widget and provide methods for
   * displaying information
   *
   * @author Iulian Dragos
   * @version 1.0
   */
  class BrowserFrame(phaseName: String = "unknown") {
    try {
      UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel")
    }
    catch {
      case _: Throwable => UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName())
    }

    val frame = new JFrame("Scala AST after " + phaseName + " phase")
    frame.setJMenuBar(new ASTMenuBar())
    val topLeftPane = new JPanel(new BorderLayout())
    val topRightPane = new JPanel(new BorderLayout())
    val bottomPane = new JPanel(new BorderLayout())
    var splitPane: JSplitPane = _
    var treeModel: ASTTreeModel = _
    var jTree: JTree = _
    val textArea: JTextArea = new JTextArea(30, 120)
    textArea.setBorder(BorderFactory.createEmptyBorder(borderSize, borderSize, borderSize, borderSize))

    val infoPanel = new TextInfoPanel()


    private def setExpansionState(root: JTree, expand: Boolean): Unit = {
      def _setExpansionState(root: JTree, path: TreePath): Unit = {
        val last = path.getLastPathComponent
        for (i <- 0 until root.getModel.getChildCount(last)) {
          val child = root.getModel.getChild(last, i)
          val childPath = path pathByAddingChild child
          _setExpansionState(root, childPath)
        }
        if (expand) {jTree expandPath path}
        else {jTree collapsePath path}
      }
      _setExpansionState(root, new TreePath(root.getModel.getRoot))
    }

    def expandAll(subtree: JTree) = setExpansionState(subtree, expand = true)
    def collapseAll(subtree: JTree) = setExpansionState(subtree, expand = false)


    /** Create a frame that displays the AST.
     *
     * @param lock The lock is used in order to stop the compilation thread
     * until the user is done with the tree inspection. Swing creates its
     * own threads when the frame is packed, and therefore execution
     * would continue. However, this is not what we want, as the tree and
     * especially symbols/types would change while the window is visible.
     */
    def createFrame(lock: Lock): Unit = {
      lock.acquire() // keep the lock until the user closes the window

      frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)

      frame.addWindowListener(new WindowAdapter() {
        /** Release the lock, so compilation may resume after the window is closed. */
        override def windowClosed(e: WindowEvent): Unit = lock.release()
      })

      jTree = new JTree(treeModel) {
        /** Return the string for a tree node. */
        override def convertValueToText(value: Any, sel: Boolean,
                                        exp: Boolean, leaf: Boolean,
                                        row: Int, hasFocus: Boolean) = {
            val (cls, name) = TreeInfo.treeName(value.asInstanceOf[Tree])
            if (name != EMPTY)
              cls + "[" + name + "]"
            else
              cls
        }
      }

      jTree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
        def valueChanged(e: javax.swing.event.TreeSelectionEvent): Unit = {
          textArea.setText(e.getPath().getLastPathComponent().toString)
          infoPanel.update(e.getPath().getLastPathComponent())
        }
      })

      val topSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, topLeftPane, topRightPane)
      topSplitPane.setResizeWeight(0.5)

      jTree.setBorder(
        BorderFactory.createEmptyBorder(borderSize, borderSize, borderSize, borderSize))
      topLeftPane.add(new JScrollPane(jTree), BorderLayout.CENTER)
      topRightPane.add(new JScrollPane(infoPanel), BorderLayout.CENTER)
      bottomPane.add(new JScrollPane(textArea), BorderLayout.CENTER)
      textArea.setFont(new Font("monospaced", Font.PLAIN, 14))
      textArea.setEditable(false)

      splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topSplitPane, bottomPane)
      frame.getContentPane().add(splitPane)
      frame.pack()
      frame.setVisible(true)
    }

    class ASTMenuBar extends JMenuBar {
      val menuKey = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
      val shiftKey = InputEvent.SHIFT_MASK
      val jmFile = new JMenu("File")
      // val jmiSaveImage = new JMenuItem(
      //   new AbstractAction("Save Tree Image") {
      //     putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, menuKey, false))
      //     override def actionPerformed(e: ActionEvent) {
      //       //TODO
      //     }
      //   }
      // )

      // jmFile add jmiSaveImage

      def closeWindow() = frame.getToolkit().getSystemEventQueue().postEvent(
        new WindowEvent(frame, WindowEvent.WINDOW_CLOSING))

      val jmiCancel = new JMenuItem (
        new AbstractAction("Cancel Compilation") {
          putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, menuKey + shiftKey, false))
          override def actionPerformed(e: ActionEvent) {
            closeWindow()
            global.currentRun.cancel()
          }
        }
      )
      jmFile add jmiCancel

      val jmiExit = new JMenuItem (
        new AbstractAction("Exit") {
          putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, menuKey, false))
          override def actionPerformed(e: ActionEvent) = closeWindow()
        }
      )
      jmFile add jmiExit
      add(jmFile)

      val jmView = new JMenu("View")
      val jmiExpand = new JMenuItem(
        new AbstractAction("Expand All Nodes") {
          putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_E, menuKey, false))
          override def actionPerformed(e: ActionEvent) {
            expandAll(jTree)
          }
        }
      )
      jmView add jmiExpand
      val jmiCollapse = new JMenuItem(
        new AbstractAction("Collapse All Nodes") {
          putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_L, menuKey, false))
          override def actionPerformed(e: ActionEvent) {
            collapseAll(jTree)
          }
        }
      )
      jmView add jmiCollapse
      add(jmView)
    }

    def setTreeModel(tm: ASTTreeModel): Unit = treeModel = tm
  }

  /**
   * Present detailed information about the selected tree node.
   */
  class TextInfoPanel extends JTextArea(20, 50) {

    setBorder(BorderFactory.createEmptyBorder(borderSize, borderSize, borderSize, borderSize))
    setEditable(false)
    setFont(new Font("monospaced", Font.PLAIN, 12))

    def update(v: AnyRef): Unit = {
      val t: Tree = v.asInstanceOf[Tree]
      val str = new StringBuilder()
      var buf = new StringWriter()

      t match {
        case ProgramTree(_) => ()
        case UnitTree(_)    => ()
        case _ =>
          str.append("tree.id: ").append(t.id)
          str.append("\ntree.pos: ").append(t.pos)
          str.append("\nSymbol: ").append(TreeInfo.symbolText(t))
          str.append("\nSymbol owner: ").append(
            if ((t.symbol ne null) && t.symbol != NoSymbol)
              t.symbol.owner.toString
            else
              "NoSymbol has no owner")
          if ((t.symbol ne null) && t.symbol.isType) {
            str.append("\ntermSymbol: " + t.symbol.tpe.termSymbol
                     + "\ntypeSymbol: " + t.symbol.tpe.typeSymbol)
          if (t.symbol.isTypeSkolem)
            str.append("\nSkolem of: " + t.symbol.deSkolemize)
          }
          str.append("\nSymbol tpe: ")
          if (t.symbol ne null) {
            str.append(t.symbol.tpe).append("\n")
            buf = new StringWriter()
            TypePrinter.toDocument(t.symbol.tpe).format(getWidth() / getColumnWidth(), buf)
            str.append(buf.toString)
          }
          str.append("\n\nSymbol info: \n")
          TreeInfo.symbolTypeDoc(t).format(getWidth() / getColumnWidth(), buf)
          str.append(buf.toString)
          str.append("\n\nSymbol Attributes: \n").append(TreeInfo.symbolAttributes(t))
          str.append("\ntree.tpe: ")
          if (t.tpe ne null) {
            str.append(t.tpe.toString).append("\n")
            buf = new StringWriter()
            TypePrinter.toDocument(t.tpe).format(getWidth() / getColumnWidth(), buf)
            str.append(buf.toString)
          }
      }
      setText(str.toString)
    }
  }

  /** Computes different information about a tree node. It
   *  is used as central place to do all pattern matching against
   *  Tree.
   */
  object TreeInfo {
    /** Return the case class name and the Name, if the node defines one */
    def treeName(t: Tree): (String, Name) = ((t.productPrefix, t match {
      case UnitTree(unit)                  => newTermName("" + unit)
      case Super(_, mix)                   => newTermName("mix: " + mix)
      case This(qual)                      => qual
      case Select(_, selector)             => selector
      case Ident(name)                     => name
      case SelectFromTypeTree(_, selector) => selector
      case x: DefTree                      => x.name
      case _                               => EMPTY
    }))

    /** Return a list of children for the given tree node */
    def children(t: Tree): List[Tree] = t match {
      case ProgramTree(units) =>
        units

      case UnitTree(unit) =>
        List(unit.body)

      case DocDef(comment, definition) =>
        List(definition)

      case ClassDef(mods, name, tparams, impl) => {
        var children: List[Tree] = List()
        children = tparams ::: children
        mods.annotations ::: impl :: children
      }

      case PackageDef(pid, stats) =>
        stats

      case ModuleDef(mods, name, impl) =>
        mods.annotations ::: List(impl)

      case ValDef(mods, name, tpe, rhs) =>
        mods.annotations ::: List(tpe, rhs)

      case DefDef(mods, name, tparams, vparams, tpe, rhs) =>
        mods.annotations ::: tpe :: rhs :: vparams.flatten ::: tparams

      case TypeDef(mods, name, tparams, rhs) =>
        mods.annotations ::: rhs :: tparams // @M: was List(rhs, lobound)

      case Import(expr, selectors) =>
        List(expr)

      case CaseDef(pat, guard, body) =>
        List(pat, guard, body)

      case Template(parents, self, body) =>
        parents ::: List(self) ::: body

      case LabelDef(name, params, rhs) =>
        params ::: List(rhs)

      case Block(stats, expr) =>
        stats ::: List(expr)

      case Alternative(trees) =>
        trees

      case Bind(name, rhs) =>
        List(rhs)

      case UnApply(fun, args) =>
        fun :: args

      case Match(selector, cases) =>
        selector :: cases

      case Function(vparams, body) =>
        vparams ::: List(body)

      case Assign(lhs, rhs) =>
        List(lhs, rhs)

      case If(cond, thenp, elsep) =>
        List(cond, thenp, elsep)

      case Return(expr) =>
        List(expr)

      case Throw(expr) =>
        List(expr)

      case New(init) =>
        List(init)

      case Typed(expr, tpe) =>
        List(expr, tpe)

      case TypeApply(fun, args) =>
        List(fun) ::: args

      case Apply(fun, args) =>
        List(fun) ::: args

      case ApplyDynamic(qual, args) =>
        List(qual) ::: args

      case Super(qualif, mix) =>
        List(qualif)

      case This(qualif) =>
        Nil

      case Select(qualif, selector) =>
        List(qualif)

      case Ident(name) =>
        Nil

      case Literal(value) =>
        Nil

      case TypeTree() =>
        Nil

      case Annotated(annot, arg) =>
        annot :: List(arg)

      case SingletonTypeTree(ref) =>
        List(ref)

      case SelectFromTypeTree(qualif, selector) =>
        List(qualif)

      case CompoundTypeTree(templ) =>
        List(templ)

      case AppliedTypeTree(tpe, args) =>
        tpe :: args

      case TypeBoundsTree(lo, hi) =>
        List(lo, hi)

      case ExistentialTypeTree(tpt, whereClauses) =>
        tpt :: whereClauses

      case Try(block, catches, finalizer) =>
        block :: catches ::: List(finalizer)

      case ArrayValue(elemtpt, elems) =>
        elemtpt :: elems

      case EmptyTree =>
        Nil

      case Star(t) =>
        List(t)
    }

    /** Return a textual representation of this t's symbol */
    def symbolText(t: Tree): String = {
      val prefix =
        if (t.hasSymbolField)  "[has] "
        else if (t.isDef) "[defines] "
        else ""

      prefix + t.symbol
    }

    /** Return t's symbol type  */
    def symbolTypeDoc(t: Tree): Document = {
      val s = t.symbol
      if (s ne null)
        TypePrinter.toDocument(s.info)
      else
        DocNil
    }

    /** Return a textual representation of (some of) the symbol's
     * attributes */
    def symbolAttributes(t: Tree): String = {
      val s = t.symbol

      if ((s ne null) && (s != NoSymbol)) {
        var str = s.flagString
        if (s.isStaticMember) str = str + " isStatic "
        (str + " annotations: " + s.annotations.mkString("", " ", "")
          + (if (s.isTypeSkolem) "\ndeSkolemized annotations: " + s.deSkolemize.annotations.mkString("", " ", "") else ""))
      }
      else ""
    }
  }

  object TypePrinter {

    ///////////////// Document pretty printer ////////////////

    implicit def view(n: String): Document = DocText(n)

    def toDocument(sym: Symbol): Document =
      toDocument(sym.info)

    def symsToDocument(syms: List[Symbol]): Document = syms match {
      case Nil => DocNil
      case s :: Nil => Document.group(toDocument(s))
      case _ =>
        Document.group(
          syms.tail.foldLeft (toDocument(syms.head) :: ", ") (
            (d: Document, s2: Symbol) => toDocument(s2) :: ", " :/: d) )
    }

    def toDocument(ts: List[Type]): Document = ts match {
      case Nil => DocNil
      case t :: Nil => Document.group(toDocument(t))
      case _ =>
        Document.group(
          ts.tail.foldLeft (toDocument(ts.head) :: ", ") (
            (d: Document, t2: Type) => toDocument(t2) :: ", " :/: d) )
    }

    def toDocument(t: Type): Document = t match {
      case ErrorType => "ErrorType()"
      case WildcardType => "WildcardType()"
      case NoType => "NoType()"
      case NoPrefix => "NoPrefix()"
      case ThisType(s) => "ThisType(" + s.name + ")"

      case SingleType(pre, sym) =>
        Document.group(
          Document.nest(4, "SingleType(" :/:
                      toDocument(pre) :: ", " :/: sym.name.toString :: ")")
        )

      case ConstantType(value) =>
         "ConstantType(" + value + ")"

      case TypeRef(pre, sym, args) =>
        Document.group(
          Document.nest(4, "TypeRef(" :/:
                        toDocument(pre) :: ", " :/:
                        sym.name.toString + sym.idString :: ", " :/:
                        "[ " :: toDocument(args) ::"]" :: ")")
        )

      case TypeBounds(lo, hi) =>
        Document.group(
          Document.nest(4, "TypeBounds(" :/:
                        toDocument(lo) :: ", " :/:
                        toDocument(hi) :: ")")
        )

       case RefinedType(parents, defs) =>
        Document.group(
          Document.nest(4, "RefinedType(" :/:
                        toDocument(parents) :: ")")
        )

      case ClassInfoType(parents, defs, clazz) =>
        Document.group(
          Document.nest(4,"ClassInfoType(" :/:
                        toDocument(parents) :: ", " :/:
                        clazz.name.toString + clazz.idString :: ")")
        )

      case MethodType(params, result) =>
        Document.group(
          Document.nest(4, "MethodType(" :/:
                        Document.group("(" :/:
                                       symsToDocument(params) :/:
                                       "), ") :/:
                        toDocument(result) :: ")")
        )

      case NullaryMethodType(result) =>
        Document.group(
          Document.nest(4,"NullaryMethodType(" :/:
                        toDocument(result) :: ")")
        )

      case PolyType(tparams, result) =>
        Document.group(
          Document.nest(4,"PolyType(" :/:
                        Document.group("(" :/:
                                       symsToDocument(tparams) :/:
                                       "), ") :/:
                        toDocument(result) :: ")")
        )

      case AnnotatedType(annots, tp) =>
        Document.group(
          Document.nest(4, "AnnotatedType(" :/:
                        annots.mkString("[", ",", "]") :/:
                        "," :/: toDocument(tp) :: ")")
        )

      case ExistentialType(tparams, result) =>
        Document.group(
            Document.nest(4, "ExistentialType(" :/:
                Document.group("(" :/: symsToDocument(tparams) :/: "), ") :/:
                toDocument(result) :: ")"))

      case ImportType(expr) =>
        "ImportType(" + expr.toString + ")"


      case SuperType(thistpe, supertpe) =>
        Document.group(
          Document.nest(4, "SuperType(" :/:
                        toDocument(thistpe) :/: ", " :/:
                        toDocument(supertpe) ::")"))
      case _ =>
        sys.error("Unknown case: " + t.toString +", "+ t.getClass)
    }
  }

}
