/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.ast

import java.awt.{List => awtList, _}
import java.awt.event._
import java.io.StringWriter

import javax.swing._
import javax.swing.event.TreeModelListener
import javax.swing.tree._

import scala.concurrent.Lock
import scala.text._
import symtab.Flags._
import symtab.SymbolTable

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

  def create(): SwingBrowser = new SwingBrowser();

  /** Pseudo tree class, so that all JTree nodes are treated uniformly */
  case class ProgramTree(units: List[UnitTree]) extends Tree {
    override def toString(): String = "Program"
  }

  /** Pseudo tree class, so that all JTree nodes are treated uniformly */
  case class UnitTree(unit: CompilationUnit) extends Tree {
    override def toString(): String = unit.toString()
  }

  /**
   * Java Swing pretty printer for Scala abstract syntax trees.
   */
  class SwingBrowser {

    def browse(t: Tree): Unit = {
      val tm = new ASTTreeModel(t)

      val frame = new BrowserFrame()
      frame.setTreeModel(tm)

      val lock = new Lock()
      frame.createFrame(lock)

      // wait for the frame to be closed
      lock.acquire
    }

    def browse(units: Iterator[CompilationUnit]): Unit =
      browse(units.toList)

    /** print the whole program */
    def browse(units: List[CompilationUnit]): Unit = {
      var unitList: List[UnitTree] = Nil

      for (i <- units)
        unitList = UnitTree(i) :: unitList
      val tm = new ASTTreeModel(ProgramTree(unitList))

      val frame = new BrowserFrame()
      frame.setTreeModel(tm)

      val lock = new Lock()
      frame.createFrame(lock)

      // wait for the frame to be closed
      lock.acquire
    }
  }

  /** Tree model for abstract syntax trees */
  class ASTTreeModel(val program: Tree) extends TreeModel {
    var listeners: List[TreeModelListener] = Nil

    /** Add a listener to this tree */
    def addTreeModelListener(l: TreeModelListener): Unit =
      listeners = l :: listeners

    /** Return the index'th child of parent */
    def getChild(parent: Any, index: Int): AnyRef =
      packChildren(parent).drop(index).head

    /** Return the number of children this 'parent' has */
    def getChildCount(parent: Any): Int =
      packChildren(parent).length

    /** Return the index of the given child */
    def getIndexOfChild(parent: Any, child: Any): Int =
      packChildren(parent).dropWhile(c => c != child).length

    /** Return the root node */
    def getRoot(): AnyRef = program

    /** Test whether the given node is a leaf */
    def isLeaf(node: Any): Boolean = packChildren(node).length == 0

    def removeTreeModelListener(l: TreeModelListener): Unit =
      listeners remove (x => x == l)

    /** we ignore this message for now */
    def valueForPathChanged(path: TreePath, newValue: Any) = ()

    /**
     * Return a list of children for the given node.
     */
    def packChildren(t: Any): List[AnyRef] =
        TreeInfo.children(t.asInstanceOf[Tree])
  }


  /**
   * A window that can host the Tree widget and provide methods for
   * displaying information
   *
   * @author Iulian Dragos
   * @version 1.0
   */
  class BrowserFrame {
    val frame = new JFrame("Scala AST")
    val topLeftPane = new JPanel(new BorderLayout())
    val topRightPane = new JPanel(new BorderLayout())
    val bottomPane = new JPanel(new BorderLayout())
    var splitPane: JSplitPane = _
    var treeModel: TreeModel = _

    val textArea: JTextArea = new JTextArea(20, 150)
    val infoPanel = new TextInfoPanel()

    /** Create a frame that displays the AST.
     *
     * @param lock The lock is used in order to stop the compilation thread
     * until the user is done with the tree inspection. Swing creates its
     * own threads when the frame is packed, and therefore execution
     * would continue. However, this is not what we want, as the tree and
     * especially symbols/types would change while the window is visible.
     */
    def createFrame(lock: Lock): Unit = {
      lock.acquire // keep the lock until the user closes the window

      frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)

      frame.addWindowListener(new WindowAdapter() {
        /** Release the lock, so compilation may resume after the window is closed. */
        override def windowClosed(e: WindowEvent): Unit = lock.release
      });

      val tree = new JTree(treeModel) {
        /** Return the string for a tree node. */
        override def convertValueToText(value: Any, sel: Boolean,
                                        exp: Boolean, leaf: Boolean,
                                        row: Int, hasFocus: Boolean) = {
            val (cls, name) = TreeInfo.treeName(value.asInstanceOf[Tree])
            if (name != EMPTY)
              cls + "[" + name.toString() + "]"
            else
              cls
        }
      }

      tree.addTreeSelectionListener(new javax.swing.event.TreeSelectionListener() {
        def valueChanged(e: javax.swing.event.TreeSelectionEvent): Unit = {
          textArea.setText(e.getPath().getLastPathComponent().toString())
          infoPanel.update(e.getPath().getLastPathComponent())
        }
      })

      val topSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, topLeftPane, topRightPane)
      topSplitPane.setResizeWeight(0.5)

      topLeftPane.add(new JScrollPane(tree), BorderLayout.CENTER)
      topRightPane.add(new JScrollPane(infoPanel), BorderLayout.CENTER)

      bottomPane.add(new JScrollPane(textArea), BorderLayout.CENTER)
      textArea.setFont(new Font("monospaced", Font.PLAIN, 14))
      textArea.setEditable(false)

      splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topSplitPane, bottomPane)
      frame.getContentPane().add(splitPane)
      frame.pack()
      frame.setVisible(true)
    }

    def setTreeModel(tm: TreeModel): Unit = treeModel = tm
  }

  /**
   * Present detailed information about the selected tree node.
   */
  class TextInfoPanel extends JTextArea(30, 40) {

    setFont(new Font("monospaced", Font.PLAIN, 12))

    def update(v: AnyRef): Unit = {
      val t: Tree = v.asInstanceOf[Tree]
      val str = new StringBuilder()
      var buf = new StringWriter()

      t match {
        case ProgramTree(_) => ()
        case UnitTree(_)    => ()
        case _ =>
          str.append("tree.pos: ").append(t.pos)
          str.append("\nSymbol: ").append(TreeInfo.symbolText(t))
          str.append("\nSymbol info: \n")
          TreeInfo.symbolTypeDoc(t).format(getWidth() / getColumnWidth(), buf)
          str.append(buf.toString())
          str.append("\nSymbol tpe: ")
          if (t.symbol ne null) {
            str.append(t.symbol.tpe).append("\n")
            buf = new StringWriter()
            TypePrinter.toDocument(t.symbol.tpe).format(getWidth() / getColumnWidth(), buf)
            str.append(buf.toString())
          }
          str.append("\nSymbol Attributes: \n").append(TreeInfo.symbolAttributes(t))
          str.append("\ntree.tpe: ")
          if (t.tpe ne null) {
            str.append(t.tpe.toString()).append("\n")
            buf = new StringWriter()
            TypePrinter.toDocument(t.tpe).format(getWidth() / getColumnWidth(), buf)
            str.append(buf.toString())
          }
      }
      setText(str.toString())
    }
  }


  /** Computes different information about a tree node. It
   *  is used as central place to do all pattern matching against
   *  Tree.
   */
  object TreeInfo {

    /** Return the case class name and the Name, if the node defines one */
    def treeName(t: Tree): (String, Name) = t match {
      case ProgramTree(units) =>
        ("Program", EMPTY)

      case UnitTree(unit) =>
        ("CompilationUnit", unit.toString())

      case DocDef(comment, definition) =>
        ("DocDef", EMPTY)

      case ClassDef(mods, name, tparams, impl) =>
        ("ClassDef", name)

      case PackageDef(packaged, impl) =>
        ("PackageDef", EMPTY)

      case ModuleDef(mods, name, impl) =>
        ("ModuleDef", name)

      case ValDef(mods, name, tpe, rhs) =>
        ("ValDef", name)

      case DefDef(mods, name, tparams, vparams, tpe, rhs) =>
        ("DefDef", name)

      case TypeDef(mods, name, tparams, rhs) =>
        ("TypeDef", name)

      case Import(expr, selectors) =>
        ("Import", EMPTY)

      case CaseDef(pat, guard, body) =>
        ("CaseDef", EMPTY)

      case Template(parents, self, body) =>
        ("Template", EMPTY)

      case LabelDef(name, params, rhs) =>
        ("LabelDef", name)

      case Block(stats, expr) =>
        ("Block", EMPTY)

      case Sequence(trees) =>
        ("Sequence", EMPTY)

      case Alternative(trees) =>
        ("Alternative", EMPTY)

      case Bind(name, rhs) =>
        ("Bind", name)

      case UnApply(fun, args) =>
        ("UnApply", EMPTY)

      case Match(selector, cases) =>
        ("Visitor", EMPTY)

      case Function(vparams, body) =>
        ("Function", EMPTY)

      case Assign(lhs, rhs) =>
        ("Assign", EMPTY)

      case If(cond, thenp, elsep) =>
        ("If", EMPTY)

      case Return(expr) =>
        ("Return", EMPTY)

      case Throw(expr) =>
        ("Throw", EMPTY)

      case New(init) =>
        ("New", EMPTY)

      case Typed(expr, tpe) =>
        ("Typed", EMPTY)

      case TypeApply(fun, args) =>
        ("TypeApply", EMPTY)

      case Apply(fun, args) =>
        ("Apply", EMPTY)

      case ApplyDynamic(qual, args) =>
        ("Apply", EMPTY)

      case Super(qualif, mix) =>
        ("Super", qualif.toString() + ", mix: " + mix.toString())

      case This(qualifier) =>
        ("This", qualifier)

      case Select(qualifier, selector) =>
        ("Select", selector)

      case Ident(name) =>
        ("Ident", name)

      case Literal(value) =>
        ("Literal", EMPTY)

      case TypeTree() =>
        ("TypeTree", EMPTY)

      case Annotated(annot, arg) =>
        ("Annotated", EMPTY)

      case Annotation(constr, elements) =>
        ("Annotation", EMPTY)

      case SingletonTypeTree(ref) =>
        ("SingletonType", EMPTY)

      case SelectFromTypeTree(qualifier, selector) =>
        ("SelectFromType", selector)

      case CompoundTypeTree(template) =>
        ("CompoundType", EMPTY)

      case AppliedTypeTree(tpe, args) =>
        ("AppliedType", EMPTY)

      case TypeBoundsTree(lo, hi) =>
        ("TypeBoundsTree", EMPTY)

      case ExistentialTypeTree(tpt, whereClauses) =>
        ("ExistentialTypeTree", EMPTY)

      case Try(block, catcher, finalizer) =>
        ("Try", EMPTY)

      case EmptyTree =>
        ("Empty", EMPTY)

      case ArrayValue(elemtpt, trees) =>
        ("ArrayValue", EMPTY)

      case Star(t) =>
        ("Star", EMPTY)
    }

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

      case PackageDef(name, stats) =>
        stats

      case ModuleDef(mods, name, impl) =>
        mods.annotations ::: List(impl)

      case ValDef(mods, name, tpe, rhs) =>
        mods.annotations ::: List(tpe, rhs)

      case DefDef(mods, name, tparams, vparams, tpe, rhs) => {
        var children: List[Tree] = List()
        children = tparams ::: children
        children = List.flatten(vparams) ::: children
        mods.annotations ::: tpe :: rhs :: children
      }

      case TypeDef(mods, name, tparams, rhs) =>
        mods.annotations ::: rhs :: tparams // @M: was List(rhs, lobound)

      case Import(expr, selectors) => {
        var children: List[Tree] = List(expr)
        children
      }

      case CaseDef(pat, guard, body) =>
        List(pat, guard, body)

      case Template(parents, self, body) =>
        parents ::: List(self) ::: body

      case LabelDef(name, params, rhs) =>
        params ::: List(rhs)

      case Block(stats, expr) =>
        stats ::: List(expr)

      case Sequence(trees) =>
        trees

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
        Nil

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
        annot.constr :: annot.elements ::: List(arg)

      case Annotation(constr, elements) =>
        constr :: elements

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
      var prefix = ""

      if (t.hasSymbol)
        prefix = "[has] "
      if (t.isDef)
        prefix = "[defines] "

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
      var att = ""

      if ((s ne null) && (s != NoSymbol)) {
        var str = flagsToString(s.flags)
        if (s.isStaticMember) str = str + " isStatic ";
        str
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
                      toDocument(pre) :: ", " :/: sym.name.toString() :: ")")
        )

      case ConstantType(value) =>
         "ConstantType(" + value + ")"

      case TypeRef(pre, sym, args) =>
        Document.group(
          Document.nest(4, "TypeRef(" :/:
                        toDocument(pre) :: ", " :/:
                        sym.name.toString() :: ", " :/:
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
                        clazz.name.toString() :: ")")
        )

      case MethodType(params, result) =>
        Document.group(
          Document.nest(4, "MethodType(" :/:
                        Document.group("(" :/:
                                       symsToDocument(params) :/:
                                       "), ") :/:
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

      case AnnotatedType(attribs, tp, _) =>
        Document.group(
          Document.nest(4, "AnnotatedType(" :/:
                        attribs.mkString("[", ",", "]") :/:
                        "," :/: toDocument(tp) :: ")")
        )

      case ExistentialType(tparams, result) =>
        Document.group(
            Document.nest(4, "ExistentialType(" :/:
                Document.group("(" :/: symsToDocument(tparams) :/: "), ") :/:
                toDocument(result) :: ")"))

      case global.analyzer.ImportType(expr) =>
        "ImportType(" + expr.toString + ")"


      case SuperType(thistpe, supertpe) =>
        Document.group(
          Document.nest(4, "SuperType(" :/:
                        toDocument(thistpe) :/: ", " :/:
                        toDocument(supertpe) ::")"))
      case _ =>
        throw new Error("Unknown case: " + t.toString() +", "+ t.getClass)
    }
  }

}
