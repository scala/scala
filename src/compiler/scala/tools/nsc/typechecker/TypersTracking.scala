/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import Mode._

trait TypersTracking {
  self: Analyzer =>

  import global._
  import typeDebug._

  // To enable decent error messages when the typer crashes.
  // TODO - this only catches trees which go through def typed,
  // but there are all kinds of back ways - typedClassDef, etc. etc.
  // Funnel everything through one doorway.
  var lastTreeToTyper: Tree = EmptyTree

  def fullSiteString(context: Context): String = {
    def owner_long_s = (
      if (settings.debug.value) {
        def flags_s = context.owner.debugFlagString match {
          case "" => ""
          case s  => " with flags " + inLightMagenta(s)
        }
        s", a ${context.owner.shortSymbolClass}$flags_s"
      }
      else ""
    )
    def marker = if (context.bufferErrors) "silent" else "site"
    def undet_s = context.undetparams match {
      case Nil => ""
      case ps  => ps.mkString(" solving: ", ",", "")
    }
    def implicits_s = (
      if (context.enrichmentEnabled)
        if (context.implicitsEnabled) ""
        else inLightRed("enrichment only")
      else inLightRed("implicits disabled")
    )

    s"($marker$undet_s: ${context.siteString}$owner_long_s) $implicits_s"
  }

  object typingStack {
    val out = new java.io.PrintWriter(System.err, true)

    // TODO - account for colors so the color of a multiline string
    // doesn't infect the connector lines
    private def currentIndent = "|    " * depth

    private var trees: List[Frame] = Nil
    private var depth = 0
    private def atLowerIndent[T](body: => T): T = {
      depth -= 1
      try body finally depth += 1
    }
    private def resetIfEmpty(s: String) = if (trees.isEmpty) resetColor(s) else s

    private def truncAndOneLine(s: String): String = {
      val s1 = s.replaceAll("\\s+", " ")
      if (s1.length < 60 || settings.debug.value) s1 else s1.take(57) + "..."
    }

    private class Frame(val tree: Tree) { }
    private def greenType(tp: Type): String = tpe_s(tp, inGreen)
    private def greenType(tree: Tree): String = tree match {
      case null                              => "[exception]"
      case md: MemberDef if md.tpe == NoType => inBlue(s"[${md.keyword} ${md.name}]") + " " + greenType(md.symbol.tpe)
      case _ if tree.tpe.isComplete          => greenType(tree.tpe)
      case _                                 => "<?>"
    }
    def indented(s: String): String =
      if (s == "") "" else currentIndent + s.replaceAll("\n", "\n" + currentIndent)

    @inline final def runWith[T](t: Tree)(body: => T): T = {
      push(t)
      try body finally pop(t)
    }
    def push(t: Tree): Unit = {
      trees ::= new Frame(t)
      depth += 1
    }
    def pop(t: Tree): Unit = {
      val frame = trees.head
      assert(frame.tree eq t, ((frame.tree, t)))
      trees = trees.tail
      depth -= 1
    }
    def show(s: String)     { if (s != "") out.println(s) }

    def showPush(tree: Tree, context: Context) {
      showPush(tree, NOmode, WildcardType, context)
    }
    def showPush(tree: Tree, mode: Mode, pt: Type, context: Context) {
      def tree_s = truncAndOneLine(ptTree(tree))
      def pt_s = if (pt.isWildcard || context.inTypeConstructorAllowed) "" else s": pt=$pt"
      def all_s = List(tree_s, pt_s, mode, fullSiteString(context)) filterNot (_ == "") mkString " "

      atLowerIndent(show(indented("""|-- """ + all_s)))
    }
    def showPop(typedTree: Tree): Tree = {
      val s = greenType(typedTree)
      show(resetIfEmpty(indented("""\-> """ + s)))
      typedTree
    }
    def showAdapt(original: Tree, adapted: Tree, pt: Type, context: Context) {
      if (!noPrintAdapt(original, adapted)) {
        def tree_s1 = inLightCyan(truncAndOneLine(ptTree(original)))
        def pt_s = if (pt.isWildcard) "" else s" based on pt $pt"
        def tree_s2 = adapted match {
          case tt: TypeTree => "is now a TypeTree(" + tpe_s(tt.tpe, inCyan) + ")"
          case _            => "adapted to " + inCyan(truncAndOneLine(ptTree(adapted))) + pt_s
        }
        show(indented(s"[adapt] $tree_s1 $tree_s2"))
      }
    }
    def showTyped(tree: Tree) {
      def class_s = tree match {
        case _: RefTree => ""
        case _          => " " + tree.shortClass
      }
      if (!noPrintTyping(tree))
        show(indented(s"[typed$class_s] " + truncAndOneLine(ptTree(tree))))
    }

    def nextTyped(tree: Tree, mode: Mode, pt: Type, context: Context)(body: => Tree): Tree =
      nextTypedInternal(tree, showPush(tree, mode, pt, context))(body)

    def nextTypedInternal(tree: Tree, pushFn: => Unit)(body: => Tree): Tree = (
      if (noPrintTyping(tree))
        body
      else
        runWith(tree) { pushFn ; showPop(body) }
    )

    @inline final def printTyping(tree: Tree, s: => String) = {
      if (printTypings && !noPrintTyping(tree))
        show(indented(s))
    }
    @inline final def printTyping(s: => String) = {
      if (printTypings)
        show(indented(s))
    }
  }
  def tpe_s(tp: Type, colorize: String => String): String = tp match {
    case OverloadedType(pre, alts) => alts map (alt => tpe_s(pre memberType alt, colorize)) mkString " <and> "
    case _                         => colorize(tp.toLongString)
  }
  // def sym_s(s: Symbol) = if (s eq null) "" + s else s.getClass.getName split '.' last;

  // Some trees which are typed with mind-numbing frequency and
  // which add nothing by being printed. Did () type to Unit? Let's
  // gamble on yes.
  def printingOk(t: Tree) = printTypings && (settings.debug.value || !noPrint(t))
  def noPrintTyping(t: Tree) = (t.tpe ne null) || !printingOk(t)
  def noPrintAdapt(tree1: Tree, tree2: Tree) = !printingOk(tree1) || (
       (tree1.tpe == tree2.tpe)
    && (tree1.symbol == tree2.symbol)
  )
}
