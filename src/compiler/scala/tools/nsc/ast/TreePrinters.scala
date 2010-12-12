/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import compat.Platform.{EOL => LINE_SEPARATOR}
import java.io.{ OutputStream, PrintWriter, StringWriter, Writer }
import symtab.Flags._
import symtab.SymbolTable

trait TreePrinters { trees: SymbolTable =>

  import treeInfo.{ IsTrue, IsFalse }

  final val showOuterTests = false

  /** Adds backticks if the name is a scala keyword. */
  def quotedName(name: Name, decode: Boolean): String = {
    val s = if (decode) name.decode else name.toString
    val term = name.toTermName
    if (nme.keywords(term) && term != nme.USCOREkw) "`%s`" format s
    else s
  }
  def quotedName(name: Name): String = quotedName(name, false)

  /** Turns a path into a String, introducing backquotes
   *  as necessary.
   */
  def backquotedPath(t: Tree): String = t match {
    case Select(qual, name) => "%s.%s".format(backquotedPath(qual), quotedName(name))
    case Ident(name)        => quotedName(name)
    case t                  => t.toString
  }

  class TreePrinter(out: PrintWriter) extends trees.AbsTreePrinter(out) {
    protected var indentMargin = 0
    protected val indentStep = 2
    protected var indentString = "                                        " // 40

    def flush() = out.flush()

    def indent = indentMargin += indentStep
    def undent = indentMargin -= indentStep

    protected def doPrintPositions = settings.Xprintpos.value
    def printPosition(tree: Tree) = if (doPrintPositions) print(tree.pos.show)

    def println() {
      out.println()
      while (indentMargin > indentString.length())
        indentString += indentString
      if (indentMargin > 0)
        out.write(indentString, 0, indentMargin)
    }

    def printSeq[a](ls: List[a])(printelem: a => Unit)(printsep: => Unit) {
      ls match {
        case List() =>
        case List(x) => printelem(x)
        case x :: rest => printelem(x); printsep; printSeq(rest)(printelem)(printsep)
      }
    }

    def printColumn(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); indent; println()
      printSeq(ts){print}{print(sep); println()}; undent; println(); print(end)
    }

    def printRow(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); printSeq(ts){print}{print(sep)}; print(end)
    }

    def printRow(ts: List[Tree], sep: String) { printRow(ts, "", sep, "") }

    def printTypeParams(ts: List[TypeDef]) {
      if (!ts.isEmpty) {
        print("["); printSeq(ts){ t =>
          printAnnotations(t)
          printParam(t)
        }{print(", ")}; print("]")
      }
    }

    def printValueParams(ts: List[ValDef]) {
      print("(")
      if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
      printSeq(ts){printParam}{print(", ")}
      print(")")
    }

    def printParam(tree: Tree) {
      tree match {
        case ValDef(mods, name, tp, rhs) =>
          printPosition(tree)
          printAnnotations(tree)
          print(symName(tree, name)); printOpt(": ", tp); printOpt(" = ", rhs)
        case TypeDef(mods, name, tparams, rhs) =>
          printPosition(tree)
          print(symName(tree, name))
          printTypeParams(tparams); print(rhs)
      }
    }

    def printBlock(tree: Tree) {
      tree match {
        case Block(_, _) =>
          print(tree)
        case _ =>
          printColumn(List(tree), "{", ";", "}")
      }
    }

    private def symFn[T](tree: Tree, f: Symbol => T, orElse: => T): T = tree.symbol match {
      case null | NoSymbol  => orElse
      case sym              => f(sym)
    }
    private def ifSym(tree: Tree, p: Symbol => Boolean) = symFn(tree, p, false)

    private def symNameInternal(tree: Tree, name: Name, decoded: Boolean): String = {
      def nameFn(sym: Symbol) = {
        val prefix = if (sym.isMixinConstructor) "/*%s*/".format(quotedName(sym.owner.name, decoded)) else ""
        prefix + tree.symbol.nameString
      }
      symFn(tree, nameFn, quotedName(name, decoded))
    }

    def decodedSymName(tree: Tree, name: Name) = symNameInternal(tree, name, true)
    def symName(tree: Tree, name: Name) = symNameInternal(tree, name, false)

    def printOpt(prefix: String, tree: Tree) {
      if (!tree.isEmpty) { print(prefix); print(tree) }
    }

    def printModifiers(tree: Tree, mods: Modifiers): Unit = printFlags(
       if (tree.symbol == NoSymbol) mods.flags else tree.symbol.flags, "" + (
         if (tree.symbol == NoSymbol) mods.privateWithin
         else if (tree.symbol.hasAccessBoundary) tree.symbol.privateWithin.name
         else ""
      )
    )

    def printFlags(flags: Long, privateWithin: String) {
      var mask: Long = if (settings.debug.value) -1L else PrintableFlags
      val s = flagsToString(flags & mask, privateWithin)
      if (s != "") print(s + " ")
    }

    def printAnnotations(tree: Tree) {
      val annots =
        if (tree.symbol.rawAnnotations.nonEmpty) tree.symbol.annotations
        else tree.asInstanceOf[MemberDef].mods.annotations

      annots foreach (annot => print("@"+annot+" "))
    }

    def print(str: String) { out.print(str) }
    def print(name: Name) { print(quotedName(name)) }

    private var currentOwner: Symbol = NoSymbol
    private var selectorType: Type = NoType

    def printRaw(tree: Tree) {
      tree match {
        case EmptyTree =>
          print("<empty>")

        case ClassDef(mods, name, tparams, impl) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          val word =
            if (mods.hasTraitFlag) "trait"
            else if (ifSym(tree, _.isModuleClass)) "object"
            else "class"

          print(word + " " + symName(tree, name))
          printTypeParams(tparams)
          print(if (mods.isDeferred) " <: " else " extends "); print(impl) // (part of DEVIRTUALIZE)

        case PackageDef(packaged, stats) =>
          printAnnotations(tree)
          print("package "); print(packaged); printColumn(stats, " {", ";", "}")

        case ModuleDef(mods, name, impl) =>
          printAnnotations(tree)
          printModifiers(tree, mods); print("object " + symName(tree, name))
          print(" extends "); print(impl)

        case ValDef(mods, name, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print(if (mods.isMutable) "var " else "val ")
          print(symName(tree, name))
          printOpt(": ", tp)
          if (!mods.isDeferred) {
            print(" = ")
            if (rhs.isEmpty) print("_") else print(rhs)
          }

        case DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print("def " + symName(tree, name))
          printTypeParams(tparams); vparamss foreach printValueParams
          printOpt(": ", tp); printOpt(" = ", rhs)

        case TypeDef(mods, name, tparams, rhs) =>
          if (mods hasFlag (PARAM | DEFERRED)) {
            printAnnotations(tree)
            printModifiers(tree, mods); print("type "); printParam(tree)
          } else {
            printAnnotations(tree)
            printModifiers(tree, mods); print("type " + symName(tree, name))
            printTypeParams(tparams); printOpt(" = ", rhs)
          }

        case LabelDef(name, params, rhs) =>
          print(symName(tree, name)); printRow(params, "(", ",", ")"); printBlock(rhs)

        case Import(expr, selectors) =>
          // Is this selector remapping a name (i.e, {name1 => name2})
          def isNotRemap(s: ImportSelector) : Boolean = (s.name == nme.WILDCARD || s.name == s.rename)
          def selectorToString(s: ImportSelector): String = {
            val from = quotedName(s.name)
            if (isNotRemap(s)) from
            else from + "=>" + quotedName(s.rename)
          }
          print("import "); print(backquotedPath(expr))
          print(".")
          selectors match {
            case List(s) =>
              // If there is just one selector and it is not remapping a name, no braces are needed
              if (isNotRemap(s)) {
                print(selectorToString(s))
              } else {
                print("{"); print(selectorToString(s)); print("}")
              }
              // If there is more than one selector braces are always needed
            case many =>
              print(many.map(selectorToString).mkString("{", ", ", "}"))
          }

        case DocDef(comment, definition) =>
          print(comment.raw); println(); print(definition)

        case Template(parents, self, body) =>
          val currentOwner1 = currentOwner
          if (tree.symbol != NoSymbol) currentOwner = tree.symbol.owner
          printRow(parents, " with ")
          if (!body.isEmpty) {
            if (self.name != nme.WILDCARD) {
              print(" { "); print(self.name); printOpt(": ", self.tpt); print(" => ")
            } else if (!self.tpt.isEmpty) {
              print(" { _ : "); print(self.tpt); print(" => ")
            } else {
              print(" {")
            }
            printColumn(body, "", ";", "}")
          }
          currentOwner = currentOwner1

        case Block(stats, expr) =>
          printColumn(stats ::: List(expr), "{", ";", "}")

        case Match(selector, cases) =>
          val selectorType1 = selectorType
          selectorType = selector.tpe
          print(selector); printColumn(cases, " match {", "", "}")
          selectorType = selectorType1

        case CaseDef(pat, guard, body) =>
          print("case ")
          def patConstr(pat: Tree): Tree = pat match {
            case Apply(fn, args) => patConstr(fn)
            case _ => pat
          }
          if (showOuterTests &&
              needsOuterTest(
                patConstr(pat).tpe.finalResultType, selectorType, currentOwner))
            print("???")
          print(pat); printOpt(" if ", guard)
          print(" => "); print(body)

        case Alternative(trees) =>
          printRow(trees, "(", "| ", ")")

        case Star(elem) =>
          print("("); print(elem); print(")*")

        case Bind(name, t) =>
          print("("); print(symName(tree, name)); print(" @ "); print(t); print(")")

        case UnApply(fun, args) =>
          print(fun); print(" <unapply> "); printRow(args, "(", ", ", ")")

        case ArrayValue(elemtpt, trees) =>
          print("Array["); print(elemtpt); printRow(trees, "]{", ", ", "}")

        case Function(vparams, body) =>
          print("("); printValueParams(vparams); print(" => "); print(body); print(")")
          if (settings.uniqid.value && tree.symbol != null) print("#"+tree.symbol.id)

        case Assign(lhs, rhs) =>
          print(lhs); print(" = "); print(rhs)

        case AssignOrNamedArg(lhs, rhs) =>
          print(lhs); print(" = "); print(rhs)

        case If(cond, thenp, elsep) =>
          print("if ("); print(cond); print(")"); indent; println()
          print(thenp); undent
          if (!elsep.isEmpty) {
            println(); print("else"); indent; println(); print(elsep); undent
          }

        case Return(expr) =>
          print("return "); print(expr)

        case Try(block, catches, finalizer) =>
          print("try "); printBlock(block)
          if (!catches.isEmpty) printColumn(catches, " catch {", "", "}")
          printOpt(" finally ", finalizer)

        case Throw(expr) =>
          print("throw "); print(expr)

        case New(tpe) =>
          print("new "); print(tpe)

        case Typed(expr, tp) =>
          print("("); print(expr); print(": "); print(tp); print(")")

        case TypeApply(fun, targs) =>
          print(fun); printRow(targs, "[", ", ", "]")

        case Apply(fun, vargs) =>
          print(fun); printRow(vargs, "(", ", ", ")")

        case ApplyDynamic(qual, vargs) =>
          print("<apply-dynamic>("); print(qual); print("#"); print(tree.symbol.nameString)
          printRow(vargs, ", (", ", ", "))")

        case Super(qual, mix) =>
          if (!qual.isEmpty || tree.symbol != NoSymbol) print(symName(tree, qual) + ".")
          print("super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case This(qual) =>
          if (!qual.isEmpty) print(symName(tree, qual) + ".")
          print("this")

        case Select(qual @ New(tpe), name) if (!settings.debug.value) =>
          print(qual)

        case Select(qualifier, name) =>
          print(backquotedPath(qualifier)); print("."); print(symName(tree, name))

        case Ident(name) =>
          print(symName(tree, name))

        case Literal(x) =>
          print(x.escapedStringValue)

        case tt: TypeTree =>
          if ((tree.tpe eq null) || (settings.Xprintpos.value && tt.original != null)) {
            if (tt.original != null) { print("<type: "); print(tt.original); print(">") }
            else print("<type ?>")
          } else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass) {
            print(tree.tpe.typeSymbol.toString())
          } else {
            print(tree.tpe.toString())
          }

        case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot() {
            print("@"); print(tpt)
            if (!args.isEmpty)
              printRow(args, "(", ",", ")")
          }
          if (tree.isType) { print(tree); print(" "); printAnnot() }
          else { print(tree); print(": "); printAnnot() }

        case SingletonTypeTree(ref) =>
          print(ref); print(".type")

        case SelectFromTypeTree(qualifier, selector) =>
          print(qualifier); print("#"); print(symName(tree, selector))

        case CompoundTypeTree(templ) =>
          print(templ)

        case AppliedTypeTree(tp, args) =>
          print(tp); printRow(args, "[", ", ", "]")

        case TypeBoundsTree(lo, hi) =>
          printOpt(" >: ", lo); printOpt(" <: ", hi)

        case ExistentialTypeTree(tpt, whereClauses) =>
          print(tpt);
          printColumn(whereClauses, " forSome { ", ";", "}")

        case SelectFromArray(qualifier, name, _) =>
          print(qualifier); print(".<arr>"); print(symName(tree, name))

        case TypeTreeWithDeferredRefCheck() =>
          print("<tree with deferred refcheck>")

        case tree =>
          print("<unknown tree of class "+tree.getClass+">")
      }
      if (settings.printtypes.value && tree.isTerm && !tree.isEmpty) {
        print("{"); print(if (tree.tpe eq null) "<null>" else tree.tpe.toString()); print("}")
      }
    }

    def print(tree: Tree) {
      printPosition(tree)
      printRaw(
        if (tree.isDef && tree.symbol != NoSymbol && tree.symbol.isInitialized) {
          tree match {
            case ClassDef(_, _, _, impl @ Template(ps, emptyValDef, body))
            if (tree.symbol.thisSym != tree.symbol) =>
              ClassDef(tree.symbol, Template(ps, ValDef(tree.symbol.thisSym), body))
            case ClassDef(_, _, _, impl)           => ClassDef(tree.symbol, impl)
            case ModuleDef(_, _, impl)             => ModuleDef(tree.symbol, impl)
            case ValDef(_, _, _, rhs)              => ValDef(tree.symbol, rhs)
            case DefDef(_, _, _, vparamss, _, rhs) => DefDef(tree.symbol, vparamss, rhs)
            case TypeDef(_, _, _, rhs)             => TypeDef(tree.symbol, rhs)
            case _ => tree
          }
        } else tree)
    }

    def print(unit: CompilationUnit) {
      print("// Scala source: " + unit.source + LINE_SEPARATOR)
      if (unit.body ne null) {
        print(unit.body); println()
      } else {
        print("<null>")
      }
      println(); flush
    }
  }

  /** A tree printer which is stingier about vertical whitespace and unnecessary
   *  punctuation than the standard one.
   */
  class CompactTreePrinter(out: PrintWriter) extends TreePrinter(out) {
    override def printRow(ts: List[Tree], start: String, sep: String, end: String) {
      print(start)
      printSeq(ts)(print)(print(sep))
      print(end)
    }

    // drill down through Blocks and pull out the real statements.
    def allStatements(t: Tree): List[Tree] = t match {
      case Block(stmts, expr) => (stmts flatMap allStatements) ::: List(expr)
      case _                  => List(t)
    }

    def printLogicalOr(t1: (Tree, Boolean), t2: (Tree, Boolean)) =
      printLogicalOp(t1, t2, "||")

    def printLogicalAnd(t1: (Tree, Boolean), t2: (Tree, Boolean)) =
      printLogicalOp(t1, t2, "&&")

    def printLogicalOp(t1: (Tree, Boolean), t2: (Tree, Boolean), op: String) = {
      def maybenot(tvalue: Boolean) = if (tvalue) "" else "!"

      print("%s(" format maybenot(t1._2))
      printRaw(t1._1)
      print(") %s %s(".format(op, maybenot(t2._2)))
      printRaw(t2._1)
      print(")")
    }

    override def printRaw(tree: Tree): Unit = {
      // routing supercalls through this for debugging ease
      def s() = super.printRaw(tree)

      tree match {
        // labels used for jumps - does not map to valid scala code
        case LabelDef(name, params, rhs) =>
          print("labeldef %s(%s) = ".format(name, params mkString ","))
          printRaw(rhs)

        case Ident(name) =>
          print(decodedSymName(tree, name))

        // target.method(arg) ==> target method arg
        case Apply(Select(target, method), List(arg)) =>
          if (method.decode.toString == "||")
            printLogicalOr(target -> true, arg -> true)
          else if (method.decode.toString == "&&")
            printLogicalAnd(target -> true, arg -> true)
          else (target, arg) match {
            case (_: Ident, _: Literal | _: Ident)  =>
              printRaw(target)
              print(" ")
              printRaw(Ident(method))
              print(" ")
              printRaw(arg)
            case _                        => s()
          }

        // target.unary_! ==> !target
        case Select(qualifier, name) if (name.decode startsWith "unary_") =>
          print(name.decode drop 6)
          printRaw(qualifier)

        case Select(qualifier, name) =>
          printRaw(qualifier)
          print(".")
          print(quotedName(name, true))

        // target.toString() ==> target.toString
        case Apply(fn, Nil)   => printRaw(fn)

        // if a Block only continues one actual statement, just print it.
        case Block(stats, expr) =>
          allStatements(tree) match {
            case List(x)            => printRaw(x)
            case xs                 => s()
          }

        // We get a lot of this stuff
        case If( IsTrue(), x, _)        => printRaw(x)
        case If(IsFalse(), _, x)        => printRaw(x)

        case If(cond,  IsTrue(), elsep)   =>  printLogicalOr(cond -> true, elsep -> true)
        case If(cond, IsFalse(), elsep)   => printLogicalAnd(cond -> false, elsep -> true)
        case If(cond,  thenp, IsTrue())   =>  printLogicalOr(cond -> false, thenp -> true)
        case If(cond,  thenp, IsFalse())  => printLogicalAnd(cond -> true, thenp -> true)

        // If thenp or elsep has only one statement, it doesn't need more than one line.
        case If(cond, thenp, elsep) =>
          def ifIndented(x: Tree) = {
            indent ; println() ; printRaw(x) ; undent
          }

          val List(thenStmts, elseStmts) = List(thenp, elsep) map allStatements
          print("if ("); print(cond); print(")")

          thenStmts match {
            case List(x: If)  => ifIndented(x)
            case List(x)      => printRaw(x)
            case _            => printRaw(thenp)
          }

          if (elseStmts.nonEmpty) {
            print("else")
            indent ; println()
            elseStmts match {
              case List(x)  => printRaw(x)
              case _        => printRaw(elsep)
            }
            undent ; println()
          }
        case _        => s()
      }
    }
  }

  /** This must guarantee not to force any evaluation, so we can learn
   *  a little bit about trees in the midst of compilation without altering
   *  the natural course of events.
   */
  class SafeTreePrinter(out: PrintWriter) extends TreePrinter(out) {
    override def print(tree: Tree) {
      printPosition(tree)
      printRaw(tree)
    }
    private def default(t: Tree) = t.getClass.getName.reverse.takeWhile(_ != '.').reverse
    private def params(trees: List[Tree]): String = trees map safe mkString ", "

    private def safe(name: Name): String = name.decode
    private def safe(tree: Tree): String = tree match {
      case Apply(fn, args)        => "%s(%s)".format(safe(fn), params(args))
      case Select(qual, name)     => safe(qual) + "." + safe(name)
      case This(qual)             => safe(qual) + ".this"
      case Ident(name)            => safe(name)
      case Literal(value)         => value.stringValue
      case _                      => "(?: %s)".format(default(tree))
    }

    override def printRaw(tree: Tree) { print(safe(tree)) }
  }

  class TreeMatchTemplate {
    // non-trees defined in Trees
    //
    // case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int)
    // case class Modifiers(flags: Long, privateWithin: Name, annotations: List[Tree], positions: Map[Long, Position])
    //
    def apply(t: Tree): Unit = t match {
      // eliminated by typer
      case Annotated(annot, arg)  =>
      case AssignOrNamedArg(lhs, rhs) =>
      case DocDef(comment, definition) =>
      case Import(expr, selectors) =>

      // eliminated by refchecks
      case ModuleDef(mods, name, impl) =>
      case TypeTreeWithDeferredRefCheck() =>

      // eliminated by erasure
      case TypeDef(mods, name, tparams, rhs) =>
      case Typed(expr, tpt) =>

      // eliminated by cleanup
      case ApplyDynamic(qual, args) =>

      // eliminated by explicitouter
      case Alternative(trees) =>
      case Bind(name, body) =>
      case CaseDef(pat, guard, body) =>
      case Star(elem) =>
      case UnApply(fun, args) =>

      // eliminated by lambdalift
      case Function(vparams, body) =>

      // eliminated by uncurry
      case AppliedTypeTree(tpt, args) =>
      case CompoundTypeTree(templ) =>
      case ExistentialTypeTree(tpt, whereClauses) =>
      case SelectFromTypeTree(qual, selector) =>
      case SingletonTypeTree(ref) =>
      case TypeBoundsTree(lo, hi) =>

      // survivors
      case Apply(fun, args) =>
      case ArrayValue(elemtpt, trees) =>
      case Assign(lhs, rhs) =>
      case Block(stats, expr) =>
      case ClassDef(mods, name, tparams, impl) =>
      case DefDef(mods, name, tparams, vparamss, tpt, rhs)  =>
      case EmptyTree =>
      case Ident(name) =>
      case If(cond, thenp, elsep) =>
      case LabelDef(name, params, rhs) =>
      case Literal(value) =>
      case Match(selector, cases) =>
      case New(tpt) =>
      case PackageDef(pid, stats) =>
      case Return(expr) =>
      case Select(qualifier, selector) =>
      case Super(qual, mix) =>
      case Template(parents, self, body) =>
      case This(qual) =>
      case Throw(expr) =>
      case Try(block, catches, finalizer) =>
      case TypeApply(fun, args) =>
      case TypeTree() =>
      case ValDef(mods, name, tpt, rhs) =>

      // missing from the Trees comment
      case Parens(args) =>                          // only used during parsing
      case SelectFromArray(qual, name, erasure) =>  // only used during erasure
    }
  }

  private def asStringInternal(t: Tree, f: PrintWriter => TreePrinter): String = {
    val buffer = new StringWriter()
    val printer = f(new PrintWriter(buffer))
    printer.print(t)
    printer.flush()
    buffer.toString
  }
  def asString(t: Tree): String = asStringInternal(t, newStandardTreePrinter)
  def asCompactString(t: Tree): String = asStringInternal(t, newCompactTreePrinter)

  def newStandardTreePrinter(writer: PrintWriter): TreePrinter = new TreePrinter(writer)
  def newStandardTreePrinter(stream: OutputStream): TreePrinter = newStandardTreePrinter(new PrintWriter(stream))
  def newStandardTreePrinter(): TreePrinter = newStandardTreePrinter(new PrintWriter(ConsoleWriter))

  def newCompactTreePrinter(writer: PrintWriter): CompactTreePrinter = new CompactTreePrinter(writer)
  def newCompactTreePrinter(stream: OutputStream): CompactTreePrinter = newCompactTreePrinter(new PrintWriter(stream))
  def newCompactTreePrinter(): CompactTreePrinter = newCompactTreePrinter(new PrintWriter(ConsoleWriter))

  def newTreePrinter(writer: PrintWriter): TreePrinter =
    if (settings.Ycompacttrees.value) newCompactTreePrinter(writer)
    else newStandardTreePrinter(writer)
  def newTreePrinter(stream: OutputStream): TreePrinter = newTreePrinter(new PrintWriter(stream))
  def newTreePrinter(): TreePrinter = newTreePrinter(new PrintWriter(ConsoleWriter))

  /** A writer that writes to the current Console and
   * is sensitive to replacement of the Console's
   * output stream.
   */
  object ConsoleWriter extends Writer {
    override def write(str: String) { Console.print(str) }

    def write(cbuf: Array[Char], off: Int, len: Int) {
      val str = new String(cbuf, off, len)
      write(str)
    }

    def close = { /* do nothing */ }
    def flush = { /* do nothing */ }
  }
}
