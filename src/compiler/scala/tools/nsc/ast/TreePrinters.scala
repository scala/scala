/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.ast

import compat.Platform.{EOL => LINE_SEPARATOR}
import java.io.{OutputStream, PrintWriter, Writer}
import symtab.Flags._
import symtab.SymbolTable

abstract class TreePrinters {

  val trees: SymbolTable
  import trees._

  final val showOuterTests = false

  class TreePrinter(out: PrintWriter) {
    protected var indentMargin = 0
    protected val indentStep = 2
    protected var indentString = "                                        " // 40

    def flush() = out.flush()

    def indent = indentMargin += indentStep
    def undent = indentMargin -= indentStep

    def println {
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
      print(start); indent; println
      printSeq(ts){print}{print(sep); println}; undent; println; print(end)
    }

    def printRow(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); printSeq(ts){print}{print(sep)}; print(end)
    }

    def printRow(ts: List[Tree], sep: String) { printRow(ts, "", sep, "") }

    def printTypeParams(ts: List[TypeDef]) {
      if (!ts.isEmpty) {
        print("["); printSeq(ts){printParam}{print(", ")}; print("]")
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
          printAnnotations(tree)
          print(symName(tree, name)); printOpt(": ", tp)
        case TypeDef(mods, name, tparams, rhs) =>
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

    def symName(tree: Tree, name: Name): String =
      if (tree.symbol != null && tree.symbol != NoSymbol) {
        ((if (tree.symbol.isMixinConstructor) "/*"+tree.symbol.owner.name+"*/" else "") +
         tree.symbol.nameString)
      } else name.toString();

    def printOpt(prefix: String, tree: Tree) {
      if (!tree.isEmpty) { print(prefix); print(tree) }
    }

    def printModifiers(tree: Tree, mods: Modifiers) {
      if (tree.symbol == NoSymbol)
        printFlags(mods.flags, mods.privateWithin.toString)
      else if (tree.symbol.privateWithin == NoSymbol ||
               tree.symbol.privateWithin == tree.symbol.owner)
        printFlags(tree.symbol.flags, "")
      else
        printFlags(tree.symbol.flags, tree.symbol.privateWithin.name.toString)
    }

    def printFlags(flags: Long, privateWithin: String) {
      var mask: Long = if (settings.debug.value) -1L else PrintableFlags
      val s = flagsToString(flags & mask, privateWithin)
      if (s.length() != 0) print(s + " ")
    }

    def printAnnotations(tree: Tree) {
      val annots = tree.symbol.attributes
      if (!annots.isEmpty) {
        annots foreach { annot => print("@"+annot+" ") }
        println
      }
      else {
        val annots = tree.asInstanceOf[MemberDef].mods.annotations
        if (!annots.isEmpty) {
          annots foreach { annot => print("@"+annot+" ") }
          println
        }
      }
    }

    def print(str: String) { out.print(str) }
    def print(name: Name) { print(name.toString()) }

    private var currentOwner: Symbol = NoSymbol
    private var selectorType: Type = NoType

    def printRaw(tree: Tree) {
      tree match {
        case EmptyTree =>
          print("<empty>")

        case ClassDef(mods, name, tparams, impl) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print((if (mods.isTrait) "trait " else "class ") + symName(tree, name))
          printTypeParams(tparams)
          print(if (mods hasFlag DEFERRED) " <: " else " extends "); print(impl) // (part of DEVIRTUALIZE)

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
          print(if (mods.hasFlag(MUTABLE)) "var " else "val ")
          print(symName(tree, name))
          printOpt(": ", tp)
          if (!mods.hasFlag(DEFERRED)) {
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
          def isNotRemap(s: (Name, Name)) : Boolean = (s._1 == nme.WILDCARD || s._1 == s._2)
          def selectorToString(s: (Name, Name)): String =
              if (isNotRemap(s)) s._1.toString else s._1.toString + "=>" + s._2.toString

          print("import "); print(expr)
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
          print(comment); println; print(definition)

        case Annotation(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), elements) =>
          print(tpt)
          if (!args.isEmpty)
            printRow(args, "(", ",", ")")
          if (!elements.isEmpty)
            print((for (Assign(name, value) <- elements) yield "val " + name + " = " + value).
                  mkString("{", ",", "}"))

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

        case Sequence(trees) =>
          printRow(trees, "[", ", ", "]")

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

        case Assign(lhs, rhs) =>
          print(lhs); print(" = "); print(rhs)

        case If(cond, thenp, elsep) =>
          print("if ("); print(cond); print(")"); indent; println
          print(thenp); undent
          if (!elsep.isEmpty) {
            println; print("else"); indent; println; print(elsep); undent
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
          print(qualifier); print("."); print(symName(tree, name))

        case Ident(name) =>
          print(symName(tree, name))

        case Literal(x) =>
          print(x.escapedStringValue)

        case tt: TypeTree =>
          if (tree.tpe eq null) {
            if (tt.original != null) { print("<type: "); print(tt.original); print(">") }
            else print("<type ?>")
          } else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass) {
            print(tree.tpe.typeSymbol.toString())
          } else {
            tree.tpe.toString()
          }

        case Annotated(Annotation(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), elements), tree) =>
          def printAnnot() {
            print("@"); print(tpt)
            if (!args.isEmpty)
              printRow(args, "(", ",", ")")
            if (!elements.isEmpty)
              print((for (Assign(name, value) <- elements) yield "val " + name + " = " + value).
                    mkString("{", ",", "}"))
          }
          if (tree.isType) { printAnnot(); print(" "); print(tree) }
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

        case tree: StubTree =>
          print(tree.toString)

        case tree =>
          print("<unknown tree of class "+tree.getClass+">")
      }
      if (settings.printtypes.value && tree.isTerm && !tree.isEmpty) {
        print("{"); print(if (tree.tpe eq null) "<null>" else tree.tpe.toString()); print("}")
      }
    }

    def print(tree: Tree) {
      if (settings.Xprintpos.value) print(tree.pos.show)
      printRaw(
        if (tree.isDef && tree.symbol != NoSymbol && tree.symbol.isInitialized) {
          tree match {
            case ClassDef(_, _, _, impl @ Template(ps, trees.emptyValDef, body))
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
        print(unit.body); println
      } else {
        print("<null>")
      }
      println; flush
    }
  }

  def create(writer: PrintWriter): TreePrinter = new TreePrinter(writer)
  def create(stream: OutputStream): TreePrinter = create(new PrintWriter(stream))
  def create(): TreePrinter = {
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
    create(new PrintWriter(ConsoleWriter))
  }
}
