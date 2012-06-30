/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

// [Eugene++ to Martin] we need to unify this prettyprinter with NodePrinters

package scala.reflect
package internal

import java.io.{ OutputStream, PrintWriter, StringWriter, Writer }
import Flags._
import compat.Platform.EOL

trait Printers extends api.Printers { self: SymbolTable =>

  //nsc import treeInfo.{ IsTrue, IsFalse }

  final val showOuterTests = false

  /** Adds backticks if the name is a scala keyword. */
  def quotedName(name: Name, decode: Boolean): String = {
    val s = if (decode) name.decode else name.toString
    val term = name.toTermName
    if (nme.keywords(term) && term != nme.USCOREkw) "`%s`" format s
    else s
  }
  def quotedName(name: Name): String = quotedName(name, false)
  def quotedName(name: String): String = quotedName(newTermName(name), false)

  private def symNameInternal(tree: Tree, name: Name, decoded: Boolean): String = {
    val sym = tree.symbol
    if (sym.name.toString == nme.ERROR.toString) {
      "<" + quotedName(name, decoded) + ": error>"
    } else if (sym != null && sym != NoSymbol) {
      val prefix = if (sym.isMixinConstructor) "/*%s*/".format(quotedName(sym.owner.name, decoded)) else ""
      var suffix = ""
      if (settings.uniqid.value) suffix += ("#" + sym.id)
      if (settings.Yshowsymkinds.value) suffix += ("#" + sym.abbreviatedKindString)
      prefix + quotedName(tree.symbol.decodedName) + suffix
    } else {
      quotedName(name, decoded)
    }
  }

  def decodedSymName(tree: Tree, name: Name) = symNameInternal(tree, name, true)
  def symName(tree: Tree, name: Name) = symNameInternal(tree, name, false)

  /** Turns a path into a String, introducing backquotes
   *  as necessary.
   */
  def backquotedPath(t: Tree): String = {
    t match {
      case Select(qual, name) if name.isTermName  => "%s.%s".format(backquotedPath(qual), symName(t, name))
      case Select(qual, name) if name.isTypeName  => "%s#%s".format(backquotedPath(qual), symName(t, name))
      case Ident(name)                            => symName(t, name)
      case _                                      => t.toString
    }
  }

  class TreePrinter(out: PrintWriter) extends super.TreePrinter {
    protected var indentMargin = 0
    protected val indentStep = 2
    protected var indentString = "                                        " // 40

    printTypes = settings.printtypes.value
    printIds = settings.uniqid.value
    printKinds = settings.Yshowsymkinds.value
    printMirrors = false // typically there's no point to print mirrors inside the compiler, as there is only one mirror there
    protected def doPrintPositions = settings.Xprintpos.value

    def indent() = indentMargin += indentStep
    def undent() = indentMargin -= indentStep

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
      printSeq(ts){print(_)}{print(sep); println()}; undent; println(); print(end)
    }

    def printRow(ts: List[Tree], start: String, sep: String, end: String) {
      print(start); printSeq(ts){print(_)}{print(sep)}; print(end)
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

    def printLabelParams(ps: List[Ident]) {
      print("(")
      printSeq(ps){printLabelParam}{print(", ")}
      print(")")
    }

    def printLabelParam(p: Ident) {
      print(symName(p, p.name)); printOpt(": ", TypeTree() setType p.tpe)
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

    def printOpt(prefix: String, tree: Tree) {
      if (!tree.isEmpty) { print(prefix, tree) }
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
      if (!isCompilerUniverse && tree.symbol != null && tree.symbol != NoSymbol)
        // [Eugene++] todo. this is not 100% correct, but is necessary for sane printing
        // the problem is that getting annotations doesn't automatically initialize the symbol
        // so we might easily print something as if it doesn't have annotations, whereas it does
        tree.symbol.initialize

      val annots = tree.symbol.annotations match {
        case Nil  => tree.asInstanceOf[MemberDef].mods.annotations
        case anns => anns
      }
      annots foreach (annot => print("@"+annot+" "))
    }

    private var currentOwner: Symbol = NoSymbol
    private var selectorType: Type = NoType

    def printTree(tree: Tree) {
      tree match {
        case EmptyTree =>
          print("<empty>")

        case ClassDef(mods, name, tparams, impl) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          val word =
            if (mods.isTrait) "trait"
            else if (ifSym(tree, _.isModuleClass)) "object"
            else "class"

          print(word, " ", symName(tree, name))
          printTypeParams(tparams)
          print(if (mods.isDeferred) " <: " else " extends ", impl)

        case PackageDef(packaged, stats) =>
          printAnnotations(tree)
          print("package ", packaged); printColumn(stats, " {", ";", "}")

        case ModuleDef(mods, name, impl) =>
          printAnnotations(tree)
          printModifiers(tree, mods);
          print("object " + symName(tree, name), " extends ", impl)

        case ValDef(mods, name, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print(if (mods.isMutable) "var " else "val ", symName(tree, name))
          printOpt(": ", tp)
          if (!mods.isDeferred)
            print(" = ", if (rhs.isEmpty) "_" else rhs)

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
          print(symName(tree, name)); printLabelParams(params); printBlock(rhs)

        case Import(expr, selectors) =>
          // Is this selector remapping a name (i.e, {name1 => name2})
          def isNotRemap(s: ImportSelector) : Boolean = (s.name == nme.WILDCARD || s.name == s.rename)
          def selectorToString(s: ImportSelector): String = {
            val from = quotedName(s.name)
            if (isNotRemap(s)) from
            else from + "=>" + quotedName(s.rename)
          }
          print("import ", backquotedPath(expr), ".")
          selectors match {
            case List(s) =>
              // If there is just one selector and it is not remapping a name, no braces are needed
              if (isNotRemap(s)) print(selectorToString(s))
              else print("{", selectorToString(s), "}")
              // If there is more than one selector braces are always needed
            case many =>
              print(many.map(selectorToString).mkString("{", ", ", "}"))
          }

       case Template(parents, self, body) =>
          val currentOwner1 = currentOwner
          if (tree.symbol != NoSymbol) currentOwner = tree.symbol.owner
//          if (parents exists isReferenceToAnyVal) {
//            print("AnyVal")
//          }
//          else {
          printRow(parents, " with ")
          if (!body.isEmpty) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name); printOpt(": ", self.tpt); print(" => ")
            } else if (!self.tpt.isEmpty) {
              print(" { _ : ", self.tpt, " => ")
            } else {
              print(" {")
            }
            printColumn(body, "", ";", "}")
          }
//          }
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
          print(" => ", body)

        case Alternative(trees) =>
          printRow(trees, "(", "| ", ")")

        case Star(elem) =>
          print("(", elem, ")*")

        case Bind(name, t) =>
          print("(", symName(tree, name), " @ ", t, ")")

        case UnApply(fun, args) =>
          print(fun, " <unapply> "); printRow(args, "(", ", ", ")")

        case ArrayValue(elemtpt, trees) =>
          print("Array[", elemtpt); printRow(trees, "]{", ", ", "}")

        case Function(vparams, body) =>
          print("("); printValueParams(vparams); print(" => ", body, ")")
          if (printIds && tree.symbol != null) print("#"+tree.symbol.id)

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case AssignOrNamedArg(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case If(cond, thenp, elsep) =>
          print("if (", cond, ")"); indent; println()
          print(thenp); undent
          if (!elsep.isEmpty) {
            println(); print("else"); indent; println(); print(elsep); undent
          }

        case Return(expr) =>
          print("return ", expr)

        case Try(block, catches, finalizer) =>
          print("try "); printBlock(block)
          if (!catches.isEmpty) printColumn(catches, " catch {", "", "}")
          printOpt(" finally ", finalizer)

        case Throw(expr) =>
          print("throw ", expr)

        case New(tpe) =>
          print("new ", tpe)

        case Typed(expr, tp) =>
          print("(", expr, ": ", tp, ")")

        case TypeApply(fun, targs) =>
          print(fun); printRow(targs, "[", ", ", "]")

        case Apply(fun, vargs) =>
          print(fun); printRow(vargs, "(", ", ", ")")

        case ApplyDynamic(qual, vargs) =>
          print("<apply-dynamic>(", qual, "#", tree.symbol.nameString)
          printRow(vargs, ", (", ", ", "))")

        case Super(This(qual), mix) =>
          if (!qual.isEmpty || tree.symbol != NoSymbol) print(symName(tree, qual) + ".")
          print("super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case Super(qual, mix) =>
          print(qual, ".super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case This(qual) =>
          if (!qual.isEmpty) print(symName(tree, qual) + ".")
          print("this")

        case Select(qual @ New(tpe), name) if (!settings.debug.value) =>
          print(qual)

        case Select(qualifier, name) =>
          print(backquotedPath(qualifier), ".", symName(tree, name))

        case id @ Ident(name) =>
          val str = symName(tree, name)
          print( if (id.isBackquoted) "`" + str + "`" else str )

        case Literal(x) =>
          print(x.escapedStringValue)

        case tt: TypeTree =>
          if ((tree.tpe eq null) || (doPrintPositions && tt.original != null)) {
            if (tt.original != null) print("<type: ", tt.original, ">")
            else print("<type ?>")
          } else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass) {
            print(tree.tpe.typeSymbol.toString)
          } else {
            print(tree.tpe.toString)
          }

        case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot() {
            print("@", tpt)
            if (!args.isEmpty)
              printRow(args, "(", ",", ")")
          }
          print(tree, if (tree.isType) " " else ": ")
          printAnnot()

        case SingletonTypeTree(ref) =>
          print(ref, ".type")

        case SelectFromTypeTree(qualifier, selector) =>
          print(qualifier, "#", symName(tree, selector))

        case CompoundTypeTree(templ) =>
          print(templ)

        case AppliedTypeTree(tp, args) =>
          print(tp); printRow(args, "[", ", ", "]")

        case TypeBoundsTree(lo, hi) =>
          printOpt(" >: ", lo); printOpt(" <: ", hi)

        case ExistentialTypeTree(tpt, whereClauses) =>
          print(tpt);
          printColumn(whereClauses, " forSome { ", ";", "}")

// SelectFromArray is no longer visible in reflect.internal.
// eliminated until we figure out what we will do with both Printers and
// SelectFromArray.
//          case SelectFromArray(qualifier, name, _) =>
//          print(qualifier); print(".<arr>"); print(symName(tree, name))

        case tree =>
          xprintTree(this, tree)
      }
      if (printTypes && tree.isTerm && !tree.isEmpty) {
        print("{", if (tree.tpe eq null) "<null>" else tree.tpe.toString, "}")
      }
    }

    def print(args: Any*): Unit = args foreach {
      case tree: Tree =>
        printPosition(tree)
        printTree(tree)
      case name: Name =>
        print(quotedName(name))
      case arg =>
        out.print(if (arg == null) "null" else arg.toString)
    }
  }

  /** Hook for extensions */
  def xprintTree(treePrinter: TreePrinter, tree: Tree) =
    treePrinter.print(tree.productPrefix+tree.productIterator.mkString("(", ", ", ")"))

  def newTreePrinter(writer: PrintWriter): TreePrinter = new TreePrinter(writer)
  def newTreePrinter(stream: OutputStream): TreePrinter = newTreePrinter(new PrintWriter(stream))
  def newTreePrinter(): TreePrinter = newTreePrinter(new PrintWriter(ConsoleWriter))

  /** A writer that writes to the current Console and
   * is sensitive to replacement of the Console's
   * output stream.
   */
  object ConsoleWriter extends Writer {
    override def write(str: String) { Console.print(str) }

    def write(cbuf: Array[Char], off: Int, len: Int) {
      write(new String(cbuf, off, len))
    }

    def close = { /* do nothing */ }
    def flush = { /* do nothing */ }
  }

  def newRawTreePrinter(writer: PrintWriter): RawTreePrinter = new RawTreePrinter(writer)
  def newRawTreePrinter(stream: OutputStream): RawTreePrinter = newRawTreePrinter(new PrintWriter(stream))
  def newRawTreePrinter(): RawTreePrinter = newRawTreePrinter(new PrintWriter(ConsoleWriter))

  // provides footnotes for types and mirrors
  import scala.collection.mutable.{Map, WeakHashMap, SortedSet}
  private val footnoteIndex = new FootnoteIndex
  private class FootnoteIndex {
    private val index = Map[Class[_], WeakHashMap[Any, Int]]()
    private def classIndex[T: ClassTag] = index.getOrElseUpdate(classTag[T].runtimeClass, WeakHashMap[Any, Int]())
    private val counters = Map[Class[_], Int]()
    private def nextCounter[T: ClassTag] = {
      val clazz = classTag[T].runtimeClass
      counters.getOrElseUpdate(clazz, 0)
      counters(clazz) = counters(clazz) + 1
      counters(clazz)
    }

    def mkFootnotes() = new Footnotes
    class Footnotes {
      private val footnotes = Map[Class[_], SortedSet[Int]]()
      private def classFootnotes[T: ClassTag] = footnotes.getOrElseUpdate(classTag[T].runtimeClass, SortedSet[Int]())

      def put[T: ClassTag](any: T): Int = {
        val index = classIndex[T].getOrElseUpdate(any, nextCounter[T])
        classFootnotes[T] += index
        index
      }

      def get[T: ClassTag]: List[(Int, Any)] =
        classFootnotes[T].toList map (fi => (fi, classIndex[T].find{ case (any, ii) => ii == fi }.get._1))

      def print[T: ClassTag](printer: Printers.super.TreePrinter): Unit = {
        val footnotes = get[T]
        if (footnotes.nonEmpty) {
          printer.print(EOL)
          footnotes.zipWithIndex foreach {
            case ((fi, any), ii) =>
              printer.print("[", fi, "] ", any)
              if (ii < footnotes.length - 1) printer.print(EOL)
          }
        }
      }
    }
  }

  // emits more or less verbatim representation of the provided tree
  class RawTreePrinter(out: PrintWriter) extends super.TreePrinter {
    private var depth = 0
    private var printTypesInFootnotes = true
    private var printingFootnotes = false
    private var footnotes = footnoteIndex.mkFootnotes()

    def print(args: Any*): Unit = {
      // don't print type footnotes if the argument is a mere type
      if (depth == 0 && args.length == 1 && args(0) != null && args(0).isInstanceOf[Type])
        printTypesInFootnotes = false

      depth += 1
      args foreach {
        case EmptyTree =>
          print("EmptyTree")
        case emptyValDef: AnyRef if emptyValDef eq self.emptyValDef =>
          print("emptyValDef")
        case Literal(Constant(value)) =>
          def print(s: String) = this.print("Literal(Constant(" + s + "))")
          value match {
            case s: String => print("\"" + s + "\"")
            case null => print(null)
            case _ => print(value.toString)
          }
        case tree: Tree =>
          val hasSymbol = tree.hasSymbol && tree.symbol != NoSymbol
          val isError = hasSymbol && tree.symbol.name.toString == nme.ERROR.toString
          printProduct(
            tree,
            preamble = _ => {
              print(tree.productPrefix)
              if (printTypes && tree.tpe != null) print(tree.tpe)
            },
            body = {
              case name: Name =>
                if (isError) {
                  if (isError) print("<")
                  print(name)
                  if (isError) print(": error>")
                } else if (hasSymbol) {
                  tree match {
                    case _: Ident | _: Select | _: SelectFromTypeTree => print(tree.symbol)
                    case _ => print(tree.symbol.name)
                  }
                } else {
                  print(name)
                }
              case arg =>
                print(arg)
            },
            postamble = {
              case tree @ TypeTree() if tree.original != null => print(".setOriginal(", tree.original, ")")
              case _ => // do nothing
            })
        case sym: Symbol =>
          if (sym.isStatic && (sym.isClass || sym.isModule)) print(sym.fullName)
          else print(sym.name)
          if (printIds) print("#", sym.id)
          if (printKinds) print("#", sym.abbreviatedKindString)
          if (printMirrors) print("%M", footnotes.put[MirrorOf[_]](mirrorThatLoaded(sym)))
        case NoType =>
          print("NoType")
        case NoPrefix =>
          print("NoPrefix")
        case tpe: Type =>
          val defer = printTypesInFootnotes && !printingFootnotes
          if (defer) print("[", footnotes.put(tpe), "]")
          else printProduct(tpe.asInstanceOf[Product])
        case mods: Modifiers =>
          print("Modifiers(")
          if (mods.flags != NoFlags || mods.privateWithin != tpnme.EMPTY || mods.annotations.nonEmpty) print(show(mods.flags))
          if (mods.privateWithin != tpnme.EMPTY || mods.annotations.nonEmpty) { print(", "); print(mods.privateWithin) }
          if (mods.annotations.nonEmpty) { print(", "); print(mods.annotations); }
          print(")")
        case name: Name =>
          print(show(name))
        case list: List[_] =>
          print("List")
          printIterable(list)
        case product: Product =>
          printProduct(product)
        case arg =>
          out.print(arg)
      }
      depth -= 1
      if (depth == 0 && !printingFootnotes) {
        printingFootnotes = true
        footnotes.print[Type](this)
        footnotes.print[MirrorOf[_]](this)
        printingFootnotes = false
      }
    }

    def printProduct(
      p: Product,
      preamble: Product => Unit = p => print(p.productPrefix),
      body: Any => Unit = print(_),
      postamble: Product => Unit = p => print("")): Unit =
    {
      preamble(p)
      printIterable(p.productIterator.toList, body = body)
      postamble(p)
    }

    def printIterable(
      iterable: List[_],
      preamble: => Unit = print(""),
      body: Any => Unit = print(_),
      postamble: => Unit = print("")): Unit =
    {
      preamble
      print("(")
      val it = iterable.iterator
      while (it.hasNext) {
        body(it.next)
        print(if (it.hasNext) ", " else "")
      }
      print(")")
      postamble
    }
  }

  def show(name: Name): String = name match {
    // base.StandardNames
    case tpnme.EMPTY => "tpnme.EMPTY"
    case tpnme.ROOT => "tpnme.ROOT"
    case tpnme.EMPTY_PACKAGE_NAME => "tpnme.EMPTY_PACKAGE_NAME"
    case tpnme.WILDCARD => "tpnme.WILDCARD"
    case nme.CONSTRUCTOR => "nme.CONSTRUCTOR"
    case nme.NO_NAME => "nme.NO_NAME"
    // api.StandardNames
    case tpnme.ERROR => "tpnme.ERROR"
    case nme.ERROR => "nme.ERROR"
    case nme.EMPTY => "nme.EMPTY"
    case tpnme.PACKAGE => "tpnme.PACKAGE"
    case nme.PACKAGE => "nme.PACKAGE"
    case _ =>
      val prefix = if (name.isTermName) "newTermName(\"" else "newTypeName(\""
      prefix + name.toString + "\")"
  }

  def show(flags: FlagSet): String = {
    if (flags == NoFlags) nme.NoFlags.toString
    else {
      val s_flags = new collection.mutable.ListBuffer[String]
      for (i <- 0 to 63 if (flags containsAll (1L << i)))
        s_flags += flagToString(1L << i).replace("<", "").replace(">", "").toUpperCase
      s_flags mkString " | "
    }
  }
}
