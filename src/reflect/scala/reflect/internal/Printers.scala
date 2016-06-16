/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

// todo. we need to unify this prettyprinter with NodePrinters

package scala
package reflect
package internal

import java.io.{ OutputStream, PrintWriter, Writer }
import Flags._
import scala.compat.Platform.EOL

trait Printers extends api.Printers { self: SymbolTable =>

  //nsc import treeInfo.{ IsTrue, IsFalse }

  /** Adds backticks if the name is a scala keyword. */
  def quotedName(name: Name, decode: Boolean): String = {
    val s = if (decode) name.decode else name.toString
    val term = name.toTermName
    if (nme.keywords(term) && term != nme.USCOREkw) "`%s`" format s
    else s
  }
  def quotedName(name: Name): String = quotedName(name, decode = false)
  def quotedName(name: String): String = quotedName(newTermName(name), decode = false)

  private def symNameInternal(tree: Tree, name: Name, decoded: Boolean): String = {
    val sym     = tree.symbol
    def qname   = quotedName(name.dropLocal, decoded)
    def qowner  = quotedName(sym.owner.name.dropLocal, decoded)
    def qsymbol = quotedName(sym.nameString)

    if (sym == null || sym == NoSymbol)
      qname
    else if (sym.isErroneous)
      s"<$qname: error>"
    else if (sym.isMixinConstructor)
      s"/*$qowner*/$qsymbol"
    else
      qsymbol
  }

  def decodedSymName(tree: Tree, name: Name) = symNameInternal(tree, name, decoded = true)
  def symName(tree: Tree, name: Name) = symNameInternal(tree, name, decoded = false)

  /** Turns a path into a String, introducing backquotes
   *  as necessary.
   */
  def backquotedPath(t: Tree): String = {
    t match {
      case Select(qual, name) if name.isTermName  => s"${backquotedPath(qual)}.${symName(t, name)}"
      case Select(qual, name) if name.isTypeName  => s"${backquotedPath(qual)}#${symName(t, name)}"
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
    printOwners = settings.Yshowsymowners.value
    printKinds = settings.Yshowsymkinds.value
    printMirrors = false // typically there's no point to print mirrors inside the compiler, as there is only one mirror there
    printPositions = settings.Xprintpos.value

    def indent() = indentMargin += indentStep
    def undent() = indentMargin -= indentStep

    def printPosition(tree: Tree) =
      if (printPositions) comment(print(tree.pos.show))

    protected def printTypesInfo(tree: Tree) =
      if (printTypes && tree.isTerm && tree.canHaveAttrs)
        comment{
          print("{", if (tree.tpe eq null) "<null>" else tree.tpe.toString, "}")
        }

    def println() = {
      out.println()
      while (indentMargin > indentString.length())
        indentString += indentString
      if (indentMargin > 0)
        out.write(indentString, 0, indentMargin)
    }

    def printSeq[a](ls: List[a])(printelem: a => Unit)(printsep: => Unit): Unit =
      ls match {
        case List() =>
        case List(x) => printelem(x)
        case x :: rest => printelem(x); printsep; printSeq(rest)(printelem)(printsep)
      }

    def printColumn(ts: List[Tree], start: String, sep: String, end: String) = {
      print(start); indent(); println()
      printSeq(ts){print(_)}{print(sep); println()}; undent(); println(); print(end)
    }

    def printRow(ts: List[Tree], start: String, sep: String, end: String): Unit = {
      print(start); printSeq(ts){print(_)}{print(sep)}; print(end)
    }

    def printRow(ts: List[Tree], sep: String): Unit = printRow(ts, "", sep, "")

    def printTypeParams(ts: List[TypeDef]): Unit =
      if (ts.nonEmpty) {
        print("["); printSeq(ts){ t =>
          printAnnotations(t)
          if (t.mods.hasFlag(CONTRAVARIANT)) {
            print("-")
          } else if (t.mods.hasFlag(COVARIANT)) {
            print("+")
          }
          printParam(t)
        }{print(", ")}; print("]")
      }

    def printLabelParams(ps: List[Ident]) = {
      print("(")
      printSeq(ps){printLabelParam}{print(", ")}
      print(")")
    }

    def printLabelParam(p: Ident) = {
      print(symName(p, p.name)); printOpt(": ", TypeTree() setType p.tpe)
    }

    protected def parenthesize(condition: Boolean = true, open: String = "(", close: String = ")")(body: => Unit) = {
      if (condition) print(open)
      body
      if (condition) print(close)
    }

    protected val commentsRequired = false

    protected def comment(body: => Unit) =
      parenthesize(commentsRequired, "/*", "*/")(body)

    protected def printImplicitInParamsList(vds: List[ValDef]) =
      if (vds.nonEmpty) printFlags(vds.head.mods.flags & IMPLICIT, "")

    def printValueParams(ts: List[ValDef], inParentheses: Boolean = true): Unit =
      parenthesize(inParentheses){
        printImplicitInParamsList(ts)
        printSeq(ts){printParam}{print(", ")}
      }

    def printParam(tree: Tree) =
      tree match {
        case vd @ ValDef(mods, name, tp, rhs) =>
          printPosition(tree)
          printAnnotations(vd)
          print(symName(tree, name)); printOpt(": ", tp); printOpt(" = ", rhs)
        case TypeDef(mods, name, tparams, rhs) =>
          printPosition(tree)
          print(symName(tree, name))
          printTypeParams(tparams); print(rhs)
      }

    def printBlock(tree: Tree) =
      tree match {
        case Block(_, _) =>
          print(tree)
        case _ =>
          printColumn(List(tree), "{", ";", "}")
      }

    private def symFn[T](tree: Tree, f: Symbol => T, orElse: => T): T = tree.symbol match {
      case null | NoSymbol => orElse
      case sym             => f(sym)
    }
    private def ifSym(tree: Tree, p: Symbol => Boolean) = symFn(tree, p, false)

    def printOpt(prefix: String, tree: Tree) = if (tree.nonEmpty) { print(prefix, tree) }

    def printModifiers(tree: Tree, mods: Modifiers): Unit = printFlags(
      if (tree.symbol == NoSymbol) mods.flags else tree.symbol.flags, "" + (
        if (tree.symbol == NoSymbol) mods.privateWithin
        else if (tree.symbol.hasAccessBoundary) tree.symbol.privateWithin.name
        else ""
      )
    )

    def printFlags(flags: Long, privateWithin: String) = {
      val mask: Long = if (settings.debug) -1L else PrintableFlags
      val s = flagsToString(flags & mask, privateWithin)
      if (s != "") print(s + " ")
    }

    def printAnnotations(tree: MemberDef) = {
      // SI-5885: by default this won't print annotations of not yet initialized symbols
      val annots = tree.symbol.annotations match {
        case Nil  => tree.mods.annotations
        case anns => anns
      }
      annots foreach (annot => print(s"@$annot "))
    }

    private var currentOwner: Symbol = NoSymbol
    private var selectorType: Type = NoType

    protected def printPackageDef(tree: PackageDef, separator: String) = {
      val PackageDef(packaged, stats) = tree
      printAnnotations(tree)
      print("package ", packaged); printColumn(stats, " {", separator, "}")
    }

    protected def printValDef(tree: ValDef, resultName: => String)(printTypeSignature: => Unit)(printRhs: => Unit) = {
      val ValDef(mods, name, tp, rhs) = tree
      printAnnotations(tree)
      printModifiers(tree, mods)
      print(if (mods.isMutable) "var " else "val ", resultName)
      printTypeSignature
      printRhs
    }

    protected def printDefDef(tree: DefDef, resultName: => String)(printTypeSignature: => Unit)(printRhs: => Unit) = {
      val DefDef(mods, name, tparams, vparamss, tp, rhs) = tree
      printAnnotations(tree)
      printModifiers(tree, mods)
      print("def " + resultName)
      printTypeParams(tparams);
      vparamss foreach {printValueParams(_)}
      printTypeSignature
      printRhs
    }

    protected def printTypeDef(tree: TypeDef, resultName: => String) = {
      val TypeDef(mods, name, tparams, rhs) = tree
      if (mods hasFlag (PARAM | DEFERRED)) {
        printAnnotations(tree)
        printModifiers(tree, mods)
        print("type ")
        printParam(tree)
      } else {
        printAnnotations(tree)
        printModifiers(tree, mods)
        print("type " + resultName)
        printTypeParams(tparams)
        printOpt(" = ", rhs)
      }
    }

    protected def printImport(tree: Import, resSelect: => String) = {
      val Import(expr, selectors) = tree
      // Is this selector renaming a name (i.e, {name1 => name2})
      def isNotRename(s: ImportSelector): Boolean =
        s.name == nme.WILDCARD || s.name == s.rename

      def selectorToString(s: ImportSelector): String = {
        val from = quotedName(s.name)
        if (isNotRename(s)) from
        else from + "=>" + quotedName(s.rename)
      }
      print("import ", resSelect, ".")
      selectors match {
        case List(s) =>
          // If there is just one selector and it is not renaming a name, no braces are needed
          if (isNotRename(s)) print(selectorToString(s))
          else print("{", selectorToString(s), "}")
        // If there is more than one selector braces are always needed
        case many =>
          print(many.map(selectorToString).mkString("{", ", ", "}"))
      }
    }

    protected def printCaseDef(tree: CaseDef) = {
      val CaseDef(pat, guard, body) = tree
      print("case ")
      def patConstr(pat: Tree): Tree = pat match {
        case Apply(fn, args) => patConstr(fn)
        case _ => pat
      }

      print(pat); printOpt(" if ", guard)
      print(" => ", body)
    }

    protected def printFunction(tree: Function)(printValueParams: => Unit) = {
      val Function(vparams, body) = tree
      print("(");
      printValueParams
      print(" => ", body, ")")
      if (printIds && tree.symbol != null)
        comment{
          print("#" + tree.symbol.id)
        }

      if (printOwners && tree.symbol != null)
        comment{
          print("@" + tree.symbol.owner.id)
        }
    }

    protected def printSuper(tree: Super, resultName: => String, checkSymbol: Boolean = true) = {
      val Super(This(qual), mix) = tree
      if (qual.nonEmpty || (checkSymbol && tree.symbol != NoSymbol)) print(resultName + ".")
      print("super")
      if (mix.nonEmpty) print(s"[$mix]")
    }

    protected def printThis(tree: This, resultName: => String) = {
      val This(qual) = tree
      if (qual.nonEmpty) print(resultName + ".")
      print("this")
    }

    protected def printBlock(stats: List[Tree], expr: Tree) =
      printColumn(stats ::: List(expr), "{", ";", "}")

    def printTree(tree: Tree) = {
      tree match {
        case EmptyTree =>
          print("<empty>")

        case cd @ ClassDef(mods, name, tparams, impl) =>
          printAnnotations(cd)
          printModifiers(tree, mods)
          val word =
            if (mods.isTrait) "trait"
            else if (ifSym(tree, _.isModuleClass)) "object"
            else "class"

          print(word, " ", symName(tree, name))
          printTypeParams(tparams)
          print(if (mods.isDeferred) " <: " else " extends ", impl)

        case pd @ PackageDef(packaged, stats) =>
          printPackageDef(pd, ";")

        case md @ ModuleDef(mods, name, impl) =>
          printAnnotations(md)
          printModifiers(tree, mods)
          print("object " + symName(tree, name), " extends ", impl)

        case vd @ ValDef(mods, name, tp, rhs) =>
          printValDef(vd, symName(tree, name))(printOpt(": ", tp)) {
            if (!mods.isDeferred) print(" = ", if (rhs.isEmpty) "_" else rhs)
          }

        case dd @ DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printDefDef(dd, symName(tree, name))(printOpt(": ", tp))(printOpt(" = ", rhs))

        case td @ TypeDef(mods, name, tparams, rhs) =>
          printTypeDef(td, symName(tree, name))

        case LabelDef(name, params, rhs) =>
          print(symName(tree, name)); printLabelParams(params); printBlock(rhs)

        case imp @ Import(expr, _) =>
          printImport(imp, backquotedPath(expr))

        case Template(parents, self, body) =>
          val currentOwner1 = currentOwner
          if (tree.symbol != NoSymbol) currentOwner = tree.symbol.owner
          printRow(parents, " with ")
          if (body.nonEmpty) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name); printOpt(": ", self.tpt); print(" => ")
            } else if (self.tpt.nonEmpty) {
              print(" { _ : ", self.tpt, " => ")
            } else {
              print(" {")
            }
            printColumn(body, "", ";", "}")
          }
          currentOwner = currentOwner1

        case Block(stats, expr) =>
          printBlock(stats, expr)

        case Match(selector, cases) =>
          val selectorType1 = selectorType
          selectorType = selector.tpe
          print(selector); printColumn(cases, " match {", "", "}")
          selectorType = selectorType1

        case cd @ CaseDef(pat, guard, body) =>
          printCaseDef(cd)

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

        case f @ Function(vparams, body) =>
          printFunction(f)(printValueParams(vparams))

        case Assign(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case AssignOrNamedArg(lhs, rhs) =>
          print(lhs, " = ", rhs)

        case If(cond, thenp, elsep) =>
          print("if (", cond, ")"); indent(); println()
          print(thenp); undent()
          if (elsep.nonEmpty) {
            println(); print("else"); indent(); println(); print(elsep); undent()
          }

        case Return(expr) =>
          print("return ", expr)

        case Try(block, catches, finalizer) =>
          print("try "); printBlock(block)
          if (catches.nonEmpty) printColumn(catches, " catch {", "", "}")
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

        case st @ Super(This(qual), mix) =>
          printSuper(st, symName(tree, qual))

        case Super(qual, mix) =>
          print(qual, ".super")
          if (mix.nonEmpty)
            print("[" + mix + "]")

        case th @ This(qual) =>
          printThis(th, symName(tree, qual))

        case Select(qual: New, name) if !settings.debug =>
          print(qual)

        case Select(qualifier, name) =>
          print(backquotedPath(qualifier), ".", symName(tree, name))

        case id @ Ident(name) =>
          val str = symName(tree, name)
          print( if (id.isBackquoted) "`" + str + "`" else str )

        case Literal(x) =>
          print(x.escapedStringValue)

        case tt: TypeTree =>
          if ((tree.tpe eq null) || (printPositions && tt.original != null)) {
            if (tt.original != null) print("<type: ", tt.original, ">")
            else print("<type ?>")
          } else if ((tree.tpe.typeSymbol ne null) && tree.tpe.typeSymbol.isAnonymousClass) {
            print(tree.tpe.typeSymbol.toString)
          } else {
            print(tree.tpe.toString)
          }

        case an @ Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot() {
            print("@", tpt)
            if (args.nonEmpty)
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
          // Avoid printing noisy empty typebounds everywhere
          // Untyped empty bounds are not printed by printOpt,
          // but after they are typed we have to exclude Nothing/Any.
          if ((lo.tpe eq null) || !(lo.tpe =:= definitions.NothingTpe))
            printOpt(" >: ", lo)

          if ((hi.tpe eq null) || !(hi.tpe =:= definitions.AnyTpe))
            printOpt(" <: ", hi)

        case ExistentialTypeTree(tpt, whereClauses) =>
          print(tpt)
          printColumn(whereClauses, " forSome { ", ";", "}")

        // SelectFromArray is no longer visible in scala.reflect.internal.
        // eliminated until we figure out what we will do with both Printers and
        // SelectFromArray.
        // case SelectFromArray(qualifier, name, _) =>
        //   print(qualifier); print(".<arr>"); print(symName(tree, name))

        case tree =>
          xprintTree(this, tree)
      }
      printTypesInfo(tree)
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

  // it's the printer for AST-based code generation
  class CodePrinter(out: PrintWriter, printRootPkg: Boolean) extends TreePrinter(out) {
    protected val parentsStack = scala.collection.mutable.Stack[Tree]()

    protected def currentTree = if (parentsStack.nonEmpty) Some(parentsStack.top) else None

    protected def currentParent = if (parentsStack.length > 1) Some(parentsStack(1)) else None

    protected def printedName(name: Name, decoded: Boolean = true) = {
      import Chars._
      val decName = name.decoded
      val bslash = '\\'
      val isDot = (x: Char) => x == '.'
      val brackets = List('[',']','(',')','{','}')

      def addBackquotes(s: String) =
        if (decoded && (decName.exists(ch => brackets.contains(ch) || isWhitespace(ch) || isDot(ch)) ||
          (name.isOperatorName && decName.exists(isOperatorPart) && decName.exists(isScalaLetter) && !decName.contains(bslash))))
          s"`$s`" else s

      if (name == nme.CONSTRUCTOR) "this"
      else addBackquotes(quotedName(name, decoded))
    }

    protected def isIntLitWithDecodedOp(qual: Tree, name: Name) = {
      val qualIsIntLit = qual match {
        case Literal(Constant(x: Int)) => true
        case _ => false
      }
      qualIsIntLit && name.isOperatorName
    }

    override protected val commentsRequired = true

    protected def needsParentheses(parent: Tree)(insideIf: Boolean = true, insideMatch: Boolean = true, insideTry: Boolean = true,
        insideAnnotated: Boolean = true, insideBlock: Boolean = true, insideLabelDef: Boolean = true, insideAssign: Boolean = true) = {
      parent match {
        case _: If => insideIf
        case _: Match => insideMatch
        case _: Try => insideTry
        case _: Annotated => insideAnnotated
        case _: Block => insideBlock
        case _: LabelDef => insideLabelDef
        case _: Assign => insideAssign
        case _ => false
      }
    }

    protected def checkForBlank(cond: Boolean) = if (cond) " " else ""
    protected def blankForOperatorName(name: Name) = checkForBlank(name.isOperatorName)
    protected def blankForName(name: Name) = checkForBlank(name.isOperatorName || name.endsWith("_"))

    protected def resolveSelect(t: Tree): String = {
      t match {
        // case for: 1) (if (a) b else c).meth1.meth2 or 2) 1 + 5 should be represented as (1).+(5)
        case Select(qual, name) if (name.isTermName && needsParentheses(qual)(insideLabelDef = false)) || isIntLitWithDecodedOp(qual, name) => s"(${resolveSelect(qual)}).${printedName(name)}"
        case Select(qual, name) if name.isTermName => s"${resolveSelect(qual)}.${printedName(name)}"
        case Select(qual, name) if name.isTypeName => s"${resolveSelect(qual)}#${blankForOperatorName(name)}%${printedName(name)}"
        case Ident(name) => printedName(name)
        case _ => render(t, new CodePrinter(_, printRootPkg))
      }
    }

    object EmptyTypeTree {
      def unapply(tt: TypeTree): Boolean = tt match {
        case build.SyntacticEmptyTypeTree() if tt.wasEmpty || tt.isEmpty => true
        case _ => false
      }
    }

    protected def isEmptyTree(tree: Tree) =
      tree match {
        case EmptyTree | EmptyTypeTree() => true
        case _ => false
      }

    protected def originalTypeTrees(trees: List[Tree]) =
      trees.filter(!isEmptyTree(_)) map {
        case tt: TypeTree if tt.original != null => tt.original
        case tree => tree
      }

    val defaultClasses = List(tpnme.AnyRef, tpnme.Object)
    val defaultTraitsForCase = List(tpnme.Product, tpnme.Serializable)
    protected def removeDefaultTypesFromList(trees: List[Tree])(classesToRemove: List[Name] = defaultClasses)(traitsToRemove: List[Name]) = {
      def removeDefaultTraitsFromList(trees: List[Tree], traitsToRemove: List[Name]): List[Tree] =
        trees match {
          case Nil => trees
          case init :+ last => last match {
            case Select(Ident(sc), name) if traitsToRemove.contains(name) && sc == nme.scala_ =>
              removeDefaultTraitsFromList(init, traitsToRemove)
            case _ => trees
          }
        }

      removeDefaultTraitsFromList(removeDefaultClassesFromList(trees, classesToRemove), traitsToRemove)
    }

    protected def removeDefaultClassesFromList(trees: List[Tree], classesToRemove: List[Name] = defaultClasses) =
      originalTypeTrees(trees) filter {
        case Select(Ident(sc), name) => !(classesToRemove.contains(name) && sc == nme.scala_)
        case tt: TypeTree if tt.tpe != null => !(classesToRemove contains(newTypeName(tt.tpe.toString())))
        case _ => true
      }

    protected def syntheticToRemove(tree: Tree) =
      tree match {
        case _: ValDef | _: TypeDef => false // don't remove ValDef and TypeDef
        case md: MemberDef if md.mods.isSynthetic => true
        case _ => false
      }

    override def printOpt(prefix: String, tree: Tree) =
      if (!isEmptyTree(tree)) super.printOpt(prefix, tree)

    override def printColumn(ts: List[Tree], start: String, sep: String, end: String) = {
      super.printColumn(ts.filter(!syntheticToRemove(_)), start, sep, end)
    }

    def printFlags(mods: Modifiers, primaryCtorParam: Boolean = false): Unit = {
      val base = AccessFlags | OVERRIDE | ABSTRACT | FINAL | SEALED | LAZY
      val mask = if (primaryCtorParam) base else base | IMPLICIT

      val s = mods.flagString(mask)
      if (s != "") print(s"$s ")
      // case flag should be the last
      if (mods.isCase) print(mods.flagBitsToString(CASE) + " ")
      if (mods.isAbstractOverride) print("abstract override ")
    }

    override def printModifiers(tree: Tree, mods: Modifiers): Unit = printModifiers(mods, primaryCtorParam = false)

    def printModifiers(mods: Modifiers, primaryCtorParam: Boolean): Unit = {
      def modsAccepted = List(currentTree, currentParent) exists (_ map {
        case _: ClassDef | _: ModuleDef | _: Template | _: PackageDef => true
        case _ => false
      } getOrElse false)

      if (currentParent.isEmpty || modsAccepted)
        printFlags(mods, primaryCtorParam)
      else
        List(IMPLICIT, CASE, LAZY, SEALED).foreach{flag => if (mods.hasFlag(flag)) print(s"${mods.flagBitsToString(flag)} ")}
    }

    def printParam(tree: Tree, primaryCtorParam: Boolean): Unit =
      tree match {
        case vd @ ValDef(mods, name, tp, rhs) =>
          printPosition(tree)
          printAnnotations(vd)
          val mutableOrOverride = mods.isOverride || mods.isMutable
          val hideCtorMods = mods.isParamAccessor && mods.isPrivateLocal && !mutableOrOverride
          val hideCaseCtorMods = mods.isCaseAccessor && mods.isPublic && !mutableOrOverride

          if (primaryCtorParam && !(hideCtorMods || hideCaseCtorMods)) {
            printModifiers(mods, primaryCtorParam)
            print(if (mods.isMutable) "var " else "val ");
          }
          print(printedName(name), blankForName(name));
          printOpt(": ", tp);
          printOpt(" = ", rhs)
        case TypeDef(_, name, tparams, rhs) =>
          printPosition(tree)
          print(printedName(name))
          printTypeParams(tparams);
          print(rhs)
        case _ =>
          super.printParam(tree)
      }

    override def printParam(tree: Tree): Unit = {
      printParam(tree, primaryCtorParam = false)
    }

    protected def printArgss(argss: List[List[Tree]]) =
      argss foreach {x: List[Tree] => if (!(x.isEmpty && argss.size == 1)) printRow(x, "(", ", ", ")")}

    override def printAnnotations(tree: MemberDef) = {
      val annots = tree.mods.annotations
      annots foreach {annot => printAnnot(annot); print(" ")}
    }

    protected def printAnnot(tree: Tree) = {
      tree match {
        case treeInfo.Applied(core, _, argss) =>
          print("@")
          core match {
            case Select(New(tree), _) => print(tree)
            case _ =>
          }
          printArgss(argss)
        case _ => super.printTree(tree)
      }
    }

    override def printTree(tree: Tree): Unit = {
      parentsStack.push(tree)
      try {
        processTreePrinting(tree);
        printTypesInfo(tree)
      } finally parentsStack.pop()
    }

    def processTreePrinting(tree: Tree): Unit = {
      tree match {
        // don't remove synthetic ValDef/TypeDef
        case _ if syntheticToRemove(tree) =>

        case cl @ ClassDef(mods, name, tparams, impl) =>
          if (mods.isJavaDefined) super.printTree(cl)
          printAnnotations(cl)
          // traits
          val clParents: List[Tree] = if (mods.isTrait) {
            // avoid abstract modifier for traits
            printModifiers(tree, mods &~ ABSTRACT)
            print("trait ", printedName(name))
            printTypeParams(tparams)

            val build.SyntacticTraitDef(_, _, _, _, parents, _, _) = tree
            parents
          // classes
          } else {
            printModifiers(tree, mods)
            print("class ", printedName(name))
            printTypeParams(tparams)

            val build.SyntacticClassDef(_, _, _, ctorMods, vparamss, earlyDefs, parents, selfType, body) = cl

            // constructor's modifier
            if (ctorMods.hasFlag(AccessFlags) || ctorMods.hasAccessBoundary) {
              print(" ")
              printModifiers(ctorMods, primaryCtorParam = false)
            }

            def printConstrParams(ts: List[ValDef]): Unit = {
              parenthesize() {
                printImplicitInParamsList(ts)
                printSeq(ts)(printParam(_, primaryCtorParam = true))(print(", "))
              }
            }
            // constructor's params processing (don't print single empty constructor param list)
            vparamss match {
              case Nil | List(Nil) if (!mods.isCase && !ctorMods.hasFlag(AccessFlags)) =>
              case _ => vparamss foreach printConstrParams
            }
            parents
          }

          // get trees without default classes and traits (when they are last)
          val printedParents = removeDefaultTypesFromList(clParents)()(if (mods.hasFlag(CASE)) defaultTraitsForCase else Nil)
          print(if (mods.isDeferred) "<: " else if (printedParents.nonEmpty) " extends " else "", impl)

        case pd @ PackageDef(packaged, stats) =>
          packaged match {
            case Ident(name) if name == nme.EMPTY_PACKAGE_NAME =>
              printSeq(stats) {
                print(_)
              } {
                println()
                println()
              };
            case _ =>
              printPackageDef(pd, scala.util.Properties.lineSeparator)
          }

        case md @ ModuleDef(mods, name, impl) =>
          printAnnotations(md)
          printModifiers(tree, mods)
          val Template(parents, self, methods) = impl
          val parWithoutAnyRef = removeDefaultClassesFromList(parents)
          print("object " + printedName(name), if (parWithoutAnyRef.nonEmpty) " extends " else "", impl)

        case vd @ ValDef(mods, name, tp, rhs) =>
          printValDef(vd, printedName(name)) {
            // place space after symbolic def name (val *: Unit does not compile)
            printOpt(s"${blankForName(name)}: ", tp)
          } {
            if (!mods.isDeferred) print(" = ", if (rhs.isEmpty) "_" else rhs)
          }

        case dd @ DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printDefDef(dd, printedName(name)) {
            if (tparams.isEmpty && (vparamss.isEmpty || vparamss(0).isEmpty)) print(blankForName(name))
            printOpt(": ", tp)
          } {
            printOpt(" = " + (if (mods.isMacro) "macro " else ""), rhs)
          }

        case td @ TypeDef(mods, name, tparams, rhs) =>
          printTypeDef(td, printedName(name))

        case LabelDef(name, params, rhs) =>
          if (name.startsWith(nme.WHILE_PREFIX)) {
            val If(cond, thenp, elsep) = rhs
            print("while (", cond, ") ")
            val Block(list, wh) = thenp
            printColumn(list, "", ";", "")
          } else if (name.startsWith(nme.DO_WHILE_PREFIX)) {
            val Block(bodyList, ifCond @ If(cond, thenp, elsep)) = rhs
            print("do ")
            printColumn(bodyList, "", ";", "")
            print(" while (", cond, ") ")
          } else {
            print(printedName(name)); printLabelParams(params);
            printBlock(rhs)
          }

        case imp @ Import(expr, _) =>
          printImport(imp, resolveSelect(expr))

        case t @ Template(parents, self, tbody) =>
          val body = treeInfo.untypecheckedTemplBody(t)
          val printedParents =
            currentParent map {
              case _: CompoundTypeTree => parents
              case ClassDef(mods, name, _, _) if mods.isCase => removeDefaultTypesFromList(parents)()(List(tpnme.Product, tpnme.Serializable))
              case _ => removeDefaultClassesFromList(parents)
            } getOrElse (parents)

          val primaryCtr = treeInfo.firstConstructor(body)
          val ap: Option[Apply] = primaryCtr match {
              case DefDef(_, _, _, _, _, Block(ctBody, _)) =>
                val earlyDefs = treeInfo.preSuperFields(ctBody) ::: body.filter {
                  case td: TypeDef => treeInfo.isEarlyDef(td)
                  case _ => false
                }
                if (earlyDefs.nonEmpty) {
                  print("{")
                  printColumn(earlyDefs, "", ";", "")
                  print("} " + (if (printedParents.nonEmpty) "with " else ""))
                }
                ctBody collectFirst {
                  case apply: Apply => apply
                }
              case _ => None
            }

          if (printedParents.nonEmpty) {
            val (clParent :: traits) = printedParents
            print(clParent)

            val constrArgss = ap match {
              case Some(treeInfo.Applied(_, _, argss)) => argss
              case _ => Nil
            }
            printArgss(constrArgss)
            if (traits.nonEmpty) {
              printRow(traits, " with ", " with ", "")
            }
          }
          /* Remove primary constr def and constr val and var defs
           * right contains all constructors
           */
          val (left, right) = body.filter {
            // remove valdefs defined in constructor and presuper vals
            case vd: ValDef => !vd.mods.isParamAccessor && !treeInfo.isEarlyValDef(vd)
            // remove $this$ from traits
            case dd: DefDef => dd.name != nme.MIXIN_CONSTRUCTOR
            case td: TypeDef => !treeInfo.isEarlyDef(td)
            case EmptyTree => false
            case _ => true
          } span {
            case dd: DefDef => dd.name != nme.CONSTRUCTOR
            case _ => true
          }
          val modBody = (left ::: right.drop(1))
          val showBody = !(modBody.isEmpty && (self == noSelfType || self.isEmpty))
          if (showBody) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name);
              printOpt(": ", self.tpt);
              print(" =>")
            } else if (self.tpt.nonEmpty) {
              print(" { _ : ", self.tpt, " =>")
            } else {
              print(" {")
            }
            printColumn(modBody, "", ";", "}")
          }

        case bl @ Block(stats, expr) =>
          printBlock(treeInfo.untypecheckedBlockBody(bl), expr)

        case Match(selector, cases) =>
          /* Insert braces if match is inner
           * make this function available for other cases
           * passing required type for checking
           */
          def insertBraces(body: => Unit): Unit =
            if (parentsStack.nonEmpty && parentsStack.tail.exists(_.isInstanceOf[Match])) {
              print("(")
              body
              print(")")
            } else body

          val printParentheses = needsParentheses(selector)(insideLabelDef = false)
          tree match {
            case Match(EmptyTree, cs) =>
              printColumn(cases, "{", "", "}")
            case _ =>
              insertBraces {
                parenthesize(printParentheses)(print(selector))
                printColumn(cases, " match {", "", "}")
              }
          }

        case cd @ CaseDef(pat, guard, body) =>
          printCaseDef(cd)

        case Star(elem) =>
          print(elem, "*")

        case Bind(name, t) =>
          if (t == EmptyTree) print("(", printedName(name), ")")
          else if (t.exists(_.isInstanceOf[Star])) print(printedName(name), " @ ", t)
          else print("(", printedName(name), " @ ", t, ")")

        case f @ Function(vparams, body) =>
          // parentheses are not allowed for val a: Int => Int = implicit x => x
          val printParentheses = vparams match {
              case head :: _ => !head.mods.isImplicit
              case _ => true
            }
          printFunction(f)(printValueParams(vparams, inParentheses = printParentheses))

        case Typed(expr, tp) =>
          def printTp() = print("(", tp, ")")

          tp match {
            case EmptyTree | EmptyTypeTree() => printTp()
            // case for untypechecked trees
            case Annotated(annot, arg) if (expr ne null) && (arg ne null) && expr.equalsStructure(arg) => printTp() // remove double arg - 5: 5: @unchecked
            case tt: TypeTree if tt.original.isInstanceOf[Annotated] => printTp()
            case Function(List(), EmptyTree) => print("(", expr, " _)") //func _
            // parentheses required when (a match {}) : Type
            case _ => print("((", expr, "): ", tp, ")")
          }

        // print only fun when targs are TypeTrees with empty original
        case TypeApply(fun, targs) =>
          if (targs.exists(isEmptyTree(_))) {
            print(fun)
          } else super.printTree(tree)

        case Apply(fun, vargs) =>
          tree match {
            // processing methods ending on colons (x \: list)
            case Apply(Block(l1 @ List(sVD: ValDef), a1 @ Apply(Select(_, methodName), l2 @ List(Ident(iVDName)))), l3)
              if sVD.mods.isSynthetic && treeInfo.isLeftAssoc(methodName) && sVD.name == iVDName =>
              val printBlock = Block(l1, Apply(a1, l3))
              print(printBlock)
            case Apply(tree1, _) if (needsParentheses(tree1)(insideAnnotated = false)) =>
              parenthesize()(print(fun)); printRow(vargs, "(", ", ", ")")
            case _ => super.printTree(tree)
          }

        case UnApply(fun, args) =>
          fun match {
            case treeInfo.Unapplied(body) =>
              body match {
                case Select(qual, name) if name == nme.unapply  => print(qual)
                case TypeApply(Select(qual, name), _) if name == nme.unapply || name == nme.unapplySeq =>
                  print(qual)
                case _ => print(body)
              }
            case _ => print(fun)
          }
          printRow(args, "(", ", ", ")")

        case st @ Super(This(qual), mix) =>
          printSuper(st, printedName(qual), checkSymbol = false)

        case th @ This(qual) =>
          if (tree.hasExistingSymbol && tree.symbol.hasPackageFlag) print(tree.symbol.fullName)
          else printThis(th, printedName(qual))

        // remove this prefix from constructor invocation in typechecked trees: this.this -> this
        case Select(This(_), name @ nme.CONSTRUCTOR) => print(printedName(name))

        case Select(qual: New, name) =>
          print(qual)

        case Select(qual, name) =>
          def checkRootPackage(tr: Tree): Boolean =
            (currentParent match { //check that Select is not for package def name
              case Some(_: PackageDef) => false
              case _ => true
            }) && (tr match { // check that Select contains package
              case Select(q, _) => checkRootPackage(q)
              case _: Ident | _: This => val sym = tr.symbol
                tr.hasExistingSymbol && sym.hasPackageFlag && sym.name != nme.ROOTPKG
              case _ => false
            })

          if (printRootPkg && checkRootPackage(tree)) print(s"${printedName(nme.ROOTPKG)}.")
          val printParentheses = needsParentheses(qual)(insideAnnotated = false) || isIntLitWithDecodedOp(qual, name)
          if (printParentheses) print("(", resolveSelect(qual), ").", printedName(name))
          else print(resolveSelect(qual), ".", printedName(name))

        case id @ Ident(name) =>
          if (name.nonEmpty) {
            if (name == nme.dollarScope) {
              print(s"scala.xml.${nme.TopScope}")
            } else {
              val str = printedName(name)
              val strIsBackquoted = str.startsWith("`") && str.endsWith("`")
              print(if (id.isBackquoted && !strIsBackquoted) "`" + str + "`" else str)
            }
          } else {
            print("")
          }

        case Literal(k @ Constant(s: String)) if s.contains(Chars.LF) =>
          val tq = "\"" * 3
          val lines = s.lines.toList
          if (lines.lengthCompare(1) <= 0) print(k.escapedStringValue)
          else {
            val tqp = """["]{3}""".r
            val tqq = """""\\""""     // ""\" is triple-quote quoted
            print(tq)
            printSeq(lines.map(x => tqp.replaceAllIn(x, tqq)))(print(_))(print(Chars.LF))
            print(tq)
          }

        case Literal(x) =>
          // processing Float constants
          val suffix = x.value match { case _: Float => "F" case _ => "" }
          print(s"${x.escapedStringValue}${suffix}")

        case an @ Annotated(ap, tree) =>
          val printParentheses = needsParentheses(tree)()
          parenthesize(printParentheses) { print(tree) }; print(if (tree.isType) " " else ": ")
          printAnnot(ap)

        case SelectFromTypeTree(qualifier, selector) =>
          print("(", qualifier, ")#", blankForOperatorName(selector), printedName(selector))

        case tt: TypeTree =>
          if (!isEmptyTree(tt)) {
            val original = tt.original
            if (original != null) print(original)
            else super.printTree(tree)
          }

        case AppliedTypeTree(tp, args) =>
          // it's possible to have (=> String) => String type but Function1[=> String, String] is not correct
          val containsByNameTypeParam = args exists treeInfo.isByNameParamType

          if (containsByNameTypeParam) {
            print("(")
            printRow(args.init, "(", ", ", ")")
            print(" => ", args.last, ")")
          } else {
            if (treeInfo.isRepeatedParamType(tree) && args.nonEmpty) {
              print(args(0), "*")
            } else if (treeInfo.isByNameParamType(tree)) {
              print("=> ", if (args.isEmpty) "()" else args(0))
            } else
              super.printTree(tree)
          }

        case ExistentialTypeTree(tpt, whereClauses) =>
          print("(", tpt);
          printColumn(whereClauses, " forSome { ", ";", "})")

        case EmptyTree =>

        case tree => super.printTree(tree)
      }
    }
  }

  /** Hook for extensions */
  def xprintTree(treePrinter: TreePrinter, tree: Tree) =
    treePrinter.print(tree.productPrefix+tree.productIterator.mkString("(", ", ", ")"))

  def newCodePrinter(writer: PrintWriter, tree: Tree, printRootPkg: Boolean): TreePrinter =
    new CodePrinter(writer, printRootPkg)

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

  // provides footnotes for types and mirrors
  private class Footnotes {
    import scala.collection.mutable.{Map, WeakHashMap, SortedSet}

    private val index = Map[Class[_], WeakHashMap[Any, Int]]()
    private def classIndex[T: ClassTag] = index.getOrElseUpdate(classTag[T].runtimeClass, WeakHashMap[Any, Int]())

    private val counters = Map[Class[_], Int]()
    private def nextCounter[T: ClassTag] = {
      val clazz = classTag[T].runtimeClass
      counters.getOrElseUpdate(clazz, 0)
      counters(clazz) = counters(clazz) + 1
      counters(clazz)
    }

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

  // emits more or less verbatim representation of the provided tree
  class RawTreePrinter(out: PrintWriter) extends super.TreePrinter {
    private var depth = 0
    private var printTypesInFootnotes = true
    private var printingFootnotes = false
    private val footnotes = new Footnotes()

    def print(args: Any*): Unit = {
      // don't print type footnotes if the argument is a mere type
      if (depth == 0 && args.length == 1 && args(0) != null && args(0).isInstanceOf[Type])
        printTypesInFootnotes = false

      depth += 1
      args foreach {
        case expr: Expr[_] =>
          print("Expr")
          if (printTypes) print(expr.staticType)
          print("(")
          print(expr.tree)
          print(")")
        case EmptyTree =>
          print("EmptyTree")
        case self.noSelfType =>
          print("noSelfType")
        case self.pendingSuperCall =>
          print("pendingSuperCall")
        case tree: Tree =>
          def hasSymbolField = tree.hasSymbolField && tree.symbol != NoSymbol
          val isError = hasSymbolField && (tree.symbol.name string_== nme.ERROR)
          printProduct(
            tree,
            preamble = _ => {
              if (printPositions) print(tree.pos.show)
              print(tree.productPrefix)
              if (printTypes && tree.tpe != null) print(tree.tpe)
            },
            body = {
              case name: Name =>
                if (isError) {
                  if (isError) print("<")
                  print(name)
                  if (isError) print(": error>")
                } else if (hasSymbolField) {
                  tree match {
                    case refTree: RefTree =>
                      if (tree.symbol.name != refTree.name) print("[", tree.symbol, " aka ", refTree.name, "]")
                      else print(tree.symbol)
                    case defTree: DefTree =>
                      print(tree.symbol)
                    case _ =>
                      print(tree.symbol.name)
                  }
                } else {
                  print(name)
                }
              case Constant(s: String) =>
                print("Constant(\"" + s + "\")")
              case Constant(null) =>
                print("Constant(null)")
              case Constant(value) =>
                print("Constant(" + value + ")")
              case arg =>
                print(arg)
            },
            postamble = {
              case tree @ TypeTree() if tree.original != null => print(".setOriginal(", tree.original, ")")
              case _ => // do nothing
            })
        case sym: Symbol =>
          if (sym == NoSymbol) print("NoSymbol")
          else if (sym.isStatic && (sym.isClass || sym.isModule)) print(sym.fullName)
          else print(sym.name)
          if (printIds) print("#", sym.id)
          if (printOwners) print("@", sym.owner.id)
          if (printKinds) print("#", sym.abbreviatedKindString)
          if (printMirrors) print("%M", footnotes.put[scala.reflect.api.Mirror[_]](mirrorThatLoaded(sym)))
        case tag: TypeTag[_] =>
          print("TypeTag(", tag.tpe, ")")
        case tag: WeakTypeTag[_] =>
          print("WeakTypeTag(", tag.tpe, ")")
        case tpe: Type =>
          val defer = printTypesInFootnotes && !printingFootnotes
          if (defer) print("[", footnotes.put(tpe), "]")
          else tpe match {
            case NoType => print("NoType")
            case NoPrefix => print("NoPrefix")
            case _ => printProduct(tpe.asInstanceOf[Product])
          }
        case mods: Modifiers =>
          print("Modifiers(")
          if (mods.flags != NoFlags || mods.privateWithin != tpnme.EMPTY || mods.annotations.nonEmpty) print(show(mods.flags))
          if (mods.privateWithin != tpnme.EMPTY || mods.annotations.nonEmpty) { print(", "); print(mods.privateWithin) }
          if (mods.annotations.nonEmpty) { print(", "); print(mods.annotations); }
          print(")")
        case name: Name =>
          print(show(name))
        case scope: Scope =>
          print("Scope")
          printIterable(scope.toList)
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
        footnotes.print[scala.reflect.api.Mirror[_]](this)
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
        body(it.next())
        print(if (it.hasNext) ", " else "")
      }
      print(")")
      postamble
    }
  }

  def show(name: Name): String = name match {
    case tpnme.WILDCARD => "typeNames.WILDCARD"
    case tpnme.EMPTY => "typeNames.EMPTY"
    case tpnme.ERROR => "typeNames.ERROR"
    case tpnme.PACKAGE => "typeNames.PACKAGE"
    case tpnme.WILDCARD_STAR => "typeNames.WILDCARD_STAR"
    case nme.WILDCARD => "termNames.WILDCARD"
    case nme.EMPTY => "termNames.EMPTY"
    case nme.ERROR => "termNames.ERROR"
    case nme.PACKAGE => "termNames.PACKAGE"
    case nme.CONSTRUCTOR => "termNames.CONSTRUCTOR"
    case nme.ROOTPKG => "termNames.ROOTPKG"
    case _ =>
      val prefix = if (name.isTermName) "TermName(\"" else "TypeName(\""
      prefix + name.toString + "\")"
  }

  def show(flags: FlagSet): String = {
    if (flags == NoFlags) nme.NoFlags.toString
    else {
      val s_flags = new scala.collection.mutable.ListBuffer[String]
      def hasFlag(left: Long, right: Long): Boolean = (left & right) != 0
      for (i <- 0 to 63 if hasFlag(flags, 1L << i))
        s_flags += flagToString(1L << i).replace("<", "").replace(">", "").toUpperCase
      s_flags mkString " | "
    }
  }

  def show(position: Position): String = {
    position.show
  }

  def showDecl(sym: Symbol): String = {
    if (!isCompilerUniverse) definitions.fullyInitializeSymbol(sym)
    sym.defString
  }
}
