/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable
import scala.reflect.internal.util.shortClassOfInstance
import scala.reflect.internal.util.StringOps._

abstract class TreeCheckers extends Analyzer {
  import global._

  override protected def onTreeCheckerError(pos: Position, msg: String) {
    if (settings.fatalWarnings)
      reporter.warning(pos, "\n** Error during internal checking:\n" + msg)
  }

  case class DiffResult[T](lost: List[T], gained: List[T]) {
    def isEmpty  = lost.isEmpty && gained.isEmpty
    def lost_s   = if (lost.isEmpty) "" else lost.mkString("lost: ", ", ", "")
    def gained_s = if (gained.isEmpty) "" else gained.mkString("gained: ", ", ", "")
    override def toString = ojoin(lost_s, gained_s)
  }

  def diffList[T](xs: List[T], ys: List[T]): DiffResult[T] =
    DiffResult(xs filterNot ys.contains, ys filterNot xs.contains)

  def diffTrees(t1: Tree, t2: Tree): DiffResult[Tree] =
    diffList(t1 filter (_ ne t1), t2 filter (_ ne t2))

  def diffTemplates(t1: Template, t2: Template): String = {
    val parents = diffList(t1.parents, t2.parents).toString match { case "" => "" case s => "parents " + s }
    val stats   = diffList(t1.body, t2.body).toString match { case ""      => "" case s => "stats " + s }
    oempty(parents, stats) mkString ", "
  }

  def diff(t1: Tree, t2: Tree): String = (t1, t2) match {
    case (_: Literal, _: Literal)     => ""
    case (t1: ImplDef, t2: ImplDef)   => diff(t1.impl, t2.impl)
    case (t1: Template, t2: Template) => diffTemplates(t1, t2)
    case _                            => diffTrees(t1, t2).toString // "<error: different tree classes>"
  }

  private def clean_s(s: String) = s.replaceAllLiterally("scala.collection.", "s.c.")
  private def typestr(x: Type)    = " (tpe = " + x + ")"
  private def treestr(t: Tree)    = t + " [" + classString(t) + "]" + typestr(t.tpe)
  private def ownerstr(s: Symbol) = "'" + s + "'" + s.locationString
  private def wholetreestr(t: Tree) = nodeToString(t) + "\n"
  private def truncate(str: String, len: Int): String = (
    if (str.length <= len) str
    else (str takeWhile (_ != '\n') take len - 3) + "..."
  )
  private def signature(sym: Symbol) = clean_s(sym match {
    case null           => "null"
    case _: ClassSymbol => sym.name + ": " + sym.tpe_*
    case _              => sym.defString
  })
  private def classString(x: Any) = x match {
    case null      => ""
    case t: Tree   => t.shortClass
    case s: Symbol => s.shortSymbolClass
    case x: AnyRef => shortClassOfInstance(x)
  }
  private def nonPackageOwners(s: Symbol) = s.ownerChain drop 1 takeWhile (!_.hasPackageFlag)
  private def nonPackageOwnersPlusOne(s: Symbol) = nonPackageOwners(s) ::: (s.ownerChain dropWhile (!_.hasPackageFlag) take 1)
  private def ownersString(s: Symbol) = nonPackageOwnersPlusOne(s) match {
    case Nil => "NoSymbol"
    case xs  => xs mkString " -> "
  }

  private def beststr(t: Tree) = "<" + {
    if (t.symbol != null && t.symbol != NoSymbol) "sym=" + ownerstr(t.symbol)
    else if (t.tpe.isComplete) "tpe=" + typestr(t.tpe)
    else t match {
      case x: DefTree => "name=" + x.name
      case x: RefTree => "reference=" + x.name
      case _          => "clazz=" + classString(t)
    }
  } + ">"

  /** This is a work in progress, don't take it too seriously.
   */
  object SymbolTracker extends Traverser {
    type PhaseMap = mutable.Map[Symbol, List[Tree]]
    def symbolTreeMap[T <: Tree]() = mutable.Map[Symbol, List[T]]() withDefaultValue Nil

    var maps: List[(Phase, PhaseMap)] = ((NoPhase, null)) :: Nil
    def prev          = maps.tail.head._2
    def latest        = maps.head._2
    val defSyms       = symbolTreeMap[DefTree]()
    val newSyms       = mutable.HashSet[Symbol]()
    val movedMsgs     = mutable.ListBuffer[String]()
    def sortedNewSyms = newSyms.toList.distinct sortBy (_.name.toString)

    def record(tree: Tree) {
      val sym = tree.symbol
      if ((sym eq null) || (sym eq NoSymbol)) return

      val prevMap   = maps.tail.head._2
      val prevTrees = if (prevMap eq null) Nil else prevMap(sym)

      tree match {
        case t: DefTree => defSyms(sym) ::= t
        case _          =>
      }

      if (prevTrees.isEmpty)
        newSyms += sym
      else if (prevTrees exists (t => (t eq tree) || (t.symbol == sym)))
        ()
      else {
        val s1 = (prevTrees map wholetreestr).sorted.distinct
        val s2 = wholetreestr(tree)
        if (s1 contains s2) ()
        else movedMsgs += ("\n** %s moved:\n** Previously:\n%s\n** Currently:\n%s".format(ownerstr(sym), s1 mkString ", ", s2))
      }
    }

    def reportChanges(): Unit = {
      // new symbols
      if (newSyms.nonEmpty) {
        informFn(newSyms.size + " new symbols.")
        val toPrint = if (settings.debug) sortedNewSyms mkString " " else ""

        newSyms.clear()
        if (toPrint != "")
          informFn(toPrint)
      }

      // moved symbols
      movedMsgs foreach errorFn
      movedMsgs.clear()

      // duplicate defs
      for ((sym, defs) <- defSyms ; if defs.size > 1) {
        errorFn("%s DefTrees with symbol '%s': %s".format(defs.size, ownerstr(sym), defs map beststr mkString ", "))
      }
      defSyms.clear()
    }

    def check(ph: Phase, unit: CompilationUnit): Unit = {
      maps match {
        case ((`ph`, _)) :: _ =>
        case _                => maps ::= ((ph, symbolTreeMap[Tree]()))
      }
      traverse(unit.body)
      reportChanges()
    }
    override def traverse(tree: Tree) {
      record(tree)
      super.traverse(tree)
    }
  }

  lazy val tpeOfTree = mutable.HashMap[Tree, Type]()
  private lazy val reportedAlready = mutable.HashSet[(Tree, Symbol)]()

  def posstr(p: Position): String = (
    if (p eq null) "" else {
      try p.source.path + ":" + p.line
      catch { case _: UnsupportedOperationException => p.toString }
    }
  )


  def errorFn(pos: Position, msg: Any): Unit = reporter.warning(pos, "[check: %s] %s".format(phase.prev, msg))
  def errorFn(msg: Any): Unit                = errorFn(NoPosition, msg)

  def informFn(msg: Any) {
    if (settings.verbose || settings.debug)
      println("[check: %s] %s".format(phase.prev, msg))
  }

  def assertFn(cond: Boolean, msg: => Any) =
    if (!cond) errorFn(msg)

  private def wrap[T](msg: => Any)(body: => T): T = {
    try body
    catch { case x: Throwable =>
      Console.println("Caught " + x)
      Console.println(msg)
      x.printStackTrace
      null.asInstanceOf[T]
    }
  }

  def checkTrees() {
    if (settings.verbose)
      Console.println("[consistency check at the beginning of phase " + phase + "]")

    currentRun.units foreach (x => wrap(x)(check(x)))
  }

  def runWithUnit[T](unit: CompilationUnit)(body: => Unit): Unit = {
    val unit0 = currentUnit
    currentRun.currentUnit = unit
    body
    currentRun.advanceUnit()
    assertFn(currentUnit == unit, "currentUnit is " + currentUnit + ", but unit is " + unit)
    currentRun.currentUnit = unit0
  }
  def check(unit: CompilationUnit) {
    informProgress("checking "+unit)
    val context = rootContext(unit, checking = true)
    tpeOfTree.clear()
    SymbolTracker.check(phase, unit)
    val checker = new TreeChecker(context)
    runWithUnit(unit) {
      checker.precheck.traverse(unit.body)
      checker.typed(unit.body)
      checker.postcheck.traverse(unit.body)
    }
  }

  override def newTyper(context: Context): Typer = new TreeChecker(context)

  class TreeChecker(context0: Context) extends Typer(context0) {
    // If we don't intercept this all the synthetics get added at every phase,
    // with predictably unfortunate results.
    override protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template = templ

    // XXX check for tree.original on TypeTrees.
    private def treesDiffer(t1: Tree, t2: Tree): Unit = {
      def len1 = t1.toString.length
      def len2 = t2.toString.length
      def name = t1 match {
        case t: NameTree => t.name
        case _           => t1.summaryString
      }
      def summary = s"${t1.shortClass} $name differs, bytes $len1 -> $len2, "
      errorFn(t1.pos, summary + diff(t1, t2))
    }

    private def typesDiffer(tree: Tree, tp1: Type, tp2: Type) =
      errorFn(tree.pos, "types differ\n old: " + tp1 + "\n new: " + tp2 + "\n tree: " + tree)

    /** XXX Disabled reporting of position errors until there is less noise. */
    private def noPos(t: Tree) =
      () // errorFn("no pos: " + treestr(t))
    private def noType(t: Tree) =
      errorFn(t.pos, "no type: " + treestr(t))

    private def checkSym(t: Tree) =
      if (t.symbol == NoSymbol)
        errorFn(t.pos, "no symbol: " + treestr(t))

    private def passThrough(tree: Tree) = tree match {
      case EmptyTree | TypeTree() => true
      case _                      => tree.tpe eq null
    }
    override def typed(tree: Tree, mode: Mode, pt: Type): Tree = (
      if (passThrough(tree))
        super.typed(tree, mode, pt)
      else
        checkedTyped(tree, mode, pt)
    )
    private def checkedTyped(tree: Tree, mode: Mode, pt: Type): Tree = {
      val typed = wrap(tree)(super.typed(tree.clearType(), mode, pt))

      // Vlad: super.typed returns null for package defs, why is that?
      if (typed eq null)
        return tree

      if (typed.tpe ne null)
        assert(!typed.tpe.isErroneous, "Tree has erroneous type: " + typed)

      if (tree ne typed)
        treesDiffer(tree, typed)
      tree
    }

    object precheck extends TreeStackTraverser {
      private var enclosingMemberDefs: List[MemberDef] = Nil
      private def pushMemberDef[T](md: MemberDef)(body: => T): T = {
        enclosingMemberDefs ::= md
        try body finally enclosingMemberDefs = enclosingMemberDefs.tail
      }
      override def traverse(tree: Tree): Unit = tree match {
        case md: MemberDef => pushMemberDef(md)(traverseInternal(tree))
        case _             => traverseInternal(tree)
      }

      private def traverseInternal(tree: Tree) {
        if (!tree.canHaveAttrs)
          return

        checkSymbolRefsRespectScope(enclosingMemberDefs takeWhile (md => !md.symbol.hasPackageFlag), tree)
        checkReturnReferencesDirectlyEnclosingDef(tree)

        val sym = tree.symbol
        def accessed = sym.accessed
        def fail(msg: String) = errorFn(tree.pos, msg + tree.shortClass + " / " + tree)

        tree match {
          case DefDef(_, _, _, _, _, _) =>
            if (sym.hasAccessorFlag && !sym.isDeferred) {
              sym.tpe.resultType match {
                case _: ConstantType  => ()
                case _                =>
                  checkSym(tree)
                  /* XXX: lots of syms show up here with accessed == NoSymbol. */
                  if (accessed != NoSymbol) {
                    val agetter = accessed.getterIn(sym.owner)
                    val asetter = accessed.setterIn(sym.owner)

                    assertFn(agetter == sym || asetter == sym,
                      sym + " is getter or setter, but accessed sym " + accessed + " shows " + agetter + " and " + asetter
                    )
                  }
              }
            }
          case ValDef(_, _, _, _) =>
            if (sym.hasGetter && !sym.isOuterField && !sym.isOuterAccessor) {
              assertFn(sym.getterIn(sym.owner) != NoSymbol, ownerstr(sym) + " has getter but cannot be found. " + sym.ownerChain)
            }
          case Apply(fn, args) =>
            if (args exists (_ == EmptyTree))
              errorFn(tree.pos, "Apply arguments to " + fn + " contains an empty tree: " + args)

          case Select(qual, name) =>
            checkSym(tree)
          case This(_) =>
            checkSym(tree)
            if (sym.isStatic && sym.hasModuleFlag) ()
            else if (currentOwner.ownerChain takeWhile (_ != sym) exists (_ == NoSymbol))
              return fail("tree symbol "+sym+" does not point to enclosing class; tree = ")

          /* XXX: temporary while Import nodes are arriving untyped. */
          case Import(_, _) =>
            return
          case _ =>
        }
        if (tree.pos == NoPosition)
          noPos(tree)
        else if (tree.tpe == null && isPastTyper)
          noType(tree)
        else if (tree.isDef) {
          checkSym(tree)

          tree match {
            case x: PackageDef    =>
              if ((sym.ownerChain contains currentOwner) || currentOwner.isEmptyPackageClass) ()
              else fail(sym + " owner chain does not contain currentOwner " + currentOwner + sym.ownerChain)
            case _ =>
              def cond(s: Symbol) = !s.isTerm || s.isMethod || s == sym.owner

              if (sym.owner != currentOwner) {
                val expected = currentOwner.ownerChain find (x => cond(x)) getOrElse { fail("DefTree can't find owner: ") ; NoSymbol }
                if (sym.owner != expected)
                  fail(sm"""|
                            | currentOwner chain: ${currentOwner.ownerChain take 3 mkString " -> "}
                            |       symbol chain: ${sym.ownerChain mkString " -> "}"""
                      )
              }
          }
        }
        super.traverse(tree)
      }

      private def checkSymbolRefsRespectScope(enclosingMemberDefs: List[MemberDef], tree: Tree) {
        def symbolOf(t: Tree): Symbol  = if (t.symbol eq null) NoSymbol else t.symbol
        def typeOf(t: Tree): Type      = if (t.tpe eq null) NoType else t.tpe
        def infoOf(t: Tree): Type      = symbolOf(t).info
        def referencesInType(tp: Type) = tp collect { case TypeRef(_, sym, _) => sym }
        // Accessors are known to steal the type of the underlying field without cloning existential symbols at the new owner.
        // This happens in Namer#accessorTypeCompleter. We just look the other way here.
        if (symbolOf(tree).isAccessor)
          return

        val treeSym  = symbolOf(tree)
        val treeInfo = infoOf(tree)
        val treeTpe  = typeOf(tree)

        def isOk(sym: Symbol) = treeSym hasTransOwner sym.enclosingSuchThat(x => !x.isTypeParameterOrSkolem) // account for higher order type params
        def isEligible(sym: Symbol) = (sym ne NoSymbol) && (
             sym.isTypeParameter
          || sym.isLocalToBlock
        )
        val referencedSymbols = (treeSym :: referencesInType(treeInfo)).distinct filter (sym => isEligible(sym) && !isOk(sym))
        def mk[T](what: String, x: T, str: T => String = (x: T) => "" + x): ((Any, String)) =
          x -> s"%10s  %-20s %s".format(what, classString(x), truncate(str(x), 80).trim)

        def encls = enclosingMemberDefs.filterNot(_.symbol == treeSym).zipWithIndex map { case (md, i) => mk(s"encl(${i+1})", md.symbol, signature) }

        def mkErrorMsg(outOfScope: Symbol): String = {

          def front = List(
            mk[Tree]("tree", tree),
            mk[Position]("position", tree.pos, posstr),
            mk("with sym", treeSym, signature)
          )
          def tpes = treeTpe match {
            case NoType => Nil
            case _      => mk[Type]("and tpe", treeTpe) :: Nil
          }
          def ref = mk[Symbol]("ref to", outOfScope, (s: Symbol) => s.nameString + " (" + s.debugFlagString + ")")

          val pairs = front ++ tpes ++ encls ++ (ref :: Nil)
          val width = pairs.map(_._2.length).max
          val fmt = "%-" + width + "s"
          val lines = pairs map {
            case (s: Symbol, msg) => fmt.format(msg) + "  in  " + ownersString(s)
            case (x, msg)         => fmt.format(msg)
          }
          lines.mkString("Out of scope symbol reference {\n", "\n", "\n}")
        }

        referencedSymbols foreach (sym =>
          if (!reportedAlready((tree, sym))) {
            errorFn("\n" + mkErrorMsg(sym))
            reportedAlready += ((tree, sym))
          }
        )
      }

      private def checkReturnReferencesDirectlyEnclosingDef(tree: Tree): Unit = tree match {
        case _: Return =>
          path collectFirst { case dd: DefDef => dd } match {
            case None                                 => errorFn(s"Return node ($tree) must be enclosed in a DefDef")
            case Some(dd) if tree.symbol != dd.symbol => errorFn(s"Return symbol (${tree.symbol}} does not reference directly enclosing DefDef (${dd.symbol})")
            case _                                    =>
          }
        case _ =>
      }
    }

    object postcheck extends Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case EmptyTree | TypeTree() => ()
        case _ =>
          tpeOfTree get tree foreach { oldtpe =>
            if (tree.tpe eq null)
              errorFn(s"tree.tpe=null for " + tree.shortClass + " (symbol: " + classString(tree.symbol) + " " + signature(tree.symbol) + "), last seen tpe was " + oldtpe)
            else if (oldtpe =:= tree.tpe)
              ()
            else
              typesDiffer(tree, oldtpe, tree.tpe)

            super.traverse(tree setType oldtpe)
          }
      }
    }
  }
}
