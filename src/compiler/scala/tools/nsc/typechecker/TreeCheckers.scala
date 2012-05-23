/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.tools.nsc.symtab.Flags._
import scala.collection.mutable
import mutable.ListBuffer
import util.returning

abstract class TreeCheckers extends Analyzer {
  import global._

  private def classstr(x: AnyRef) = x.getClass.getName split """\\.|\\$""" last;
  private def typestr(x: Type)    = " (tpe = " + x + ")"
  private def treestr(t: Tree)    = t + " [" + classstr(t) + "]" + typestr(t.tpe)
  private def ownerstr(s: Symbol) = "'" + s + "'" + s.locationString
  private def wholetreestr(t: Tree) = nodeToString(t) + "\n"

  private def beststr(t: Tree) = "<" + {
    if (t.symbol != null && t.symbol != NoSymbol) "sym=" + ownerstr(t.symbol)
    else if (t.tpe.isComplete) "tpe=" + typestr(t.tpe)
    else t match {
      case x: DefTree => "name=" + x.name
      case x: RefTree => "reference=" + x.name
      case _          => "clazz=" + classstr(t)
    }
  } + ">"

  /** This is a work in progress, don't take it too seriously.
   */
  object SymbolTracker extends Traverser {
    type PhaseMap = mutable.HashMap[Symbol, List[Tree]]

    val maps          = ListBuffer[(Phase, PhaseMap)]()
    def prev          = maps.init.last._2
    def latest        = maps.last._2
    val defSyms       = mutable.HashMap[Symbol, List[DefTree]]()
    val newSyms       = mutable.HashSet[Symbol]()
    val movedMsgs     = new ListBuffer[String]
    def sortedNewSyms = newSyms.toList.distinct sortBy (_.name.toString)

    def inPrev(sym: Symbol) = {
      (maps.size >= 2) && (prev contains sym)
    }
    def record(sym: Symbol, tree: Tree) = {
      if (latest contains sym) latest(sym) = latest(sym) :+ tree
      else latest(sym) = List(tree)

      if (inPrev(sym)) {
        val prevTrees = prev(sym)

        if (prevTrees exists (t => (t eq tree) || (t.symbol == sym))) ()
        else if (prevTrees exists (_.symbol.owner == sym.owner.implClass)) {
          errorFn("Noticed " + ownerstr(sym) + " moving to implementation class.")
        }
        else {
          val s1 = (prevTrees map wholetreestr).sorted.distinct
          val s2 = wholetreestr(tree)
          if (s1 contains s2) ()
          else movedMsgs += ("\n** %s moved:\n** Previously:\n%s\n** Currently:\n%s".format(ownerstr(sym), s1 mkString ", ", s2))
        }
      }
      else newSyms += sym
    }
    def reportChanges(): Unit = {
      // new symbols
      if (newSyms.nonEmpty) {
        informFn(newSyms.size + " new symbols.")
        val toPrint = if (settings.debug.value) sortedNewSyms mkString " " else ""

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
      if (maps.isEmpty || maps.last._1 != ph)
        maps += ((ph, new PhaseMap))

      traverse(unit.body)
      reportChanges()
    }
    override def traverse(tree: Tree): Unit = {
      val sym    = tree.symbol
      if (sym != null && sym != NoSymbol) {
        record(sym, tree)
        tree match {
          case x: DefTree =>
            if (defSyms contains sym) defSyms(sym) = defSyms(sym) :+ x
            else defSyms(sym) = List(x)
          case _ => ()
        }
      }

      super.traverse(tree)
    }
  }

  lazy val tpeOfTree = mutable.HashMap[Tree, Type]()

  def posstr(p: Position) =
    try p.source.path + ":" + p.line
    catch { case _: UnsupportedOperationException => p.toString }

  def errorFn(msg: Any): Unit                = println("[check: %s] %s".format(phase.prev, msg))
  def errorFn(pos: Position, msg: Any): Unit = errorFn(posstr(pos) + ": " + msg)
  def informFn(msg: Any) {
    if (settings.verbose.value || settings.debug.value)
      println("[check: %s] %s".format(phase.prev, msg))
  }

  def assertFn(cond: Boolean, msg: => Any) =
    if (!cond) errorFn(msg)

  private def wrap[T](msg: => Any)(body: => Unit) {
    try body
    catch { case x =>
      Console.println("Caught " + x)
      Console.println(msg)
      x.printStackTrace
    }
  }

  def checkTrees() {
    if (settings.verbose.value)
      Console.println("[consistency check at the beginning of phase " + phase + "]")

    currentRun.units foreach (x => wrap(x)(check(x)))
  }

  def printingTypings[T](body: => T): T = {
    val saved = global.printTypings
    global.printTypings = true
    val result = body
    global.printTypings = saved
    result
  }
  def runWithUnit[T](unit: CompilationUnit)(body: => Unit): Unit = {
    val unit0 = currentUnit
    currentRun.currentUnit = unit
    body
    currentRun.advanceUnit
    assertFn(currentUnit == unit, "currentUnit is " + currentUnit + ", but unit is " + unit)
    currentRun.currentUnit = unit0
  }
  def check(unit: CompilationUnit) {
    informProgress("checking "+unit)
    val context = rootContext(unit)
    context.checking = true
    tpeOfTree.clear
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
    override protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template = {
      // If we don't intercept this all the synthetics get added at every phase,
      // with predictably unfortunate results.
      templ
    }

    // XXX check for tree.original on TypeTrees.
    private def treesDiffer(t1: Tree, t2: Tree) =
      errorFn(t1.pos, "trees differ\n old: " + treestr(t1) + "\n new: " + treestr(t2))
    private def typesDiffer(tree: Tree, tp1: Type, tp2: Type) =
      errorFn(tree.pos, "types differ\n old: " + tp1 + "\n new: " + tp2 + "\n tree: " + tree)
    private def ownersDiffer(tree: Tree, shouldBe: Symbol) = {
      val sym = tree.symbol
      errorFn(tree.pos, sym + " has wrong owner: " + ownerstr(sym.owner) + ", should be: " + ownerstr(shouldBe))
    }

    /** XXX Disabled reporting of position errors until there is less noise. */
    private def noPos(t: Tree) =
      () // errorFn("no pos: " + treestr(t))
    private def noType(t: Tree) =
      errorFn(t.pos, "no type: " + treestr(t))

    private def checkSym(t: Tree) =
      if (t.symbol == NoSymbol)
        errorFn(t.pos, "no symbol: " + treestr(t))

    override def typed(tree: Tree, mode: Int, pt: Type): Tree = returning(tree) {
      case EmptyTree | TypeTree() => ()
      case _ if tree.tpe != null  =>
        tpeOfTree.getOrElseUpdate(tree, {
          val saved = tree.tpe
          tree.tpe = null
          saved
        })
        wrap(tree)(super.typed(tree, mode, pt) match {
          case _: Literal     => ()
          case x if x ne tree => treesDiffer(tree, x)
          case _              => ()
        })
      case _ => ()
    }

    object precheck extends Traverser {
      override def traverse(tree: Tree) {
        val sym = tree.symbol
        def accessed = sym.accessed
        def fail(msg: String) = errorFn(tree.pos, msg + classstr(tree) + " / " + tree)

        tree match {
          case DefDef(_, _, _, _, _, _) =>
            if (sym.hasAccessorFlag && !sym.isDeferred) {
              sym.tpe.resultType match {
                case _: ConstantType  => ()
                case _                =>
                  checkSym(tree)
                  /** XXX: lots of syms show up here with accessed == NoSymbol. */
                  if (accessed != NoSymbol) {
                    val agetter = accessed.getter(sym.owner)
                    val asetter = accessed.setter(sym.owner)

                    assertFn(agetter == sym || asetter == sym,
                      sym + " is getter or setter, but accessed sym " + accessed + " shows " + agetter + " and " + asetter
                    )
                  }
              }
            }
          case ValDef(_, _, _, _) =>
            if (sym.hasGetter && !sym.isOuterField) {
              assertFn(sym.getter(sym.owner) != NoSymbol, ownerstr(sym) + " has getter but cannot be found. " + sym.ownerChain)
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

          /** XXX: temporary while Import nodes are arriving untyped. */
          case Import(_, _) =>
            return
          case _ =>
        }

        if (tree.pos == NoPosition && tree != EmptyTree)
          noPos(tree)
        else if (tree.tpe == null && phase.id > currentRun.typerPhase.id)
          noType(tree)
        else if (tree.isDef) {
          checkSym(tree)

          tree match {
            case x: PackageDef    =>
              if ((sym.ownerChain contains currentOwner) || currentOwner == definitions.EmptyPackageClass) ()
              else fail(sym + " owner chain does not contain currentOwner " + currentOwner + sym.ownerChain)
            case _ =>
              def cond(s: Symbol) = !s.isTerm || s.isMethod || s == sym.owner

              if (sym.owner != currentOwner) {
                val expected = currentOwner.ownerChain find (x => cond(x)) getOrElse fail("DefTree can't find owner: ")
                if (sym.owner != expected)
                  fail("""|
                          | currentOwner chain: %s
                          |       symbol chain: %s""".stripMargin.format(
                            currentOwner.ownerChain take 3 mkString " -> ",
                            sym.ownerChain mkString " -> ")
                      )
              }
          }
        }
        super.traverse(tree)
      }
    }

    object postcheck extends Traverser {
      override def traverse(tree: Tree) {
        tree match {
          case EmptyTree | TypeTree() => ()
          case _ =>
            tpeOfTree get tree foreach { oldtpe =>
              if (oldtpe =:= tree.tpe) ()
              else typesDiffer(tree, oldtpe, tree.tpe)

              tree.tpe = oldtpe
              super.traverse(tree)
            }
        }
      }
    }
  }
}
