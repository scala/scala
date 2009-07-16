package scala.tools.nsc.interactive

import java.io.{ PrintWriter, StringWriter }

import scala.collection.mutable.{LinkedHashMap, SynchronizedMap}
import scala.concurrent.SyncVar
import scala.util.control.ControlException
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, RangePosition, OffsetPosition, NoPosition, WorkScheduler}
import scala.tools.nsc.reporters._
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._

/** The main class of the presentation compiler in an interactive environment such as an IDE
 */
class Global(settings: Settings, reporter: Reporter)
  extends nsc.Global(settings, reporter)
     with CompilerControl
     with RangePositions
     with ContextTrees
     with RichCompilationUnits {
self =>

  import definitions._

  override def onlyPresentation = true

  /** A list indicating in which order some units should be typechecked.
   *  All units in firsts are typechecked before any unit not in this list
   *  Modified by askToDoFirst, reload, typeAtTree.
   */
  var firsts: List[SourceFile] = List()

  /** A map of all loaded files to the rich compilation units that correspond to them.
   */
  val unitOfFile = new LinkedHashMap[AbstractFile, RichCompilationUnit] with
                       SynchronizedMap[AbstractFile, RichCompilationUnit]

  /** The currently active typer run */
  private var currentTyperRun: TyperRun = _

  /** Is a background compiler run needed? */
  private var outOfDate = false

  /** Is a reload/background compiler currently running? */
  private var acting = false

  /** The status value of a unit that has not yet been loaded */
  final val NotLoaded = -1

  /** The status value of a unit that has not yet been typechecked */
  final val JustParsed = 0

  // ----------- Overriding hooks in nsc.Global -----------------------

  /** Called from typechecker, which signal hereby that a node has been completely typechecked.
   *  If the node is included in unit.targetPos, abandons run and returns newly attributed tree.
   *  Otherwise, if there's some higher priority work to be done, also abandons run with a FreshRunReq.
   *  @param  context  The context that typechecked the node
   *  @param  old      The original node
   *  @param  result   The transformed node
   */
  override def signalDone(context: Context, old: Tree, result: Tree) {
    def integrateNew() {
      context.unit.body = new TreeReplacer(old, result) transform context.unit.body
    }
    if (activeLocks == 0) {
      if (context.unit != null &&
          !result.pos.isSynthetic &&
          !isTransparent(result.pos) &&
          (result.pos includes context.unit.targetPos)) {
        integrateNew()
        var located = new Locator(context.unit.targetPos) locateIn result
        if (located == EmptyTree) {
          println("something's wrong: no "+context.unit+" in "+result+result.pos)
          located = result
        }
        throw new TyperResult(located)
      }
      val typerRun = currentTyperRun

      while(true)
        try {
          pollForWork()
          if (typerRun == currentTyperRun)
            return

          integrateNew()
          throw new FreshRunReq
        } catch {
          case ex : ValidateError => // Ignore, this will have been reported elsewhere
          case t : Throwable => throw t
        }
    }
  }

  /** Called from typechecker every time a context is created.
   *  Registers the context in a context tree
   */
  override def registerContext(c: Context) = c.unit match {
    case u: RichCompilationUnit => addContext(u.contexts, c)
    case _ =>
  }

  // ----------------- Polling ---------------------------------------

  /** Called from runner thread and signalDone:
   *  Poll for exeptions.
   *  Poll for work reload/typedTreeAt/doFirst commands during background checking.
   */
  def pollForWork() {
    scheduler.pollException() match {
      case Some(ex: CancelActionReq) => if (acting) throw ex
      case Some(ex: FreshRunReq) => if (outOfDate) throw ex
      case Some(ex: Throwable) => throw ex
      case _ =>
    }
    scheduler.nextWorkItem() match {
      case Some(action) =>
        try {
          acting = true
          println("picked up work item: "+action)
          action()
          println("done with work item: "+action)
        } catch {
          case ex: CancelActionReq =>
            println("cancelled work item: "+action)
        } finally {
          println("quitting work item: "+action)
          acting = false
        }
      case None =>
    }
  }

  def debugInfo(source : SourceFile, start : Int, length : Int): String = {
    println("DEBUG INFO "+source+"/"+start+"/"+length)
    val end = start+length
    val pos = rangePos(source, start, start, end)

    val tree = locateTree(pos)
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    treePrinters.create(pw).print(tree)
    pw.flush

    val typed = new Response[Tree]
    askTypeAt(pos, typed)
    val typ = typed.get.left.toOption match {
      case Some(tree) =>
        val sw = new StringWriter
        val pw = new PrintWriter(sw)
        treePrinters.create(pw).print(tree)
        pw.flush
        sw.toString
      case None => "<None>"
    }

    val completionResponse = new Response[List[Member]]
    askCompletion(pos, completionResponse)
    val completion = completionResponse.get.left.toOption match {
      case Some(members) =>
        members mkString "\n"
      case None => "<None>"
    }

    source.content.view.drop(start).take(length).mkString+" : "+source.path+" ("+start+", "+end+
    ")\n\nlocateTree:\n"+sw.toString+"\n\naskTypeAt:\n"+typ+"\n\ncompletion:\n"+completion
  }

  // ----------------- The Background Runner Thread -----------------------

  /** The current presentation compiler runner */
  private var compileRunner = newRunnerThread

  /** Create a new presentation compiler runner.
   */
  def newRunnerThread: Thread = new Thread("Scala Presentation Compiler") {
    override def run() {
      try {
        while (true) {
          scheduler.waitForMoreWork()
          pollForWork()
          while (outOfDate) {
            try {
              backgroundCompile()
              outOfDate = false
            } catch {
              case ex: FreshRunReq =>
            }
          }
        }
      } catch {
        case ex: ShutdownReq =>
          ;
        case ex =>
          ex.printStackTrace()
          outOfDate = false
          inform("Fatal Error: "+ex)
          compileRunner = newRunnerThread
      }
    }
    start()
  }

  /** Compile all given units
   */
  private def backgroundCompile() {
    inform("Starting new presentation compiler type checking pass")
    reporter.reset
    firsts = firsts filter (s => unitOfFile contains (s.file))
    val prefix = firsts map unitOf
    val units = prefix ::: (unitOfFile.values.toList diff prefix)
    recompile(units)
    inform("Everything is now up to date")
  }

  /** Reset unit to just-parsed state */
  def reset(unit: RichCompilationUnit): Unit =
    if (unit.status > JustParsed) {
      unit.depends.clear()
      unit.defined.clear()
      unit.synthetics.clear()
      unit.toCheck.clear()
      unit.targetPos = NoPosition
      unit.contexts.clear()
      unit.body = EmptyTree
      unit.status = NotLoaded
    }

  /** Parse unit and create a name index. */
  def parse(unit: RichCompilationUnit): Unit = {
    currentTyperRun.compileLate(unit)
    validatePositions(unit.body)
    //println("parsed: [["+unit.body+"]]")
    unit.status = JustParsed
  }

  /** Make sure symbol and type attributes are reset and recompile units.
   */
  def recompile(units: List[RichCompilationUnit]) {
    for (unit <- units) {
      reset(unit)
      inform("parsing: "+unit)
      parse(unit)
    }
    for (unit <- units) {
      inform("type checking: "+unit)
      activeLocks = 0
      currentTyperRun.typeCheck(unit)
      unit.status = currentRunId
    }
  }

  /** Move list of files to front of firsts */
  def moveToFront(fs: List[SourceFile]) {
    firsts = fs ::: (firsts diff fs)
  }

  // ----------------- Implementations of client commmands -----------------------

  def respond[T](result: Response[T])(op: => T): Unit = try {
    result set Left(op)
  } catch {
    case ex =>
      result set Right(ex)
      throw ex
  }

  /** Make sure a set of compilation units is loaded and parsed */
  def reloadSources(sources: List[SourceFile]) {
    currentTyperRun = new TyperRun()
    for (source <- sources) {
      val unit = new RichCompilationUnit(source)
      unitOfFile(source.file) = unit
      parse(unit)
      if (settings.Xprintpos.value) treePrinter.print(unit)
    }
    moveToFront(sources)
  }

  /** Make sure a set of compilation units is loaded and parsed */
  def reload(sources: List[SourceFile], result: Response[Unit]) {
    respond(result)(reloadSources(sources))
    if (outOfDate) throw new FreshRunReq
    else outOfDate = true
  }

  /** A fully attributed tree located at position `pos`  */
  def typedTreeAt(pos: Position): Tree = {
    val unit = unitOf(pos)
    val sources = List(unit.source)
    if (unit.status == NotLoaded) reloadSources(sources)
    moveToFront(sources)
    val typedTree = currentTyperRun.typedTreeAt(pos)
    new Locator(pos) locateIn typedTree
  }

  /** Set sync var `result` to a fully attributed tree located at position `pos`  */
  def getTypedTreeAt(pos: Position, result: Response[Tree]) {
    respond(result)(typedTreeAt(pos))
  }

  def stabilizedType(tree: Tree): Type = tree match {
    case Ident(_) if tree.symbol.isStable => singleType(NoPrefix, tree.symbol)
    case Select(qual, _) if tree.symbol.isStable => singleType(qual.tpe, tree.symbol)
    case _ => tree.tpe
  }

  import analyzer.{SearchResult, ImplicitSearch}

  def completion(pos: Position, result: Response[List[Member]]) {
    respond(result) {
      val tree = typedTreeAt(pos)
      locateContext(pos) match {
        case Some(context) =>
          val superAccess = tree.isInstanceOf[Super]
          val pre = stabilizedType(tree)
          def member(sym: Symbol, inherited: Boolean) = new Member(
            sym,
            pre memberType sym,
            context.isAccessible(sym, pre, superAccess),
            inherited,
            NoSymbol
          )
          def implicitMembers(s: SearchResult): List[Member] = {
            val vtree = viewApply(s, tree, context)
            val vpre = stabilizedType(vtree)
            vtree.tpe.members map { sym => new Member(
              sym,
              vpre memberType sym,
              context.isAccessible(sym, vpre, false),
              false,
              s.tree.symbol
            )}
          }
          println("completion at "+tree+" "+tree.tpe)
          val decls = tree.tpe.decls.toList map (member(_, false))
          val inherited = tree.tpe.members.toList diff decls map (member(_, true))
          val implicits = applicableViews(tree, context) flatMap implicitMembers
          def isVisible(m: Member) =
            !(decls exists (_.shadows(m))) && !(inherited exists (_.shadows(m)))
          decls ::: inherited ::: (implicits filter isVisible)
        case None =>
          throw new FatalError("no context found for "+pos)
      }
    }
  }

  def applicableViews(tree: Tree, context: Context): List[SearchResult] =
    new ImplicitSearch(tree, functionType(List(tree.tpe), AnyClass.tpe), true, context.makeImplicit(false))
      .allImplicits

  def viewApply(view: SearchResult, tree: Tree, context: Context): Tree = {
    assert(view.tree != EmptyTree)
    try {
      analyzer.newTyper(context.makeImplicit(false)).typed(Apply(view.tree, List(tree)) setPos tree.pos)
    } catch {
      case ex: TypeError => EmptyTree
    }
  }

  // ---------------- Helper classes ---------------------------

  /** A transformer that replaces tree `from` with tree `to` in a given tree */
  class TreeReplacer(from: Tree, to: Tree) extends Transformer {
    override def transform(t: Tree): Tree = {
      if (t == from) to
      else if ((t.pos includes from.pos) || isTransparent(t.pos)) super.transform(t)
      else t
    }
  }

  /** A traverser that resets all type and symbol attributes in a tree
  object ResetAttrs extends Transformer {
    override def transform(t: Tree): Tree = {
      if (t.hasSymbol) t.symbol = NoSymbol
      t match {
        case EmptyTree =>
          t
        case tt: TypeTree =>
          if (tt.original != null) tt.original
          else t
        case _ =>
          t.tpe = null
          super.transform(t)
      }
    }
  }
  */

  /** The typer run */
  class TyperRun extends Run {
    // units is always empty
    // symSource, symData are ignored
    override def compiles(sym: Symbol) = false

    def typeCheck(unit: CompilationUnit): Unit = applyPhase(typerPhase, unit)

    def enterNames(unit: CompilationUnit): Unit = applyPhase(namerPhase, unit)

    /** Return fully attributed tree at given position
     *  (i.e. largest tree that's contained by position)
     */
    def typedTreeAt(pos: Position): Tree = {
      println("starting typedTreeAt")
      val tree = locateTree(pos)
      println("at pos "+pos+" was found: "+tree+tree.pos.show)
      if (tree.tpe ne null) {
        println("already attributed")
        tree
      } else {
        val unit = unitOf(pos)
        assert(unit.status >= JustParsed)
        unit.targetPos = pos
        try {
          println("starting type targetted check")
          typeCheck(unit)
          throw new FatalError("tree not found")
        } catch {
          case ex: TyperResult =>
            ex.tree
        } finally {
          unit.targetPos = NoPosition
        }
      }
    }

    /** Apply a phase to a compilation unit
     *  @return true iff typechecked correctly
     */
    private def applyPhase(phase: Phase, unit: CompilationUnit) {
      val oldSource = reporter.getSource
      try {
        reporter.setSource(unit.source)
        atPhase(phase) { phase.asInstanceOf[GlobalPhase] applyPhase unit }
      } finally {
        reporter setSource oldSource
      }
    }
  }

  class TyperResult(val tree: Tree) extends Exception with ControlException

  assert(globalPhase.id == 0)
}

