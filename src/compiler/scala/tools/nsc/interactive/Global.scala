package scala.tools.nsc.interactive

import scala.collection.mutable.{LinkedHashMap, SynchronizedMap}
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
     with Positions
     with ContextTrees
     with RichCompilationUnits {
self =>

  /** A list indicating in which order some units should be typechecked.
   *  All units in firsts are typechecked before any unit not in this list
   *  Modified by askToDoFirst, reload, typeAtTree.
   */
  var firsts: List[SourceFile] = List()

  /** A map of all loaded files units to the rich compilation units that corresponds to them.
   */
  val unitOfFile = new LinkedHashMap[AbstractFile, RichCompilationUnit] with
                       SynchronizedMap[AbstractFile, RichCompilationUnit]

  /** The currently active typer run */
  private var currentTyperRun: TyperRun = _

  /** Is a background compiler run needed? */
  private var outOfDate = false

  /** Is a reload/ background compiler currently running? */
  private var acting = false

  /** The status value of a unit that has not yet been loaded */
  final val NotLoaded = -1

  /** The status value of a unit that has not yet been typechecked */
  final val JustParsed = 0

  // ----------- Overriding hooks in nsc.Global -----------------------

  /** Create a RangePosition */
  override def rangePos(source: SourceFile, start: Int, point: Int, end: Int) =
    new RangePosition(source, start, point, end)

  /** Called from typechecker: signal that a node has been completely typechecked
   *  @param  context  The context that typechecked the node
   *  @param  old      The original node
   *  @param  result   The transformed node
   */
  override def signalDone(context: Context, old: Tree, result: Tree) {
    def integrateNew() {
      context.unit.body = new TreeReplacer(old, result) transform context.unit.body
    }
    if ((context.unit != null) && (context.unit.targetPos includes result.pos)) {
      integrateNew()
      throw new TyperResult(result)
    }
    val typerRun = currentTyperRun
    pollForWork()
    if (typerRun != currentTyperRun) {
      integrateNew()
      throw new FreshRunReq
    }
  }

  /** Called every time a context is created
   *  Register the context in a context tree
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
          action()
        } catch {
          case ex: CancelActionReq =>
        } finally {
          acting = false
        }
      case None =>
    }
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
            } catch {
              case ex: FreshRunReq =>
            } finally {
              outOfDate = false
            }
          }
        }
      } catch {
        case ex: ShutdownReq =>
          ;
        case ex =>
          ex.printStackTrace()
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
    units foreach recompile
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
    unit.status = JustParsed
  }

  /** Make sure symbol and type attributes are reset and recompile unit.
   */
  def recompile(unit: RichCompilationUnit) {
    reset(unit)
    inform("parsing: "+unit)
    parse(unit)
    inform("type checking: "+unit)
    currentTyperRun.typeCheck(unit)
    unit.status = currentRunId
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
  def reload(sources: List[SourceFile], result: Response[Unit]) {
    respond(result) {
      currentTyperRun = new TyperRun()
      for (source <- sources) {
        val unit = new RichCompilationUnit(source)
        unitOfFile(source.file) = unit
        parse(unit)
        if (settings.Xprintpos.value) treePrinter.print(unit)
      }
      moveToFront(sources)
      ()
    }
    if (outOfDate) throw new FreshRunReq
    else outOfDate = true
  }

  /** A fully attributed tree located at position `pos`  */
  def typedTreeAt(pos: Position): Tree = {
    val unit = unitOf(pos)
    assert(unit.status != NotLoaded)
    moveToFront(List(unit.source))
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

  def completion(pos: Position, result: Response[List[Member]]) {
    import MemberStatus._
    respond(result) {
      val tree = typedTreeAt(pos)
      locateContext(pos) match {
        case Some(context) =>
          val superAccess = tree.isInstanceOf[Super]
          val pre = stabilizedType(tree)
          def withStatus(sym: Symbol, vs: ValueSet) = (
            sym,
            pre.memberType(sym),
            if (context.isAccessible(sym, pre, superAccess)) vs + Accessible else vs
          )
          val decls = tree.tpe.decls.toList map (sym => withStatus(sym, ValueSet()))
          val inherited = tree.tpe.members.toList diff decls map (sym => withStatus(sym, ValueSet(Inherited)))
          val implicits = List() // not yet done
          decls ::: inherited ::: implicits
        case None =>
          throw new FatalError("no context found for "+pos)
      }
    }
  }

  // ---------------- Helper classes ---------------------------

  /** A transformer that replaces tree `from` with tree `to` in a given tree */
  class TreeReplacer(from: Tree, to: Tree) extends Transformer {
    override def transform(t: Tree): Tree = {
      if (t.pos includes from.pos)
        if (t == from) to
        else super.transform(t)
      else
        t
    }
  }

  /** A traverser that resets all type and symbol attributes in a tree */
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
      val tree = locateTree(pos)
//      println("at pos "+pos+" was found: "+tree)
      if (tree.tpe ne null) tree
      else {
        val unit = unitOf(pos)
        assert(unit.status >= JustParsed)
        unit.targetPos = pos
        try {
          typeCheck(unit)
          throw new FatalError("tree not found")
        } catch {
          case ex: TyperResult => ex.tree
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

  class TyperResult(val tree: Tree) extends Exception

  assert(globalPhase.id == 0)

}

