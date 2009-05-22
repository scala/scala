package scala.tools.nsc.interactive

import scala.collection.mutable.{LinkedHashSet, LinkedHashMap}
import scala.concurrent.SyncVar
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, RangePosition, OffsetPosition, NoPosition, WorkScheduler}
import scala.tools.nsc.reporters._
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._

class Global(_settings: Settings, _reporter: Reporter) extends nsc.Global(_settings, _reporter) {
self =>

  /** Make rangePos return a RangePosition */
  override def rangePos(source: SourceFile, start: Int, mid: Int, end: Int) = new RangePosition(source, start, mid, end)

  private var pollingEnabled = true

  /** Called from typechecker */
  override def pollForHighPriorityJob() {
    scheduler.nextWorkItem() match {
      case Some(action) =>
        pollingEnabled = false
        try {
          action()
        } catch {
          case ex: CancelActionReq =>
        }
        pollingEnabled = true
      case None =>
    }
  }

  /** A list indicating in which order some units should be typechecked.
   *  All units in priorityUnits are typechecked before any unit not in this list
   *  Modified by askToDoFirst
   */
  var priorityUnits: List[CompilationUnit] = List()

  /** The list of high-proiority units that still needs to be typechecked in current iteration */
  var remainingPriUnits: List[CompilationUnit] = List()

  /** Is there a loaded unit not up-to-date wrt typechecking?
   */
  var outOfDate: Boolean = false

  /** Has a loaded unit changed recently, or has a new unit been added?
   */
  var change: Boolean = false

  /** The units that still need compiling in the current pass */
  val unitsToCompile = new LinkedHashSet[CompilationUnit]

  /** A map of all loaded units to the id of the last compiler run that typechecked them
   *  @see NotLoaded, JustParsed
   */
  val unitsWithRunId = new LinkedHashMap[CompilationUnit, Int]

  /** The value associated in unitsWithRunId for units that have not yet been loaded */
  final val NotLoaded = -1

  /** The value associated in unitsWithRunId for units that have not yet been typechecked */
  final val JustParsed = 0

  /** The currently active typer run */
  var currentTyperRun: TyperRun = _

  /** The current presentation compiler runner */
  private var compileRunner = newRunnerThread
  compileRunner.start()

  /** Create a new presentation compiler runner */
  def newRunnerThread: Thread = new Thread("Scala Presentation Compiler") {
    override def run() {
      try {
        while (true) typeCheckPass()
      } catch {
        case ex: ShutdownReq =>
          ;
        case ex =>
          ex.printStackTrace()
          inform("Fatal Error: "+ex)
          compileRunner = newRunnerThread
          compileRunner.start()
      }
    }
  }

  /** A pass that first waits for something to do, then typechecks all loaded units
   *  until everything is up-to-date
   */
  private def typeCheckPass() = try {
    while (!outOfDate) {
      scheduler.waitForMoreWork()
      pollForHighPriorityJob()
    }
    inform("Starting new presentation compiler type checking pass")
    currentTyperRun = new TyperRun
    while (outOfDate) {
      outOfDate = false
      remainingPriUnits = priorityUnits
      for ((unit, id) <- unitsWithRunId.elements) {
        if (id != currentRunId) unitsToCompile += unit
      }
      while (unitsToCompile.nonEmpty) {
        if (change) {
          change = false
          outOfDate = true
          currentTyperRun = new TyperRun()
        }
        var unit = unitsToCompile.head
        if (!remainingPriUnits.isEmpty) {
          unit = remainingPriUnits.head
          remainingPriUnits = remainingPriUnits.tail
        }
        inform("type checking: "+unit)
        currentTyperRun.typeCheck(unit, NoPosition)
        unitsWithRunId(unit) = currentRunId
        unitsToCompile -= unit
      }
    }
  } catch {
    case ex: FreshRunReq => outOfDate = true
  }

  /** The compilation unit corresponding to a source file */
  def unitOf(s: SourceFile): CompilationUnit =
    unitsWithRunId.keys find (_.source == s) match {
      case Some(unit) => unit
      case None =>
        val unit = new CompilationUnit(s)
        unitsWithRunId(unit) = NotLoaded
        unit
    }

  /** The compilation unit corresponding to a position */
  def unitOf(pos: Position): CompilationUnit = unitOf(pos.source.get)

  /** Make sure a set of compilation units is loaded and parsed */
  def reload(units: Set[CompilationUnit]) = {
    for (unit <- units) {
      currentTyperRun.compileLate(unit)
      unitsWithRunId(unit) = JustParsed
      unitsToCompile += unit
    }
    change = true
    outOfDate = true
  }

  /** Set sync var `result` to a fully attributed tree located at position `pos`  */
  def typedTreeAt(pos: Position, result: SyncVar[Tree]) {
    val unit = unitOf(pos)
    if (unitsWithRunId(unit) == NotLoaded)
      reload(Set(unit))
    if (unitsWithRunId(unit) != currentRunId)
      currentTyperRun.typeCheck(unit, pos)
    result set locateTree(pos)
  }

  /** A locator for trees with given positions.
   *  Given a position `pos`, locator.apply returns
   *  the smallest tree that encloses `pos`.
   */
  object locate extends Traverser {
    var pos: Position = _
    var last: Tree = _
    def apply(pos: Position, root: Tree): Tree = {
      this.pos = pos
      this.last = EmptyTree
      traverse(root)
      this.last
    }
    override def traverse(t: Tree) {
      if (t.pos.start <= pos.start && pos.end <= t.pos.end) {
        last = t
        super.traverse(t)
      }
    }
  }

  /** Locate smallest tree that encloses position */
  def locateTree(pos: Position): Tree =
    locate(pos, unitOf(pos).body)

  // ----------------- interface to IDE ------------------------------------

  private val scheduler = new WorkScheduler

  /** Make sure a set of compilation units is loaded and parsed */
  def askReload(units: Set[CompilationUnit]) =
    scheduler.postWorkItem(() => reload(units))

  /** Set sync var `result` to a fully attributed tree located at position `pos`  */
  def askTypeAt(pos: Position, result: SyncVar[Tree]) =
    scheduler.postWorkItem(() => self.typedTreeAt(pos, result))

  /** Ask to do unit first on subsequent type checking passes */
  def askToDoFirst(unit: CompilationUnit) = {
    def moveToFront(unit: CompilationUnit, units: List[CompilationUnit]) = unit :: (units filter (unit !=))
    scheduler.postWorkItem { () =>
      remainingPriUnits = moveToFront(unit, remainingPriUnits)
      priorityUnits = moveToFront(unit, priorityUnits)
    }
  }

  /** Cancel current high-priority job */
  def askCancel() =
    scheduler.raise(new CancelActionReq)

  /** Cancel current compiler run and start a fresh one where everything will be re-typechecked
   *  (but not re-loaded).
   */
  def askReset() =
    scheduler.raise(new FreshRunReq)

  /** Tell the compile server to shutdown, and do not restart again */
  def askShutdown() =
    scheduler.raise(new ShutdownReq)

  class CancelActionReq extends Exception
  class FreshRunReq extends Exception
  class ShutdownReq extends Exception

  /** The typer run */
  class TyperRun extends Run {
    // units is always empty
    // symSource, symData are ignored
    override def compiles(sym: Symbol) = false

    def typeCheck(unit: CompilationUnit, pos: Position) {
      assert(unitsWithRunId(unit) >= JustParsed)
/*
      unit.needOnly =
        if (pos == NoPosition) PackageDef(nme.EMPTY, List()) setPos(
        else

*/
      typerPhase.applyPhase(unit)
    }
  }

  assert(globalPhase.id == 0)
}
