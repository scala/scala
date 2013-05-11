/* NSC -- new Scala compiler
 * Copyright 2009-2013 Typesafe/Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package interactive

import java.io.{ PrintWriter, StringWriter, FileReader, FileWriter }
import scala.collection.mutable
import mutable.{LinkedHashMap, SynchronizedMap, HashSet, SynchronizedSet}
import scala.concurrent.SyncVar
import scala.util.control.ControlThrowable
import scala.tools.nsc.io.{ AbstractFile, LogReplay, Logger, NullLogger, Replayer }
import scala.tools.nsc.util.{ WorkScheduler, MultiHashMap }
import scala.reflect.internal.util.{ SourceFile, BatchSourceFile, Position, RangePosition, NoPosition }
import scala.tools.nsc.reporters._
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._
import scala.tools.nsc.io.Pickler._
import scala.tools.nsc.typechecker.DivergentImplicit
import scala.annotation.tailrec
import symtab.Flags.{ACCESSOR, PARAMACCESSOR}
import scala.annotation.elidable
import scala.language.implicitConversions

/** The main class of the presentation compiler in an interactive environment such as an IDE
 */
class Global(settings: Settings, _reporter: Reporter, projectName: String = "")  extends {
  /* Is the compiler initializing? Early def, so that the field is true during the
   *  execution of the super constructor.
   */
  private var initializing = true
} with scala.tools.nsc.Global(settings, _reporter)
  with CompilerControl
  with RangePositions
  with ContextTrees
  with RichCompilationUnits
  with ScratchPadMaker
  with Picklers {

  import definitions._

  val debugIDE: Boolean = settings.YpresentationDebug.value
  val verboseIDE: Boolean = settings.YpresentationVerbose.value

  private def replayName = settings.YpresentationReplay.value
  private def logName = settings.YpresentationLog.value
  private def afterTypeDelay = settings.YpresentationDelay.value
  private final val SleepTime = 10

  val log =
    if (replayName != "") new Replayer(new FileReader(replayName))
    else if (logName != "") new Logger(new FileWriter(logName))
    else NullLogger

  import log.logreplay
  debugLog("logger: " + log.getClass + " writing to " + (new java.io.File(logName)).getAbsolutePath)
  debugLog("classpath: "+classPath)

  private var curTime = System.nanoTime
  private def timeStep = {
    val last = curTime
    curTime = System.nanoTime
    ", delay = " + (curTime - last) / 1000000 + "ms"
  }

  /** Print msg only when debugIDE is true. */
  @inline final def debugLog(msg: => String) =
    if (debugIDE) println("[%s] %s".format(projectName, msg))

  /** Inform with msg only when verboseIDE is true. */
  @inline final def informIDE(msg: => String) =
    if (verboseIDE) println("[%s][%s]".format(projectName, msg))

  override def forInteractive = true

  /** A map of all loaded files to the rich compilation units that correspond to them.
   */
  val unitOfFile = new LinkedHashMap[AbstractFile, RichCompilationUnit] with
                       SynchronizedMap[AbstractFile, RichCompilationUnit] {
    override def put(key: AbstractFile, value: RichCompilationUnit) = {
      val r = super.put(key, value)
      if (r.isEmpty) debugLog("added unit for "+key)
      r
    }
    override def remove(key: AbstractFile) = {
      val r = super.remove(key)
      if (r.nonEmpty) debugLog("removed unit for "+key)
      r
    }
  }

  /** A set containing all those files that need to be removed
   *  Units are removed by getUnit, typically once a unit is finished compiled.
   */
  protected val toBeRemoved: mutable.Set[AbstractFile] =
    new HashSet[AbstractFile] with SynchronizedSet[AbstractFile]

  /** A set containing all those files that need to be removed after a full background compiler run
   */
  protected val toBeRemovedAfterRun: mutable.Set[AbstractFile] =
    new HashSet[AbstractFile] with SynchronizedSet[AbstractFile]

  class ResponseMap extends MultiHashMap[SourceFile, Response[Tree]] {
    override def += (binding: (SourceFile, Set[Response[Tree]])) = {
      assert(interruptsEnabled, "delayed operation within an ask")
      super.+=(binding)
    }
  }

  /** A map that associates with each abstract file the set of responses that are waiting
   *  (via waitLoadedTyped) for the unit associated with the abstract file to be loaded and completely typechecked.
   */
  protected val waitLoadedTypeResponses = new ResponseMap

  /** A map that associates with each abstract file the set of responses that ware waiting
   *  (via build) for the unit associated with the abstract file to be parsed and entered
   */
  protected var getParsedEnteredResponses = new ResponseMap

  private def cleanResponses(rmap: ResponseMap): Unit = {
    for ((source, rs) <- rmap.toList) {
      for (r <- rs) {
        if (getUnit(source).isEmpty)
          r raise new NoSuchUnitError(source.file)
        if (r.isComplete)
          rmap(source) -= r
      }
      if (rmap(source).isEmpty)
        rmap -= source
    }
  }

  private def cleanAllResponses() {
    cleanResponses(waitLoadedTypeResponses)
    cleanResponses(getParsedEnteredResponses)
  }

  private def checkNoOutstanding(rmap: ResponseMap): Unit =
    for ((_, rs) <- rmap.toList; r <- rs) {
      debugLog("ERROR: missing response, request will be discarded")
      r raise new MissingResponse
    }

  def checkNoResponsesOutstanding() {
    checkNoOutstanding(waitLoadedTypeResponses)
    checkNoOutstanding(getParsedEnteredResponses)
  }

  /** The compilation unit corresponding to a source file
   *  if it does not yet exist create a new one atomically
   *  Note: We want to remove this.
   */
  protected[interactive] def getOrCreateUnitOf(source: SourceFile): RichCompilationUnit =
    unitOfFile.getOrElse(source.file, { println("precondition violated: "+source+" is not loaded"); new Exception().printStackTrace(); new RichCompilationUnit(source) })

  /** Work through toBeRemoved list to remove any units.
   *  Then return optionally unit associated with given source.
   */
  protected[interactive] def getUnit(s: SourceFile): Option[RichCompilationUnit] = {
    toBeRemoved.synchronized {
      for (f <- toBeRemoved) {
        informIDE("removed: "+s)
        unitOfFile -= f
        allSources = allSources filter (_.file != f)
      }
      toBeRemoved.clear()
    }
    unitOfFile get s.file
  }

  /** A list giving all files to be typechecked in the order they should be checked.
   */
  protected var allSources: List[SourceFile] = List()

  private var lastException: Option[Throwable] = None

  /** A list of files that crashed the compiler. They will be ignored during background
   *  compilation until they are removed from this list.
   */
  private var ignoredFiles: Set[AbstractFile] = Set()

  /** Flush the buffer of sources that are ignored during background compilation. */
  def clearIgnoredFiles() {
    ignoredFiles = Set()
  }

  /** Remove a crashed file from the ignore buffer. Background compilation will take it into account
   *  and errors will be reported against it. */
  def enableIgnoredFile(file: AbstractFile) {
    ignoredFiles -= file
    debugLog("Removed crashed file %s. Still in the ignored buffer: %s".format(file, ignoredFiles))
  }

  /** The currently active typer run */
  private var currentTyperRun: TyperRun = _
  newTyperRun()

  /** Is a background compiler run needed?
   *  Note: outOfDate is true as long as there is a background compile scheduled or going on.
   */
  private var outOfDate = false

  def isOutOfDate: Boolean = outOfDate

  def demandNewCompilerRun() = {
    if (outOfDate) throw new FreshRunReq // cancel background compile
    else outOfDate = true            // proceed normally and enable new background compile
  }

  protected[interactive] var minRunId = 1

  private[interactive] var interruptsEnabled = true

  private val NoResponse: Response[_] = new Response[Any]

  /** The response that is currently pending, i.e. the compiler
   *  is working on providing an asnwer for it.
   */
  private var pendingResponse: Response[_] = NoResponse

  // ----------- Overriding hooks in nsc.Global -----------------------

  /** Called from parser, which signals hereby that a method definition has been parsed.
   */
  override def signalParseProgress(pos: Position) {
    // We only want to be interruptible when running on the PC thread.
    if(onCompilerThread) {
      checkForMoreWork(pos)
    }
  }

  /** Called from typechecker, which signals hereby that a node has been completely typechecked.
   *  If the node includes unit.targetPos, abandons run and returns newly attributed tree.
   *  Otherwise, if there's some higher priority work to be done, also abandons run with a FreshRunReq.
   *  @param  context  The context that typechecked the node
   *  @param  old      The original node
   *  @param  result   The transformed node
   */
  override def signalDone(context: Context, old: Tree, result: Tree) {
    if (interruptsEnabled && analyzer.lockedCount == 0) {
      if (context.unit.exists &&
          result.pos.isOpaqueRange &&
          (result.pos includes context.unit.targetPos)) {
        var located = new TypedLocator(context.unit.targetPos) locateIn result
        if (located == EmptyTree) {
          println("something's wrong: no "+context.unit+" in "+result+result.pos)
          located = result
        }
        throw new TyperResult(located)
      }
      try {
        checkForMoreWork(old.pos)
      } catch {
        case ex: ValidateException => // Ignore, this will have been reported elsewhere
          debugLog("validate exception caught: "+ex)
        case ex: Throwable =>
          log.flush()
          throw ex
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

  /** The top level classes and objects currently seen in the presentation compiler
   */
  private val currentTopLevelSyms = new mutable.LinkedHashSet[Symbol]

  /** The top level classes and objects no longer seen in the presentation compiler
   */
  val deletedTopLevelSyms = new mutable.LinkedHashSet[Symbol] with mutable.SynchronizedSet[Symbol]

  /** Called from typechecker every time a top-level class or object is entered.
   */
  override def registerTopLevelSym(sym: Symbol) { currentTopLevelSyms += sym }

  /** Symbol loaders in the IDE parse all source files loaded from a package for
   *  top-level idents. Therefore, we can detect top-level symbols that have a name
   *  different from their source file
   */
  override lazy val loaders = new BrowsingLoaders {
    val global: Global.this.type = Global.this
  }

  // ----------------- Polling ---------------------------------------

  case class WorkEvent(atNode: Int, atMillis: Long)

  private var moreWorkAtNode: Int = -1
  private var nodesSeen = 0
  private var lastWasReload = false

  /** The number of pollForWorks after which the presentation compiler yields.
   *  Yielding improves responsiveness on systems with few cores because it
   *  gives the UI thread a chance to get new tasks and interrupt the presentation
   *  compiler with them.
   */
  private final val yieldPeriod = 10

  /** Called from runner thread and signalDone:
   *  Poll for interrupts and execute them immediately.
   *  Then, poll for exceptions and execute them.
   *  Then, poll for work reload/typedTreeAt/doFirst commands during background checking.
   *  @param pos   The position of the tree if polling while typechecking, NoPosition otherwise
   *
   */
  private[interactive] def pollForWork(pos: Position) {
    if (!interruptsEnabled) return
    if (pos == NoPosition || nodesSeen % yieldPeriod == 0)
      Thread.`yield`()

    def nodeWithWork(): Option[WorkEvent] =
      if (scheduler.moreWork || pendingResponse.isCancelled) Some(new WorkEvent(nodesSeen, System.currentTimeMillis))
      else None

    nodesSeen += 1
    logreplay("atnode", nodeWithWork()) match {
      case Some(WorkEvent(id, _)) =>
        debugLog("some work at node "+id+" current = "+nodesSeen)
//        assert(id >= nodesSeen)
        moreWorkAtNode = id
      case None =>
    }

    if (nodesSeen >= moreWorkAtNode) {

      logreplay("asked", scheduler.pollInterrupt()) match {
        case Some(ir) =>
          try {
            interruptsEnabled = false
            debugLog("ask started"+timeStep)
            ir.execute()
          } finally {
            debugLog("ask finished"+timeStep)
            interruptsEnabled = true
          }
          pollForWork(pos)
        case _ =>
      }

      if (logreplay("cancelled", pendingResponse.isCancelled)) {
        throw CancelException
      }

      logreplay("exception thrown", scheduler.pollThrowable()) match {
        case Some(ex: FreshRunReq) =>
          newTyperRun()
          minRunId = currentRunId
          demandNewCompilerRun()

        case Some(ShutdownReq) =>
          scheduler.synchronized { // lock the work queue so no more items are posted while we clean it up
            val units = scheduler.dequeueAll {
              case item: WorkItem => Some(item.raiseMissing())
              case _ => Some(())
            }

            // don't forget to service interrupt requests
            val iqs = scheduler.dequeueAllInterrupts(_.execute())

            debugLog("ShutdownReq: cleaning work queue (%d items)".format(units.size))
            debugLog("Cleanup up responses (%d loadedType pending, %d parsedEntered pending)"
                .format(waitLoadedTypeResponses.size, getParsedEnteredResponses.size))
            checkNoResponsesOutstanding()

            log.flush();
            scheduler = new NoWorkScheduler
            throw ShutdownReq
          }

        case Some(ex: Throwable) => log.flush(); throw ex
        case _ =>
      }

      lastWasReload = false

      logreplay("workitem", scheduler.nextWorkItem()) match {
        case Some(action) =>
          try {
            debugLog("picked up work item at "+pos+": "+action+timeStep)
            action()
            debugLog("done with work item: "+action)
          } finally {
            debugLog("quitting work item: "+action+timeStep)
          }
        case None =>
      }
    }
  }

  protected def checkForMoreWork(pos: Position) {
    val typerRun = currentTyperRun
    pollForWork(pos)
    if (typerRun != currentTyperRun) demandNewCompilerRun()
  }

  def debugInfo(source : SourceFile, start : Int, length : Int): String = {
    println("DEBUG INFO "+source+"/"+start+"/"+length)
    val end = start+length
    val pos = rangePos(source, start, start, end)

    val tree = locateTree(pos)
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    newTreePrinter(pw).print(tree)
    pw.flush

    val typed = new Response[Tree]
    askTypeAt(pos, typed)
    val typ = typed.get.left.toOption match {
      case Some(tree) =>
        val sw = new StringWriter
        val pw = new PrintWriter(sw)
        newTreePrinter(pw).print(tree)
        pw.flush
        sw.toString
      case None => "<None>"
    }

    val completionResponse = new Response[List[Member]]
    askTypeCompletion(pos, completionResponse)
    val completion = completionResponse.get.left.toOption match {
      case Some(members) =>
        members mkString "\n"
      case None => "<None>"
    }

    source.content.view.drop(start).take(length).mkString+" : "+source.path+" ("+start+", "+end+
    ")\n\nlocateTree:\n"+sw.toString+"\n\naskTypeAt:\n"+typ+"\n\ncompletion:\n"+completion
  }

  // ----------------- The Background Runner Thread -----------------------

  private var threadId = 0

  /** The current presentation compiler runner */
  @volatile private[interactive] var compileRunner: Thread = newRunnerThread()

  /** Check that the currenyly executing thread is the presentation compiler thread.
   *
   *  Compiler initialization may happen on a different thread (signalled by globalPhase being NoPhase)
   */
  @elidable(elidable.WARNING)
  override def assertCorrectThread() {
    assert(initializing || onCompilerThread,
        "Race condition detected: You are running a presentation compiler method outside the PC thread.[phase: %s]".format(globalPhase) +
        " Please file a ticket with the current stack trace at https://www.assembla.com/spaces/scala-ide/support/tickets")
  }

  /** Create a new presentation compiler runner.
   */
  private def newRunnerThread(): Thread = {
    threadId += 1
    compileRunner = new PresentationCompilerThread(this, projectName)
    compileRunner.setDaemon(true)
    compileRunner.start()
    compileRunner
  }

  private def ensureUpToDate(unit: RichCompilationUnit) =
    if (!unit.isUpToDate && unit.status != JustParsed) reset(unit) // reparse previously typechecked units.

  /** Compile all loaded source files in the order given by `allSources`.
   */
  private[interactive] final def backgroundCompile() {
    informIDE("Starting new presentation compiler type checking pass")
    reporter.reset()

    // remove any files in first that are no longer maintained by presentation compiler (i.e. closed)
    allSources = allSources filter (s => unitOfFile contains (s.file))

    // ensure all loaded units are parsed
    for (s <- allSources; unit <- getUnit(s)) {
      // checkForMoreWork(NoPosition)  // disabled, as any work done here would be in an inconsistent state
      ensureUpToDate(unit)
      parseAndEnter(unit)
      serviceParsedEntered()
    }

    // sleep window
    if (afterTypeDelay > 0 && lastWasReload) {
      val limit = System.currentTimeMillis() + afterTypeDelay
      while (System.currentTimeMillis() < limit) {
        Thread.sleep(SleepTime)
        checkForMoreWork(NoPosition)
      }
    }

    // ensure all loaded units are typechecked
    for (s <- allSources; if !ignoredFiles(s.file); unit <- getUnit(s)) {
      try {
        if (!unit.isUpToDate)
          if (unit.problems.isEmpty || !settings.YpresentationStrict.value)
            typeCheck(unit)
          else debugLog("%s has syntax errors. Skipped typechecking".format(unit))
        else debugLog("already up to date: "+unit)
        for (r <- waitLoadedTypeResponses(unit.source))
          r set unit.body
        serviceParsedEntered()
      } catch {
        case ex: FreshRunReq => throw ex           // propagate a new run request
        case ShutdownReq     => throw ShutdownReq  // propagate a shutdown request
        case ex: ControlThrowable => throw ex
        case ex: Throwable =>
          println("[%s]: exception during background compile: ".format(unit.source) + ex)
          ex.printStackTrace()
          for (r <- waitLoadedTypeResponses(unit.source)) {
            r.raise(ex)
          }
          serviceParsedEntered()

          lastException = Some(ex)
          ignoredFiles += unit.source.file
          println("[%s] marking unit as crashed (crashedFiles: %s)".format(unit, ignoredFiles))

          reporter.error(unit.body.pos, "Presentation compiler crashed while type checking this file: %s".format(ex.toString()))
      }
    }

    // move units removable after this run to the "to-be-removed" buffer
    toBeRemoved ++= toBeRemovedAfterRun

    // clean out stale waiting responses
    cleanAllResponses()

    // wind down
    if (waitLoadedTypeResponses.nonEmpty || getParsedEnteredResponses.nonEmpty) {
      // need another cycle to treat those
      newTyperRun()
      backgroundCompile()
    } else {
      outOfDate = false
      informIDE("Everything is now up to date")
    }
  }

  /** Service all pending getParsedEntered requests
   */
  private def serviceParsedEntered() {
    var atOldRun = true
    for ((source, rs) <- getParsedEnteredResponses; r <- rs) {
      if (atOldRun) { newTyperRun(); atOldRun = false }
      getParsedEnteredNow(source, r)
    }
    getParsedEnteredResponses.clear()
  }

  /** Reset unit to unloaded state */
  private def reset(unit: RichCompilationUnit): Unit = {
    unit.depends.clear()
    unit.defined.clear()
    unit.synthetics.clear()
    unit.toCheck.clear()
    unit.checkedFeatures = Set()
    unit.targetPos = NoPosition
    unit.contexts.clear()
    unit.problems.clear()
    unit.body = EmptyTree
    unit.status = NotLoaded
  }

  /** Parse unit and create a name index, unless this has already been done before */
  private def parseAndEnter(unit: RichCompilationUnit): Unit =
    if (unit.status == NotLoaded) {
      debugLog("parsing: "+unit)
      currentTyperRun.compileLate(unit)
      if (debugIDE && !reporter.hasErrors) validatePositions(unit.body)
      if (!unit.isJava) syncTopLevelSyms(unit)
      unit.status = JustParsed
    }

  /** Make sure unit is typechecked
   */
  private def typeCheck(unit: RichCompilationUnit) {
    debugLog("type checking: "+unit)
    parseAndEnter(unit)
    unit.status = PartiallyChecked
    currentTyperRun.typeCheck(unit)
    unit.lastBody = unit.body
    unit.status = currentRunId
  }

  /** Update deleted and current top-level symbols sets */
  def syncTopLevelSyms(unit: RichCompilationUnit) {
    val deleted = currentTopLevelSyms filter { sym =>
      /** We sync after namer phase and it resets all the top-level symbols
       *  that survive the new parsing
       *  round to NoPeriod.
       */
      sym.sourceFile == unit.source.file &&
      sym.validTo != NoPeriod &&
      runId(sym.validTo) < currentRunId
    }
    for (d <- deleted) {
      d.owner.info.decls unlink d
      deletedTopLevelSyms += d
      currentTopLevelSyms -= d
    }
  }

  /** Move list of files to front of allSources */
  def moveToFront(fs: List[SourceFile]) {
    allSources = fs ::: (allSources diff fs)
  }

  // ----------------- Implementations of client commands -----------------------

  def respond[T](result: Response[T])(op: => T): Unit =
    respondGradually(result)(Stream(op))

  def respondGradually[T](response: Response[T])(op: => Stream[T]): Unit = {
    val prevResponse = pendingResponse
    try {
      pendingResponse = response
      if (!response.isCancelled) {
        var results = op
        while (!response.isCancelled && results.nonEmpty) {
          val result = results.head
          results = results.tail
          if (results.isEmpty) {
            response set result
            debugLog("responded"+timeStep)
          } else response setProvisionally result
        }
      }
    } catch {
      case CancelException =>
        debugLog("cancelled")
      case ex: FreshRunReq =>
        if (debugIDE) {
          println("FreshRunReq thrown during response")
          ex.printStackTrace()
        }
        response raise ex
        throw ex

      case ex @ ShutdownReq =>
        if (debugIDE) {
          println("ShutdownReq thrown during response")
          ex.printStackTrace()
        }
        response raise ex
        throw ex

      case ex: Throwable =>
        if (debugIDE) {
          println("exception thrown during response: "+ex)
          ex.printStackTrace()
        }
        response raise ex
    } finally {
      pendingResponse = prevResponse
    }
  }

  private def reloadSource(source: SourceFile) {
    val unit = new RichCompilationUnit(source)
    unitOfFile(source.file) = unit
    toBeRemoved -= source.file
    toBeRemovedAfterRun -= source.file
    reset(unit)
    //parseAndEnter(unit)
  }

  /** Make sure a set of compilation units is loaded and parsed */
  private def reloadSources(sources: List[SourceFile]) {
    newTyperRun()
    minRunId = currentRunId
    sources foreach reloadSource
    moveToFront(sources)
  }

  /** Make sure a set of compilation units is loaded and parsed */
  private[interactive] def reload(sources: List[SourceFile], response: Response[Unit]) {
    informIDE("reload: " + sources)
    lastWasReload = true
    respond(response)(reloadSources(sources))
    demandNewCompilerRun()
  }

  private[interactive] def filesDeleted(sources: List[SourceFile], response: Response[Unit]) {
    informIDE("files deleted: " + sources)
    val deletedFiles = sources.map(_.file).toSet
    val deletedSyms = currentTopLevelSyms filter {sym => deletedFiles contains sym.sourceFile}
    for (d <- deletedSyms) {
      d.owner.info.decls unlink d
      deletedTopLevelSyms += d
      currentTopLevelSyms -= d
    }
    sources foreach (removeUnitOf(_))
    minRunId = currentRunId
    respond(response)(())
    demandNewCompilerRun()
  }

  /** Arrange for unit to be removed after run, to give a chance to typecheck the unit fully.
   *  If we do just removeUnit, some problems with default parameters can ensue.
   *  Calls to this method could probably be replaced by removeUnit once default parameters are handled more robustly.
   */
  private def afterRunRemoveUnitsOf(sources: List[SourceFile]) {
    toBeRemovedAfterRun ++= sources map (_.file)
  }

  /** A fully attributed tree located at position `pos` */
  private def typedTreeAt(pos: Position): Tree = getUnit(pos.source) match {
    case None =>
      reloadSources(List(pos.source))
      try typedTreeAt(pos)
      finally afterRunRemoveUnitsOf(List(pos.source))
    case Some(unit) =>
      informIDE("typedTreeAt " + pos)
      parseAndEnter(unit)
      val tree = locateTree(pos)
      debugLog("at pos "+pos+" was found: "+tree.getClass+" "+tree.pos.show)
      tree match {
        case Import(expr, _) =>
          debugLog("import found"+expr.tpe+(if (expr.tpe == null) "" else " "+expr.tpe.members))
        case _ =>
      }
      if (stabilizedType(tree) ne null) {
        debugLog("already attributed: "+tree.symbol+" "+tree.tpe)
        tree
      } else {
        unit.targetPos = pos
        try {
          debugLog("starting targeted type check")
          typeCheck(unit)
//          println("tree not found at "+pos)
          EmptyTree
        } catch {
          case ex: TyperResult => new Locator(pos) locateIn ex.tree
        } finally {
          unit.targetPos = NoPosition
        }
      }
  }

  /** A fully attributed tree corresponding to the entire compilation unit  */
  private[interactive] def typedTree(source: SourceFile, forceReload: Boolean): Tree = {
    informIDE("typedTree " + source + " forceReload: " + forceReload)
    val unit = getOrCreateUnitOf(source)
    if (forceReload) reset(unit)
    parseAndEnter(unit)
    if (unit.status <= PartiallyChecked) typeCheck(unit)
    unit.body
  }

  /** Set sync var `response` to a fully attributed tree located at position `pos`  */
  private[interactive] def getTypedTreeAt(pos: Position, response: Response[Tree]) {
    respond(response)(typedTreeAt(pos))
  }

  /** Set sync var `response` to a fully attributed tree corresponding to the
   *  entire compilation unit  */
  private[interactive] def getTypedTree(source: SourceFile, forceReload: Boolean, response: Response[Tree]) {
    respond(response)(typedTree(source, forceReload))
  }

  private def withTempUnits[T](sources: List[SourceFile])(f: (SourceFile => RichCompilationUnit) => T): T = {
    val unitOfSrc: SourceFile => RichCompilationUnit = src => unitOfFile(src.file)
    sources filterNot (getUnit(_).isDefined) match {
      case Nil =>
        f(unitOfSrc)
      case unknown =>
        reloadSources(unknown)
        try {
          f(unitOfSrc)
        } finally
          afterRunRemoveUnitsOf(unknown)
    }
  }

  private def withTempUnit[T](source: SourceFile)(f: RichCompilationUnit => T): T =
    withTempUnits(List(source)){ srcToUnit =>
      f(srcToUnit(source))
    }

  /** Find a 'mirror' of symbol `sym` in unit `unit`. Pre: `unit is loaded. */
  private def findMirrorSymbol(sym: Symbol, unit: RichCompilationUnit): Symbol = {
    val originalTypeParams = sym.owner.typeParams
    ensureUpToDate(unit)
    parseAndEnter(unit)
    val pre = adaptToNewRunMap(ThisType(sym.owner))
    val rawsym = pre.typeSymbol.info.decl(sym.name)
    val newsym = rawsym filter { alt =>
      sym.isType || {
        try {
          val tp1 = pre.memberType(alt) onTypeError NoType
          val tp2 = adaptToNewRunMap(sym.tpe) substSym (originalTypeParams, sym.owner.typeParams)
          matchesType(tp1, tp2, false) || {
            debugLog(s"findMirrorSymbol matchesType($tp1, $tp2) failed")
            val tp3 = adaptToNewRunMap(sym.tpe) substSym (originalTypeParams, alt.owner.typeParams)
            matchesType(tp1, tp3, false) || {
              debugLog(s"findMirrorSymbol fallback matchesType($tp1, $tp3) failed")
              false
            }
          }
        }
        catch {
          case ex: ControlThrowable => throw ex
          case ex: Throwable =>
            debugLog("error in findMirrorSymbol: " + ex)
            ex.printStackTrace()
            false
        }
      }
    }
    if (newsym == NoSymbol) {
      if (rawsym.exists && !rawsym.isOverloaded) rawsym
      else {
        debugLog("mirror not found " + sym + " " + unit.source + " " + pre)
        NoSymbol
      }
    } else if (newsym.isOverloaded) {
      settings.uniqid.value = true
      debugLog("mirror ambiguous " + sym + " " + unit.source + " " + pre + " " + newsym.alternatives)
      NoSymbol
    } else {
      debugLog("mirror found for " + newsym + ": " + newsym.pos)
      newsym
    }
  }

  /** Implements CompilerControl.askLinkPos */
  private[interactive] def getLinkPos(sym: Symbol, source: SourceFile, response: Response[Position]) {
    informIDE("getLinkPos "+sym+" "+source)
    respond(response) {
      if (sym.owner.isClass) {
        withTempUnit(source){ u =>
          findMirrorSymbol(sym, u).pos
        }
      } else {
        debugLog("link not in class "+sym+" "+source+" "+sym.owner)
        NoPosition
      }
    }
  }

  private def forceDocComment(sym: Symbol, unit: RichCompilationUnit) {
    unit.body foreachPartial {
      case DocDef(comment, defn) if defn.symbol == sym =>
        fillDocComment(defn.symbol, comment)
        EmptyTree
      case _: ValOrDefDef =>
        EmptyTree
    }
  }

  /** Implements CompilerControl.askDocComment */
  private[interactive] def getDocComment(sym: Symbol, source: SourceFile, site: Symbol, fragments: List[(Symbol,SourceFile)],
                                         response: Response[(String, String, Position)]) {
    informIDE(s"getDocComment $sym at $source, site $site")
    respond(response) {
      withTempUnits(fragments.unzip._2){ units =>
        for((sym, src) <- fragments) {
          val mirror = findMirrorSymbol(sym, units(src))
          if (mirror ne NoSymbol) forceDocComment(mirror, units(src))
        }
        val mirror = findMirrorSymbol(sym, units(source))
        if (mirror eq NoSymbol)
          ("", "", NoPosition)
        else {
          (expandedDocComment(mirror, site), rawDocComment(mirror), docCommentPos(mirror))
        }
      }
    }
    // New typer run to remove temp units and drop per-run caches that might refer to symbols entered from temp units.
    newTyperRun()
  }

  def stabilizedType(tree: Tree): Type = tree match {
    case Ident(_) if tree.symbol.isStable =>
      singleType(NoPrefix, tree.symbol)
    case Select(qual, _) if qual.tpe != null && tree.symbol.isStable =>
      singleType(qual.tpe, tree.symbol)
    case Import(expr, selectors) =>
      tree.symbol.info match {
        case analyzer.ImportType(expr) => expr match {
          case s@Select(qual, name) => singleType(qual.tpe, s.symbol)
          case i : Ident => i.tpe
          case _ => tree.tpe
        }
        case _ => tree.tpe
      }

    case _ => tree.tpe
  }

  import analyzer.{SearchResult, ImplicitSearch}

  private[interactive] def getScopeCompletion(pos: Position, response: Response[List[Member]]) {
    informIDE("getScopeCompletion" + pos)
    respond(response) { scopeMembers(pos) }
  }

  private val Dollar = newTermName("$")

  private class Members[M <: Member] extends LinkedHashMap[Name, Set[M]] {
    override def default(key: Name) = Set()

    private def matching(sym: Symbol, symtpe: Type, ms: Set[M]): Option[M] = ms.find { m =>
      (m.sym.name == sym.name) && (m.sym.isType || (m.tpe matches symtpe))
    }

    private def keepSecond(m: M, sym: Symbol, implicitlyAdded: Boolean): Boolean =
      m.sym.hasFlag(ACCESSOR | PARAMACCESSOR) &&
      !sym.hasFlag(ACCESSOR | PARAMACCESSOR) &&
      (!implicitlyAdded || m.implicitlyAdded)

    def add(sym: Symbol, pre: Type, implicitlyAdded: Boolean)(toMember: (Symbol, Type) => M) {
      if ((sym.isGetter || sym.isSetter) && sym.accessed != NoSymbol) {
        add(sym.accessed, pre, implicitlyAdded)(toMember)
      } else if (!sym.name.decodedName.containsName(Dollar) && !sym.isSynthetic && sym.hasRawInfo) {
        val symtpe = pre.memberType(sym) onTypeError ErrorType
        matching(sym, symtpe, this(sym.name)) match {
          case Some(m) =>
            if (keepSecond(m, sym, implicitlyAdded)) {
              //print(" -+ "+sym.name)
              this(sym.name) = this(sym.name) - m + toMember(sym, symtpe)
            }
          case None =>
            //print(" + "+sym.name)
            this(sym.name) = this(sym.name) + toMember(sym, symtpe)
        }
      }
    }

    def addNonShadowed(other: Members[M]) = {
      for ((name, ms) <- other)
        if (ms.nonEmpty && this(name).isEmpty) this(name) = ms
    }

    def allMembers: List[M] = values.toList.flatten
  }

  /** Return all members visible without prefix in context enclosing `pos`. */
  private def scopeMembers(pos: Position): List[ScopeMember] = {
    typedTreeAt(pos) // to make sure context is entered
    val context = doLocateContext(pos)
    val locals = new Members[ScopeMember]
    val enclosing = new Members[ScopeMember]
    def addScopeMember(sym: Symbol, pre: Type, viaImport: Tree) =
      locals.add(sym, pre, false) { (s, st) =>
        new ScopeMember(s, st, context.isAccessible(s, pre, false), viaImport)
      }
    def localsToEnclosing() = {
      enclosing.addNonShadowed(locals)
      locals.clear()
    }
    //print("add scope members")
    var cx = context
    while (cx != NoContext) {
      for (sym <- cx.scope)
        addScopeMember(sym, NoPrefix, EmptyTree)
      localsToEnclosing()
      if (cx == cx.enclClass) {
        val pre = cx.prefix
        for (sym <- pre.members)
          addScopeMember(sym, pre, EmptyTree)
        localsToEnclosing()
      }
      cx = cx.outer
    }
    //print("\nadd imported members")
    for (imp <- context.imports) {
      val pre = imp.qual.tpe
      for (sym <- imp.allImportedSymbols)
        addScopeMember(sym, pre, imp.qual)
      localsToEnclosing()
    }
    // println()
    val result = enclosing.allMembers
//    if (debugIDE) for (m <- result) println(m)
    result
  }

  private[interactive] def getTypeCompletion(pos: Position, response: Response[List[Member]]) {
    informIDE("getTypeCompletion " + pos)
    respondGradually(response) { typeMembers(pos) }
    //if (debugIDE) typeMembers(pos)
  }

  private def typeMembers(pos: Position): Stream[List[TypeMember]] = {
    var tree = typedTreeAt(pos)

    // if tree consists of just x. or x.fo where fo is not yet a full member name
    // ignore the selection and look in just x.
    tree match {
      case Select(qual, name) if tree.tpe == ErrorType => tree = qual
      case _ =>
    }

    val context = doLocateContext(pos)

    if (tree.tpe == null)
      // TODO: guard with try/catch to deal with ill-typed qualifiers.
      tree = analyzer.newTyper(context).typedQualifier(tree)

    debugLog("typeMembers at "+tree+" "+tree.tpe)

    val superAccess = tree.isInstanceOf[Super]
    val members = new Members[TypeMember]

    def addTypeMember(sym: Symbol, pre: Type, inherited: Boolean, viaView: Symbol) = {
      val implicitlyAdded = viaView != NoSymbol
      members.add(sym, pre, implicitlyAdded) { (s, st) =>
        new TypeMember(s, st,
          context.isAccessible(if (s.hasGetter) s.getter(s.owner) else s, pre, superAccess && !implicitlyAdded),
          inherited,
          viaView)
      }
    }

    /** Create a function application of a given view function to `tree` and typechecked it.
     */
    def viewApply(view: SearchResult): Tree = {
      assert(view.tree != EmptyTree)
      analyzer.newTyper(context.makeImplicit(reportAmbiguousErrors = false))
        .typed(Apply(view.tree, List(tree)) setPos tree.pos)
        .onTypeError(EmptyTree)
    }

    val pre = stabilizedType(tree)

    val ownerTpe = tree.tpe match {
      case analyzer.ImportType(expr) => expr.tpe
      case null => pre
      case MethodType(List(), rtpe) => rtpe
      case _ => tree.tpe
    }

    //print("add members")
    for (sym <- ownerTpe.members)
      addTypeMember(sym, pre, sym.owner != ownerTpe.typeSymbol, NoSymbol)
    members.allMembers #:: {
      //print("\nadd pimped")
      val applicableViews: List[SearchResult] =
        if (ownerTpe.isErroneous) List()
        else new ImplicitSearch(
          tree, functionType(List(ownerTpe), AnyClass.tpe), isView = true,
          context0 = context.makeImplicit(reportAmbiguousErrors = false)).allImplicits
      for (view <- applicableViews) {
        val vtree = viewApply(view)
        val vpre = stabilizedType(vtree)
        for (sym <- vtree.tpe.members) {
          addTypeMember(sym, vpre, false, view.tree.symbol)
        }
      }
      //println()
      Stream(members.allMembers)
    }
  }

  /** Implements CompilerControl.askLoadedTyped */
  private[interactive] def waitLoadedTyped(source: SourceFile, response: Response[Tree], onSameThread: Boolean = true) {
    getUnit(source) match {
      case Some(unit) =>
        if (unit.isUpToDate) {
          debugLog("already typed");
          response set unit.body
        } else if (ignoredFiles(source.file)) {
          response.raise(lastException.getOrElse(CancelException))
        } else if (onSameThread) {
          getTypedTree(source, forceReload = false, response)
        } else {
          debugLog("wait for later")
          outOfDate = true
          waitLoadedTypeResponses(source) += response
        }
      case None =>
        debugLog("load unit and type")
        try reloadSources(List(source))
        finally waitLoadedTyped(source, response, onSameThread)
    }
  }

  /** Implements CompilerControl.askParsedEntered */
  private[interactive] def getParsedEntered(source: SourceFile, keepLoaded: Boolean, response: Response[Tree], onSameThread: Boolean = true) {
    getUnit(source) match {
      case Some(unit) =>
        getParsedEnteredNow(source, response)
      case None =>
        try {
          if (keepLoaded || outOfDate && onSameThread)
            reloadSources(List(source))
        } finally {
          if (keepLoaded || !outOfDate || onSameThread)
            getParsedEnteredNow(source, response)
          else
            getParsedEnteredResponses(source) += response
        }
    }
  }

  /** Parses and enters given source file, stroring parse tree in response */
  private def getParsedEnteredNow(source: SourceFile, response: Response[Tree]) {
    respond(response) {
      onUnitOf(source) { unit =>
        parseAndEnter(unit)
        unit.body
      }
    }
  }

  @deprecated("SI-6458: Instrumentation logic will be moved out of the compiler.","2.10.0")
  def getInstrumented(source: SourceFile, line: Int, response: Response[(String, Array[Char])]) =
    try {
      interruptsEnabled = false
      respond(response) {
        instrument(source, line)
      }
    } finally {
      interruptsEnabled = true
    }

  // ---------------- Helper classes ---------------------------

  /** A transformer that replaces tree `from` with tree `to` in a given tree */
  class TreeReplacer(from: Tree, to: Tree) extends Transformer {
    override def transform(t: Tree): Tree = {
      if (t == from) to
      else if ((t.pos includes from.pos) || t.pos.isTransparent) super.transform(t)
      else t
    }
  }

  /** The typer run */
  class TyperRun extends Run {
    // units is always empty

    /** canRedefine is used to detect double declarations of classes and objects
     *  in multiple source files.
     *  Since the IDE rechecks units several times in the same run, these tests
     *  are disabled by always returning true here.
     */
    override def canRedefine(sym: Symbol) = true

    def typeCheck(unit: CompilationUnit): Unit = {
      applyPhase(typerPhase, unit)
    }

    /** Apply a phase to a compilation unit
     *  @return true iff typechecked correctly
     */
    private def applyPhase(phase: Phase, unit: CompilationUnit) {
      atPhase(phase) { phase.asInstanceOf[GlobalPhase] applyPhase unit }
    }
  }

  def newTyperRun() {
    currentTyperRun = new TyperRun
  }

  class TyperResult(val tree: Tree) extends ControlThrowable

  assert(globalPhase.id == 0)

  implicit def addOnTypeError[T](x: => T): OnTypeError[T] = new OnTypeError(x)

  // OnTypeError should still catch TypeError because of cyclic references,
  // but DivergentImplicit shouldn't leak anymore here
  class OnTypeError[T](op: => T) {
    def onTypeError(alt: => T) = try {
      op
    } catch {
      case ex: TypeError =>
        debugLog("type error caught: "+ex)
        alt
      case ex: DivergentImplicit =>
        if (settings.Xdivergence211.value) {
          debugLog("this shouldn't happen. DivergentImplicit exception has been thrown with -Xdivergence211 turned on: "+ex)
          alt
        } else {
          debugLog("divergent implicit caught: "+ex)
          alt
        }
    }
  }

  /** The compiler has been initialized. Constructors are evaluated in textual order,
   *  so this is set to true only after all super constructors and the primary constructor
   *  have been executed.
   */
  initializing = false
}

object CancelException extends Exception

