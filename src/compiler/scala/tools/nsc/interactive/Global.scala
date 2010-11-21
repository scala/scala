package scala.tools.nsc
package interactive

import java.io.{ PrintWriter, StringWriter }

import scala.collection.mutable
import mutable.{LinkedHashMap, SynchronizedMap,LinkedHashSet, SynchronizedSet}
import scala.concurrent.SyncVar
import scala.util.control.ControlThrowable
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{SourceFile, Position, RangePosition, NoPosition, WorkScheduler}
import scala.tools.nsc.reporters._
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._

/** The main class of the presentation compiler in an interactive environment such as an IDE
 */
class Global(settings: Settings, reporter: Reporter)
  extends scala.tools.nsc.Global(settings, reporter)
     with CompilerControl
     with RangePositions
     with ContextTrees
     with RichCompilationUnits {
self =>

  import definitions._

  val debugIDE = false

  /** Print msg only when debugIDE is true. */
  @inline final def debugLog(msg: => String) =
    if (debugIDE) println(msg)

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
  private var currentTyperRun: TyperRun = newTyperRun

  /** Is a background compiler run needed?
   *  Note: outOfDate is true as long as there is a backgroud compile scheduled or going on.
   */
  private var outOfDate = false

  /** Units compiled by a run with id >= minRunId are considered up-to-date  */
  private[interactive] var minRunId = 1

  private val NoResponse: Response[_] = new Response[Any]
  private var pendingResponse: Response[_] = NoResponse

  /** Is a reload/background compiler currently running? */
  private var acting = false

  // ----------- Overriding hooks in nsc.Global -----------------------

  /** Called from typechecker, which signal hereby that a node has been completely typechecked.
   *  If the node includes unit.targetPos, abandons run and returns newly attributed tree.
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
          result.pos.isOpaqueRange &&
          (result.pos includes context.unit.targetPos)) {
        integrateNew()
        var located = new TypedLocator(context.unit.targetPos) locateIn result
        if (located == EmptyTree) {
          println("something's wrong: no "+context.unit+" in "+result+result.pos)
          located = result
        }
        throw new TyperResult(located)
      }
      val typerRun = currentTyperRun

      while(true)
        try {
          try {
            pollForWork()
	  } catch {
            case ex : Throwable =>
	      if (context.unit != null) integrateNew()
              throw ex
	  }
          if (typerRun == currentTyperRun)
            return

          // @Martin
          // Guard against NPEs in integrateNew if context.unit == null here.
          // But why are we doing this at all? If it was non-null previously
          // integrateNew will already have been called. If it was null previously
          // it will still be null now?
          if (context.unit != null)
            integrateNew()
          throw FreshRunReq
        }
        catch {
          case ex : ValidateException => // Ignore, this will have been reported elsewhere
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

  // ----------------- Polling ---------------------------------------

  /** Called from runner thread and signalDone:
   *  Poll for interrupts and execute them immediately.
   *  Then, poll for exceptions and execute them.
   *  Then, poll for work reload/typedTreeAt/doFirst commands during background checking.
   */
  def pollForWork() {
    scheduler.pollInterrupt() match {
      case Some(ir) =>
	try {
	  activeLocks += 1
          ir.execute()
	} finally {
	  activeLocks -= 1
	}
        pollForWork()
      case _ =>
    }
    if (pendingResponse.isCancelled)
      throw CancelException
    scheduler.pollThrowable() match {
      case Some(ex @ FreshRunReq) =>
        currentTyperRun = newTyperRun
        minRunId = currentRunId
        if (outOfDate) throw ex
        else outOfDate = true
      case Some(ex: Throwable) => throw ex
      case _ =>
    }
    scheduler.nextWorkItem() match {
      case Some(action) =>
        try {
          acting = true
          debugLog("picked up work item: "+action)
          action()
          debugLog("done with work item: "+action)
        } finally {
          debugLog("quitting work item: "+action)
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

  /** The current presentation compiler runner */
  @volatile protected var compileRunner = newRunnerThread
  compileRunner.start()

  private var threadId = 1

  /** Create a new presentation compiler runner.
   */
  def newRunnerThread: Thread = new Thread("Scala Presentation Compiler V"+threadId) {
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
              case FreshRunReq =>
            }
          }
        }
      } catch {
        case ShutdownReq =>
          ;
        case ex =>
          outOfDate = false
          compileRunner = newRunnerThread
          compileRunner.start()
          ex match {
            case FreshRunReq =>   // This shouldn't be reported
            case _ : ValidateException => // This will have been reported elsewhere
            case _ => ex.printStackTrace(); inform("Fatal Error: "+ex)
          }
      }
    }
    threadId += 1
  }

  /** Compile all given units
   */
  private def backgroundCompile() {
    if (debugIDE) inform("Starting new presentation compiler type checking pass")
    reporter.reset

    // remove any files in first that are no longer maintained by presentation compiler (i.e. closed)
    firsts = firsts filter (s => unitOfFile contains (s.file))

    val prefix = firsts map unitOf

    val units = prefix ::: (unitOfFile.values.toList diff prefix) filter (!_.isUpToDate)

    recompile(units)

    if (debugIDE) inform("Everything is now up to date")
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
      unit.lastBody = unit.body
      unit.body = EmptyTree
      unit.status = NotLoaded
    }

  /** Parse unit and create a name index. */
  def parse(unit: RichCompilationUnit): Unit = {
    currentTyperRun.compileLate(unit)
    if (!reporter.hasErrors) validatePositions(unit.body)
    //println("parsed: [["+unit.body+"]]")
    if (!unit.isJava) syncTopLevelSyms(unit)
    unit.status = JustParsed
  }

  /** Make sure symbol and type attributes are reset and recompile units.
   */
  def recompile(units: List[RichCompilationUnit]) {
    for (unit <- units) {
      reset(unit)
      debugLog("parsing: "+unit)
      parse(unit)
    }
    for (unit <- units) {
      debugLog("type checking: "+unit)
      activeLocks = 0
      currentTyperRun.typeCheck(unit)
      unit.status = currentRunId
    }
  }

  def syncTopLevelSyms(unit: RichCompilationUnit) {
    val deleted = currentTopLevelSyms filter { sym =>
      /** We sync after namer phase and it resets all the top-level symbols that survive the new parsing
       * round to NoPeriod. */
      sym.sourceFile == unit.source.file && sym.validTo != NoPeriod && runId(sym.validTo) < currentRunId
    }
    for (d <- deleted) {
      d.owner.info.decls unlink d
      deletedTopLevelSyms += d
      currentTopLevelSyms -= d
    }
  }

  /** Move list of files to front of firsts */
  def moveToFront(fs: List[SourceFile]) {
    firsts = fs ::: (firsts diff fs)
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
          if (results.isEmpty) response set result
          else response setProvisionally result
        }
      }
    } catch {
      case CancelException =>
        ;
      case ex @ FreshRunReq =>
        scheduler.postWorkItem(() => respondGradually(response)(op))
        throw ex
      case ex =>
        response raise ex
        throw ex
    } finally {
      pendingResponse = prevResponse
    }
  }

  /** Make sure a set of compilation units is loaded and parsed */
  def reloadSources(sources: List[SourceFile]) {
    currentTyperRun = newTyperRun
    for (source <- sources) {
      val unit = new RichCompilationUnit(source)
      unitOfFile(source.file) = unit
      parse(unit)
    }
    moveToFront(sources)
  }

  /** Make sure a set of compilation units is loaded and parsed */
  def reload(sources: List[SourceFile], response: Response[Unit]) {
    respond(response)(reloadSources(sources))
    if (outOfDate) throw FreshRunReq // cancel background compile
    else outOfDate = true            // proceed normally and enable new background compile
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

  /** A fully attributed tree corresponding to the entire compilation unit  */
  def typedTree(source: SourceFile, forceReload: Boolean): Tree = {
    val unit = unitOf(source)
    val sources = List(source)
    if (unit.status == NotLoaded || forceReload) reloadSources(sources)
    moveToFront(sources)
    currentTyperRun.typedTree(unitOf(source))
  }

  /** Set sync var `response` to a fully attributed tree located at position `pos`  */
  def getTypedTreeAt(pos: Position, response: Response[Tree]) {
    respond(response)(typedTreeAt(pos))
  }

  /** Set sync var `response` to a fully attributed tree corresponding to the entire compilation unit  */
  def getTypedTree(source : SourceFile, forceReload: Boolean, response: Response[Tree]) {
    respond(response)(typedTree(source, forceReload))
  }

  /** Set sync var `result` to the last fully attributed tree produced from the entire compilation unit  */
  def getLastTypedTree(source : SourceFile, result: Response[Tree]) {
    respond(result) {
      val unit = unitOf(source)
      if (unit.status > JustParsed) unit.body
      else if (unit.lastBody ne EmptyTree) unit.lastBody
      else typedTree(source, false)
    }
  }

  def stabilizedType(tree: Tree): Type = tree match {
    case Ident(_) if tree.symbol.isStable => singleType(NoPrefix, tree.symbol)
    case Select(qual, _) if qual.tpe != null && tree.symbol.isStable => singleType(qual.tpe, tree.symbol)
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

  def getScopeCompletion(pos: Position, response: Response[List[Member]]) {
    respond(response) { scopeMembers(pos) }
  }

  val Dollar = newTermName("$")

  /** Return all members visible without prefix in context enclosing `pos`. */
  def scopeMembers(pos: Position): List[ScopeMember] = {
    typedTreeAt(pos) // to make sure context is entered
    val context = doLocateContext(pos)
    val locals = new LinkedHashMap[Name, ScopeMember]
    def addScopeMember(sym: Symbol, pre: Type, viaImport: Tree) =
      if (!sym.name.decode.containsName(Dollar) &&
          !sym.isSynthetic &&
          !locals.contains(sym.name)) {
        locals(sym.name) = new ScopeMember(
          sym,
          pre.memberType(sym),
          context.isAccessible(sym, pre, false),
          viaImport)
      }
    var cx = context
    while (cx != NoContext) {
      for (sym <- cx.scope)
        addScopeMember(sym, NoPrefix, EmptyTree)
      if (cx == cx.enclClass) {
        val pre = cx.prefix
        for (sym <- pre.members)
          addScopeMember(sym, pre, EmptyTree)
      }
      cx = cx.outer
    }

    for (imp <- context.imports) {
      val pre = imp.qual.tpe
      for (sym <- imp.allImportedSymbols) {
        addScopeMember(sym, pre, imp.qual)
      }
    }
    val result = locals.values.toList
    if (debugIDE) for (m <- result) println(m)
    result
  }

  def getTypeCompletion(pos: Position, response: Response[List[Member]]) {
    respondGradually(response) { typeMembers(pos) }
    if (debugIDE) typeMembers(pos)
  }

  def typeMembers(pos: Position): Stream[List[TypeMember]] = {
    var tree = typedTreeAt(pos)

    // Let's say you have something like val x: List[Int] and ypu want to get completion after List
    // Then the tree found at first is a TypeTree, ????
    tree match {
      case tt : TypeTree if tt.original != null => tree = tt.original // ???
      case _ =>
    }

    // if tree consists of just x. or x.fo where fo is not yet a full member name
    // ignore the selection and look in just x.
    tree match {
      case Select(qual, name) if tree.tpe == ErrorType => tree = qual
      case _ =>
    }

    val context = doLocateContext(pos)

    if (tree.tpe == null)
      tree = analyzer.newTyper(context).typedQualifier(tree)

    debugLog("typeMembers at "+tree+" "+tree.tpe)

    val superAccess = tree.isInstanceOf[Super]
    val scope = new Scope
    val members = new LinkedHashMap[Symbol, TypeMember]

    def addTypeMember(sym: Symbol, pre: Type, inherited: Boolean, viaView: Symbol) {
      val symtpe = pre.memberType(sym)
      if (scope.lookupAll(sym.name) forall (sym => !(members(sym).tpe matches symtpe))) {
        scope enter sym
        members(sym) = new TypeMember(
          sym,
          symtpe,
          context.isAccessible(sym, pre, superAccess && (viaView == NoSymbol)),
          inherited,
          viaView)
      }
    }

    /** Create a fucntion application of  a given view function to `tree` and typechecked it.
     */
    def viewApply(view: SearchResult): Tree = {
      assert(view.tree != EmptyTree)
      try {
        analyzer.newTyper(context.makeImplicit(reportAmbiguousErrors = false))
          .typed(Apply(view.tree, List(tree)) setPos tree.pos)
      } catch {
        case ex: TypeError => EmptyTree
      }
    }

    /** Names containing $ are not valid completions. */
    def shouldDisplay(sym: Symbol): Boolean =
      !sym.name.toString.contains("$")

    val pre = stabilizedType(tree)
    val ownerTpe = tree.tpe match {
      case analyzer.ImportType(expr) => expr.tpe
      case null => pre
      case _ => tree.tpe
    }

    for (sym <- ownerTpe.decls if shouldDisplay(sym))
      addTypeMember(sym, pre, false, NoSymbol)
    members.values.toList #:: {
      for (sym <- ownerTpe.members if shouldDisplay(sym))
        addTypeMember(sym, pre, true, NoSymbol)
      members.values.toList #:: {
        val applicableViews: List[SearchResult] =
          new ImplicitSearch(tree, functionType(List(ownerTpe), AnyClass.tpe), isView = true, context.makeImplicit(reportAmbiguousErrors = false))
            .allImplicits
        for (view <- applicableViews) {
          val vtree = viewApply(view)
          val vpre = stabilizedType(vtree)
          for (sym <- vtree.tpe.members) {
            addTypeMember(sym, vpre, false, view.tree.symbol)
          }
        }
        Stream(members.values.toList)
      }
    }
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

    /** canRedefine is used to detect double declarations in multiple source files.
     *  Since the IDE rechecks units several times in the same run, these tests
     *  are disabled by always returning true here.
     */
    override def canRedefine(sym: Symbol) = true

    def typeCheck(unit: CompilationUnit): Unit = {
      activeLocks = 0
      applyPhase(typerPhase, unit)
    }

    def enterNames(unit: CompilationUnit): Unit = {
      applyPhase(namerPhase, unit)
    }


    /** Return fully attributed tree at given position
     *  (i.e. smallest tree containing position)
     */
    def typedTreeAt(pos: Position): Tree = {
      debugLog("starting typedTreeAt")
      val tree = locateTree(pos)
      debugLog("at pos "+pos+" was found: "+tree+tree.pos.show)
      if (stabilizedType(tree) ne null) {
        debugLog("already attributed")
        tree
      } else {
        val unit = unitOf(pos)
        assert(unit.isParsed)
        unit.targetPos = pos
        val lastPrintTypings = printTypings
        try {
          println("starting targeted type check")
          if (debugIDE) printTypings = true
          typeCheck(unit)
          throw new FatalError("tree not found")
        } catch {
          case ex: TyperResult =>
            ex.tree
        } finally {
          unit.targetPos = NoPosition
          printTypings = lastPrintTypings
        }
      }
    }

    def typedTree(unit: RichCompilationUnit): Tree = {
      assert(unit.isParsed)
      unit.targetPos = NoPosition
      typeCheck(unit)
      unit.body
    }

    /** Apply a phase to a compilation unit
     *  @return true iff typechecked correctly
     */
    private def applyPhase(phase: Phase, unit: CompilationUnit) {
      val oldSource = reporter.getSource
      reporter.withSource(unit.source) {
        atPhase(phase) { phase.asInstanceOf[GlobalPhase] applyPhase unit }
      }
    }
  }

  def newTyperRun = new TyperRun

  class TyperResult(val tree: Tree) extends ControlThrowable

  assert(globalPhase.id == 0)
}

object CancelException extends Exception

