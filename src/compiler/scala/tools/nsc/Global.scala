/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.{ File, FileOutputStream, PrintWriter, IOException, FileNotFoundException }
import java.nio.charset.{ Charset, CharsetDecoder, IllegalCharsetNameException, UnsupportedCharsetException }
import compat.Platform.currentTime

import scala.tools.util.Profiling
import scala.collection.{ mutable, immutable }
import io.{ SourceReader, AbstractFile, Path }
import reporters.{ Reporter, ConsoleReporter }
import util.{ Exceptional, ClassPath, SourceFile, Statistics, BatchSourceFile, ScriptSourceFile, ShowPickled, returning }
import reflect.generic.{ PickleBuffer, PickleFormat }
import settings.{ AestheticSettings }

import symtab.{ Flags, SymbolTable, SymbolLoaders, SymbolTrackers }
import symtab.classfile.Pickler
import dependencies.DependencyAnalysis
import plugins.Plugins
import ast._
import ast.parser._
import typechecker._
import transform._

import backend.icode.{ ICodes, GenICode, ICodeCheckers }
import backend.{ ScalaPrimitives, Platform, MSILPlatform, JavaPlatform }
import backend.jvm.GenJVM
import backend.opt.{ Inliners, ClosureElimination, DeadCodeElimination }
import backend.icode.analysis._

class Global(var settings: Settings, var reporter: Reporter) extends SymbolTable
                                                             with CompilationUnits
                                                             with Plugins
                                                             with PhaseAssembly
{
  // alternate constructors ------------------------------------------

  def this(reporter: Reporter) =
    this(new Settings(err => reporter.error(null, err)), reporter)

  def this(settings: Settings) =
    this(settings, new ConsoleReporter(settings))

  // platform specific elements

  type ThisPlatform = Platform[_] { val global: Global.this.type }

  lazy val platform: ThisPlatform =
    if (forMSIL) new { val global: Global.this.type = Global.this } with MSILPlatform
    else new { val global: Global.this.type = Global.this } with JavaPlatform

  def classPath: ClassPath[_] = platform.classPath
  def rootLoader: LazyType = platform.rootLoader

  // sub-components --------------------------------------------------

  /** Generate ASTs */
  object gen extends {
    val global: Global.this.type = Global.this
  } with TreeGen {
    def mkAttributedCast(tree: Tree, pt: Type): Tree =
      typer.typed(mkCast(tree, pt))
  }

  /** Fold constants */
  object constfold extends {
    val global: Global.this.type = Global.this
  } with ConstantFolder

  /** ICode generator */
  object icodes extends {
    val global: Global.this.type = Global.this
  } with ICodes

  /** Scala primitives, used in genicode */
  object scalaPrimitives extends {
    val global: Global.this.type = Global.this
  } with ScalaPrimitives

  /** Computing pairs of overriding/overridden symbols */
  object overridingPairs extends {
    val global: Global.this.type = Global.this
  } with OverridingPairs

  // Optimizer components

  /** ICode analysis for optimization */
  object analysis extends {
    val global: Global.this.type = Global.this
  } with TypeFlowAnalysis

  /** Copy propagation for optimization */
  object copyPropagation extends {
    val global: Global.this.type = Global.this
  } with CopyPropagation

  // Components for collecting and generating output

  /** Some statistics (normally disabled) set with -Ystatistics */
  object statistics extends {
    val global: Global.this.type = Global.this
  } with Statistics

  /** Print tree in detailed form */
  object nodePrinters extends {
    val global: Global.this.type = Global.this
  } with NodePrinters {
    infolevel = InfoLevel.Verbose
  }

  /** Representing ASTs as graphs */
  object treeBrowsers extends {
    val global: Global.this.type = Global.this
  } with TreeBrowsers

  val nodeToString = nodePrinters.nodeToString
  val treeBrowser = treeBrowsers.create()

  // ------------ Hooks for interactive mode-------------------------

  /** Called every time an AST node is successfully typechecked in typerPhase.
   */
  def signalDone(context: analyzer.Context, old: Tree, result: Tree) {}

  /** Called from parser, which signals hereby that a method definition has been parsed. */
  def signalParseProgress(pos: Position) {}

  /** Register new context; called for every created context
   */
  def registerContext(c: analyzer.Context) {}

  /** Register top level class (called on entering the class)
   */
  def registerTopLevelSym(sym: Symbol) {}

// ------------------ Reporting -------------------------------------

  // not deprecated yet, but a method called "error" imported into
  // nearly every trait really must go.  For now using globalError.
  def error(msg: String)       = globalError(msg)
  def globalError(msg: String) = reporter.error(NoPosition, msg)
  def inform(msg: String)      = reporter.info(NoPosition, msg, true)
  def warning(msg: String)     =
    if (opt.fatalWarnings) globalError(msg)
    else reporter.warning(NoPosition, msg)

  private def elapsedMessage(msg: String, start: Long) =
    msg + " in " + (currentTime - start) + "ms"

  def informComplete(msg: String): Unit    = reporter.withoutTruncating(inform(msg))
  def informProgress(msg: String)          = if (opt.verbose) inform("[" + msg + "]")
  def inform[T](msg: String, value: T): T  = returning(value)(x => inform(msg + x))
  def informTime(msg: String, start: Long) = informProgress(elapsedMessage(msg, start))

  def logError(msg: String, t: Throwable): Unit = ()
  def log(msg: => AnyRef): Unit = if (opt.logPhase) inform("[log " + phase + "] " + msg)

  def logThrowable(t: Throwable): Unit = globalError(throwableAsString(t))
  def throwableAsString(t: Throwable): String =
    if (opt.richExes) Exceptional(t).force().context()
    else util.stackTraceString(t)

// ------------ File interface -----------------------------------------

  private val reader: SourceReader = {
    val defaultEncoding = Properties.sourceEncoding
    val defaultReader   = Properties.sourceReader

    def loadCharset(name: String) =
      try Some(Charset.forName(name))
      catch {
        case _: IllegalCharsetNameException =>
          globalError("illegal charset name '" + name + "'")
          None
        case _: UnsupportedCharsetException =>
          globalError("unsupported charset '" + name + "'")
          None
      }

    val charset = opt.encoding flatMap loadCharset getOrElse {
      settings.encoding.value = defaultEncoding // A mandatory charset
      Charset.forName(defaultEncoding)
    }

    def loadReader(name: String): Option[SourceReader] = {
      def ccon = Class.forName(name).getConstructor(classOf[CharsetDecoder], classOf[Reporter])

      try Some(ccon.newInstance(charset.newDecoder(), reporter).asInstanceOf[SourceReader])
      catch { case x =>
        globalError("exception while trying to instantiate source reader '" + name + "'")
        None
      }
    }

    opt.sourceReader flatMap loadReader getOrElse {
      new SourceReader(charset.newDecoder(), reporter)
    }
  }

  if (!dependencyAnalysis.off)
    dependencyAnalysis.loadDependencyAnalysis()

  if (opt.verbose || opt.logClasspath) {
    // Uses the "do not truncate" inform
    informComplete("[search path for source files: " + classPath.sourcepaths.mkString(",") + "]")
    informComplete("[search path for class files: " + classPath.asClasspathString + "]")
  }

  object opt extends AestheticSettings {
    def settings = Global.this.settings

    // protected implicit lazy val globalPhaseOrdering: Ordering[Phase] = Ordering[Int] on (_.id)
    def isActive(ph: Settings#PhasesSetting)  = ph containsPhase globalPhase
    def wasActive(ph: Settings#PhasesSetting) = ph containsPhase globalPhase.prev

    // Allows for syntax like scalac -Xshow-class Random@erasure,typer
    private def splitClassAndPhase(str: String, term: Boolean): Name = {
      def mkName(s: String) = if (term) newTermName(s) else newTypeName(s)
      (str indexOf '@') match {
        case -1   => mkName(str)
        case idx  =>
          val phasePart = str drop (idx + 1)
          settings.Yshow.tryToSetColon(phasePart split ',' toList)
          mkName(str take idx)
      }
    }

    // debugging
    def checkPhase = wasActive(settings.check)
    def logPhase   = isActive(settings.log)
    def typerDebug = settings.Ytyperdebug.value
    def writeICode = settings.writeICode.value

    // showing/printing things
    def browsePhase   = isActive(settings.browse)
    def echoFilenames = opt.debug && (opt.verbose || currentRun.size < 5)
    def noShow        = settings.Yshow.isDefault
    def printLate     = settings.printLate.value
    def printPhase    = isActive(settings.Xprint)
    def showNames     = List(showClass, showObject).flatten
    def showPhase     = isActive(settings.Yshow)
    def showSymbols   = settings.Yshowsyms.value
    def showTrees     = settings.Xshowtrees.value
    val showClass     = optSetting[String](settings.Xshowcls) map (x => splitClassAndPhase(x, false))
    val showObject    = optSetting[String](settings.Xshowobj) map (x => splitClassAndPhase(x, true))

    // profiling
    def profCPUPhase = isActive(settings.Yprofile) && !profileAll
    def profileAll   = settings.Yprofile.doAllPhases
    def profileAny   = !settings.Yprofile.isDefault || !settings.YprofileMem.isDefault
    def profileClass = settings.YprofileClass.value
    def profileMem   = settings.YprofileMem.value

    // XXX: short term, but I can't bear to add another option.
    // scalac -Dscala.timings will make this true.
    def timings       = sys.props contains "scala.timings"
  }

  // True if -Xscript has been set, indicating a script run.
  def isScriptRun = opt.script.isDefined

  def getSourceFile(f: AbstractFile): BatchSourceFile =
    if (isScriptRun) ScriptSourceFile(f, reader read f)
    else new BatchSourceFile(f, reader read f)

  def getSourceFile(name: String): SourceFile = {
    val f = AbstractFile.getFile(name)
    if (f eq null) throw new FileNotFoundException(
      "source file '" + name + "' could not be found")
    getSourceFile(f)
  }

  lazy val loaders = new SymbolLoaders {
    val global: Global.this.type = Global.this
  }

// ------------ Phases -------------------------------------------}

  var globalPhase: Phase = NoPhase

  val MaxPhases = 64

  val phaseWithId: Array[Phase] = Array.fill(MaxPhases)(NoPhase)

  abstract class GlobalPhase(prev: Phase) extends Phase(prev) {
    phaseWithId(id) = this

    def run() {
      echoPhaseSummary(this)
      currentRun.units foreach applyPhase
    }

    def apply(unit: CompilationUnit): Unit

    private val isErased = prev.name == "erasure" || prev.erasedTypes
    override def erasedTypes: Boolean = isErased
    private val isFlat = prev.name == "flatten" || prev.flatClasses
    override def flatClasses: Boolean = isFlat
    // private val isDevirtualized = prev.name == "devirtualize" || prev.devirtualized
    // override def devirtualized: Boolean = isDevirtualized  // (part of DEVIRTUALIZE)
    private val isSpecialized = prev.name == "specialize" || prev.specialized
    override def specialized: Boolean = isSpecialized
    private val isRefChecked = prev.name == "refchecks" || prev.refChecked
    override def refChecked: Boolean = isRefChecked

    /** Is current phase cancelled on this unit? */
    def cancelled(unit: CompilationUnit) = {
      // run the typer only if in `createJavadoc` mode
      val maxJavaPhase = if (createJavadoc) currentRun.typerPhase.id else currentRun.namerPhase.id
      reporter.cancelled || unit.isJava && this.id > maxJavaPhase
    }

    final def applyPhase(unit: CompilationUnit) {
      if (opt.echoFilenames)
        inform("[running phase " + name + " on " + unit + "]")

      val unit0 = currentRun.currentUnit
      try {
        currentRun.currentUnit = unit
        if (!cancelled(unit)) {
          currentRun.informUnitStarting(this, unit)
          reporter.withSource(unit.source) { apply(unit) }
        }
        currentRun.advanceUnit
      } finally {
        //assert(currentRun.currentUnit == unit)
        currentRun.currentUnit = unit0
      }
    }
  }

  /** Switch to turn on detailed type logs */
  var printTypings = opt.typerDebug

  // phaseName = "parser"
  object syntaxAnalyzer extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]()
    val runsRightAfter = None
  } with SyntaxAnalyzer

  // factory for phases: namer, packageobjects, typer
  object analyzer extends {
    val global: Global.this.type = Global.this
  } with Analyzer

  // phaseName = "superaccessors"
  object superAccessors extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("typer")
    val runsRightAfter = None
  } with SuperAccessors

  // phaseName = "pickler"
  object pickler extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("superaccessors")
    val runsRightAfter = None
  } with Pickler

  // phaseName = "refchecks"
  object refchecks extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("pickler")
    val runsRightAfter = None
  } with RefChecks

  // phaseName = "liftcode"
  object liftcode extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("refchecks")
    val runsRightAfter = None
  } with LiftCode

  // phaseName = "uncurry"
  object uncurry extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("refchecks", "liftcode")
    val runsRightAfter = None
  } with UnCurry

  // phaseName = "tailcalls"
  object tailCalls extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("uncurry")
    val runsRightAfter = None
  } with TailCalls

  // phaseName = "explicitouter"
  object explicitOuter extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("tailcalls")
    val runsRightAfter = None
  } with ExplicitOuter

  // phaseName = "specialize"
  object specializeTypes extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("")
    val runsRightAfter = Some("tailcalls")
  } with SpecializeTypes

  // phaseName = "erasure"
  object erasure extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("explicitouter")
    val runsRightAfter = Some("explicitouter")
  } with Erasure

  // phaseName = "lazyvals"
  object lazyVals extends {
    final val FLAGS_PER_WORD = 32
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("erasure")
    val runsRightAfter = None
  } with LazyVals

  // phaseName = "lambdalift"
  object lambdaLift extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("lazyvals")
    val runsRightAfter = None
  } with LambdaLift

  // phaseName = "constructors"
  object constructors extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("lambdalift")
    val runsRightAfter = None
  } with Constructors

  // phaseName = "flatten"
  object flatten extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("constructors")
    val runsRightAfter = None
  } with Flatten

  // phaseName = "mixin"
  object mixer extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("flatten", "constructors")
    val runsRightAfter = None
  } with Mixin

  // phaseName = "cleanup"
  object cleanup extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("mixin")
    val runsRightAfter = None
  } with CleanUp

  // phaseName = "icode"
  object genicode extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("cleanup")
    val runsRightAfter = None
  } with GenICode

  // phaseName = "inliner"
  object inliner extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("icode")
    val runsRightAfter = None
  } with Inliners

  // phaseName = "closelim"
  object closureElimination extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("inliner")
    val runsRightAfter = None
  } with ClosureElimination

  // phaseName = "dce"
  object deadCode extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("closelim")
    val runsRightAfter = None
  } with DeadCodeElimination

  // phaseName = "jvm"
  object genJVM extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("dce")
    val runsRightAfter = None
  } with GenJVM

  // This phase is optional: only added if settings.make option is given.
  // phaseName = "dependencyAnalysis"
  object dependencyAnalysis extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("jvm")
    val runsRightAfter = None
  } with DependencyAnalysis

  // phaseName = "terminal"
  object terminal extends {
    val global: Global.this.type = Global.this
    val phaseName = "terminal"
    val runsAfter = List[String]("jvm", "msil")
    val runsRightAfter = None
  } with SubComponent {
    private var cache: Option[GlobalPhase] = None
    def reset(): Unit = cache = None

    def newPhase(prev: Phase): GlobalPhase =
      cache getOrElse returning(new TerminalPhase(prev))(x => cache = Some(x))

    class TerminalPhase(prev: Phase) extends GlobalPhase(prev) {
      def name = "terminal"
      def apply(unit: CompilationUnit) {}
    }
  }

  // phaseName = "SAMPLE PHASE"
  object sampleTransform extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]()
    val runsRightAfter = None
  } with SampleTransform

  /** The checkers are for validating the compiler data structures
   *  at phase boundaries.
   */

  /** Tree checker */
  object treeChecker extends {
    val global: Global.this.type = Global.this
  } with TreeCheckers

  /** Icode verification */
  object icodeCheckers extends {
    val global: Global.this.type = Global.this
  } with ICodeCheckers

  object icodeChecker extends icodeCheckers.ICodeChecker()

  object typer extends analyzer.Typer(
    analyzer.NoContext.make(EmptyTree, Global.this.definitions.RootClass, new Scope)
  )

  /** Add the internal compiler phases to the phases set.
   *  This implementation creates a description map at the same time.
   */
  protected def computeInternalPhases() {
    // Note: this fits -Xshow-phases into 80 column width, which it is
    // desirable to preserve.
    val phs = List(
      syntaxAnalyzer          -> "parse source into ASTs, perform simple desugaring",
      analyzer.namerFactory   -> "resolve names, attach symbols to named trees",
      analyzer.packageObjects -> "load package objects",
      analyzer.typerFactory   -> "the meat and potatoes: type the trees",
      superAccessors          -> "add super accessors in traits and nested classes",
      pickler                 -> "serialize symbol tables",
      refchecks               -> "reference/override checking, translate nested objects",
      uncurry                 -> "uncurry, translate function values to anonymous classes",
      tailCalls               -> "replace tail calls by jumps",
      specializeTypes         -> "@specialized-driven class and method specialization",
      explicitOuter           -> "this refs to outer pointers, translate patterns",
      erasure                 -> "erase types, add interfaces for traits",
      lazyVals                -> "allocate bitmaps, translate lazy vals into lazified defs",
      lambdaLift              -> "move nested functions to top level",
      constructors            -> "move field definitions into constructors",
      mixer                   -> "mixin composition",
      cleanup                 -> "platform-specific cleanups, generate reflective calls",
      genicode                -> "generate portable intermediate code",
      inliner                 -> "optimization: do inlining",
      closureElimination      -> "optimization: eliminate uncalled closures",
      deadCode                -> "optimization: eliminate dead code",
      terminal                -> "The last phase in the compiler chain"
    )

    phs foreach (addToPhasesSet _).tupled
  }
  // This is slightly inelegant but it avoids adding a new member to SubComponent,
  // and attractive -Xshow-phases output is unlikely if the descs span 20 files anyway.
  private val otherPhaseDescriptions = Map(
    "flatten"  -> "eliminate inner classes",
    "liftcode" -> "reify trees",
    "jvm"      -> "generate JVM bytecode"
  ) withDefaultValue ""

  protected def computePlatformPhases() = platform.platformPhases foreach { sub =>
    addToPhasesSet(sub, otherPhaseDescriptions(sub.phaseName))
  }

  // sequences the phase assembly
  protected def computePhaseDescriptors: List[SubComponent] = {
    computeInternalPhases()       // Global.scala
    computePlatformPhases()       // backend/Platform.scala
    computePluginPhases()         // plugins/Plugins.scala
    buildCompilerFromPhasesSet()  // PhaseAssembly.scala
  }

  /* The phase descriptor list */
  lazy val phaseDescriptors: List[SubComponent] = computePhaseDescriptors

  /* The set of phase objects that is the basis for the compiler phase chain */
  protected lazy val phasesSet     = new mutable.HashSet[SubComponent]
  protected lazy val phasesDescMap = new mutable.HashMap[SubComponent, String] withDefaultValue ""
  private lazy val phaseTimings = new Phase.TimingModel   // tracking phase stats
  protected def addToPhasesSet(sub: SubComponent, descr: String) {
    phasesSet += sub
    phasesDescMap(sub) = descr
  }

  /** The names of the phases. */
  lazy val phaseNames = {
    new Run // force some initialization
    phaseDescriptors map (_.phaseName)
  }

  /** A description of the phases that will run */
  def phaseDescriptions: String = {
    val width = phaseNames map (_.length) max
    val fmt   = "%" + width + "s  %2s  %s\n"

    val line1 = fmt.format("phase name", "id", "description")
    val line2 = fmt.format("----------", "--", "-----------")
    val descs = phaseDescriptors.zipWithIndex map {
      case (ph, idx) => fmt.format(ph.phaseName, idx + 1, phasesDescMap(ph))
    }
    line1 :: line2 :: descs mkString
  }

  // ----------- Runs ---------------------------------------

  private var curRun: Run = null
  private var curRunId = 0

  /** Remove the current run when not needed anymore. Used by the build
   *  manager to save on the memory foot print. The current run holds on
   *  to all compilation units, which in turn hold on to trees.
   */
  private [nsc] def dropRun() {
    curRun = null
  }

  /** The currently active run
   */
  def currentRun: Run = curRun

  /** The id of the currently active run
   */
  override def currentRunId = curRunId

  def echoPhaseSummary(ph: Phase) = {
    /** Only output a summary message under debug if we aren't echoing each file. */
    if (opt.debug && !opt.echoFilenames)
      inform("[running phase " + ph.name + " on " + currentRun.size +  " compilation units]")
  }

  /** A Run is a single execution of the compiler on a sets of units
   */
  class Run {
    var isDefined = false
    /** To be initialized from firstPhase. */
    private var terminalPhase: Phase = NoPhase

    // Calculate where to stop based on settings -Ystop-before or -Ystop-after.
    // Slightly complicated logic due to wanting -Ystop-before:parser to fail rather
    // than mysteriously running to completion.
    private lazy val stopPhaseSetting = {
      val result = phaseDescriptors sliding 2 collectFirst {
        case xs if xs exists (settings.stopBefore contains _.phaseName) => if (settings.stopBefore contains xs.head.phaseName) xs.head else xs.last
        case xs if settings.stopAfter contains xs.head.phaseName        => xs.last
      }
      if (result exists (_.phaseName == "parser"))
        globalError("Cannot stop before parser phase.")

      result
    }
    // The phase to stop BEFORE running.
    protected def stopPhase(name: String) = stopPhaseSetting exists (_.phaseName == name)
    protected def skipPhase(name: String) = settings.skip contains name

    /** As definitions.init requires phase != NoPhase, and calling phaseDescriptors.head
     *  will force init, there is some jockeying herein regarding init order: instead of
     *  taking the head descriptor we create a parser phase directly.
     */
    private val firstPhase = {
      /** Initialization. */
      curRunId += 1
      curRun = this

      /** Set phase to a newly created syntaxAnalyzer and call definitions.init. */
      val parserPhase: Phase = syntaxAnalyzer.newPhase(NoPhase)
      phase = parserPhase
      definitions.init

      // Flush the cache in the terminal phase: the chain could have been built
      // before without being used. (This happens in the interpreter.)
      terminal.reset

      // Each subcomponent supplies a phase, which are chained together.
      //   If -Ystop:phase is given, neither that phase nor any beyond it is added.
      //   If -Yskip:phase is given, that phase will be skipped.
      val lastPhase = phaseDescriptors.tail .
        takeWhile (pd => !stopPhase(pd.phaseName)) .
        filterNot (pd =>  skipPhase(pd.phaseName)) .
        foldLeft (parserPhase) ((chain, ph) => ph newPhase chain)

      // Ensure there is a terminal phase at the end, since -Ystop may have limited the phases.
      terminalPhase =
        if (lastPhase.name == "terminal") lastPhase
        else terminal newPhase lastPhase

      parserPhase
    }

    // --------------- Miscellania -------------------------------

    /** The currently compiled unit; set from GlobalPhase */
    var currentUnit: CompilationUnit = _

    /** Counts for certain classes of warnings during this run. */
    var deprecationWarnings: Int = 0
    var uncheckedWarnings: Int = 0

    /** Progress tracking.  Measured in "progress units" which are 1 per
     *  compilation unit per phase completed.
     *
     *  @param    current   number of "progress units" completed
     *  @param    total     total number of "progress units" in run
     */
    def progress(current: Int, total: Int) {}

    /**
     * For subclasses to override. Called when `phase` is about to be run on `unit`.
     * Variables are passed explicitly to indicate that `globalPhase` and `currentUnit` have been set.
     */
    def informUnitStarting(phase: Phase, unit: CompilationUnit) { }

    /** take note that phase is completed
     *  (for progress reporting)
     */
    def advancePhase() {
      unitc = 0
      phasec += 1
      refreshProgress
    }
    /** take note that a phase on a unit is completed
     *  (for progress reporting)
     */
    def advanceUnit() {
      unitc += 1
      refreshProgress
    }

    def cancel() { reporter.cancelled = true }

    private var phasec: Int       = 0   // phases completed
    private var unitc: Int        = 0   // units completed this phase
    private def currentProgress   = (phasec * size) + unitc
    private def totalProgress     = (phaseDescriptors.size - 1) * size // -1: drops terminal phase
    private def refreshProgress() = if (size > 0) progress(currentProgress, totalProgress)

    // ----- finding phases --------------------------------------------

    def phaseNamed(name: String): Phase = {
      var p: Phase = firstPhase
      while (p.next != p && p.name != name) p = p.next
      if (p.name != name) NoPhase else p
    }

    val parserPhase        = phaseNamed("parser")
    val namerPhase         = phaseNamed("namer")
    // packageobjects
    val typerPhase         = phaseNamed("typer")
    // superaccessors
    val picklerPhase       = phaseNamed("pickler")
    val refchecksPhase     = phaseNamed("refchecks")
    val uncurryPhase       = phaseNamed("uncurry")
    // tailcalls, specialize
    val explicitouterPhase = phaseNamed("explicitouter")
    val erasurePhase       = phaseNamed("erasure")
    // lazyvals, lambdalift, constructors
    val flattenPhase       = phaseNamed("flatten")
    val mixinPhase         = phaseNamed("mixin")
    val cleanupPhase       = phaseNamed("cleanup")
    val icodePhase         = phaseNamed("icode")
    // inliner, closelim, dce
    val jvmPhase           = phaseNamed("jvm")

    def runIsAt(ph: Phase)   = globalPhase.id == ph.id
    def runIsPast(ph: Phase) = globalPhase.id > ph.id

    isDefined = true

    // ----------- Units and top-level classes and objects --------

    private val unitbuf = new mutable.ListBuffer[CompilationUnit]
    val compiledFiles   = new mutable.HashSet[String]

    private var _unitbufSize = 0
    def size = _unitbufSize

    /** add unit to be compiled in this run */
    private def addUnit(unit: CompilationUnit) {
      unitbuf += unit
      _unitbufSize += 1 // counting as they're added so size is cheap
      compiledFiles += unit.source.file.path
    }
    private def checkDeprecatedSettings(unit: CompilationUnit) {
      // issue warnings for any usage of deprecated settings
      settings.userSetSettings filter (_.isDeprecated) foreach { s =>
        unit.deprecationWarning(NoPosition, s.name + " is deprecated: " + s.deprecationMessage.get)
      }
    }

    /* An iterator returning all the units being compiled in this run */
    /* !!! Note: changing this to unitbuf.toList.iterator breaks a bunch
       of tests in tests/res.  This is bad, it means the resident compiler
       relies on an iterator of a mutable data structure reflecting changes
       made to the underlying structure (in whatever accidental way it is
       currently depending upon.)
     */
    def units: Iterator[CompilationUnit] = unitbuf.iterator

    /** A map from compiled top-level symbols to their source files */
    val symSource = new mutable.HashMap[Symbol, AbstractFile]

    /** A map from compiled top-level symbols to their picklers */
    val symData = new mutable.HashMap[Symbol, PickleBuffer]

    def registerPickle(sym: Symbol): Unit = {
      // Convert all names to the type name: objects don't store pickled data
      if (opt.showPhase && (opt.showNames exists (x => findNamedMember(x.toTypeName, sym) != NoSymbol))) {
        symData get sym foreach { pickle =>
          ShowPickled.show("\n<<-- " + sym.fullName + " -->>\n", pickle, false)
        }
      }
    }

    /** does this run compile given class, module, or case factory? */
    def compiles(sym: Symbol): Boolean =
      if (sym == NoSymbol) false
      else if (symSource.isDefinedAt(sym)) true
      else if (!sym.owner.isPackageClass) compiles(sym.toplevelClass)
      else if (sym.isModuleClass) compiles(sym.sourceModule)
      else false

    /** Is this run allowed to redefine the given symbol? Usually this is true
     *  if the run does not already compile `sym`, but for interactive mode
     *  we have a more liberal interpretation.
     */
    def canRedefine(sym: Symbol) = !compiles(sym)

    // --------------- Compilation methods ----------------------------

    protected def runCheckers() {
      val toCheck  = globalPhase.prev
      val canCheck = toCheck.checkable
      val fmt      = if (canCheck) "[Now checking: %s]" else "[Not checkable: %s]"

      inform(fmt format toCheck.name)

      if (canCheck) {
        phase = globalPhase
        if (globalPhase.id >= icodePhase.id) icodeChecker.checkICodes
        else treeChecker.checkTrees
      }
    }

    private def showMembers() =
      opt.showNames foreach (x => showDef(x, opt.declsOnly, globalPhase))

    // If -Yprofile isn't given this will never be triggered.
    lazy val profiler = Class.forName(opt.profileClass).newInstance().asInstanceOf[Profiling]

    // Similarly, this will only be created under -Yshow-syms.
    object trackerFactory extends SymbolTrackers {
      val global: Global.this.type = Global.this
      lazy val trackers = currentRun.units.toList map (x => SymbolTracker(x))
      def snapshot() = {
        inform("\n[[symbol layout at end of " + phase + "]]")
        atPhase(phase.next) {
          trackers foreach { t =>
            t.snapshot()
            inform(t.show("Heading from " + phase.prev.name + " to " + phase.name))
          }
        }
      }
    }

    def reportCompileErrors() {
      if (reporter.hasErrors) {
        for ((sym, file) <- symSource.iterator) {
          sym.reset(new loaders.SourcefileLoader(file))
          if (sym.isTerm)
            sym.moduleClass reset loaders.moduleClassLoader
        }
      }
      else {
        def warn(count: Int, what: String, option: Settings#BooleanSetting) = (
          if (option.isDefault && count > 0)
            warning("there were %d %s warnings; re-run with %s for details".format(count, what, option.name))
        )
        warn(deprecationWarnings, "deprecation", settings.deprecation)
        warn(uncheckedWarnings, "unchecked", settings.unchecked)
        // todo: migrationWarnings
      }
    }

    /** Compile list of source files */
    def compileSources(_sources: List[SourceFile]) {
      val depSources = dependencyAnalysis calculateFiles _sources.distinct
      val sources    = coreClassesFirst(depSources)
      // there is a problem already, e.g. a plugin was passed a bad option
      if (reporter.hasErrors)
        return

      // nothing to compile, but we should still report use of deprecated options
      if (sources.isEmpty) {
        checkDeprecatedSettings(new CompilationUnit(new BatchSourceFile("<no file>", "")))
        reportCompileErrors()
        return
      }

      val startTime = currentTime
      reporter.reset();
      {
        val first :: rest = sources
        val unit = new CompilationUnit(first)
        addUnit(unit)
        checkDeprecatedSettings(unit)

        for (source <- rest)
          addUnit(new CompilationUnit(source))
      }
      globalPhase = firstPhase

      if (opt.profileAll) {
        inform("starting CPU profiling on compilation run")
        profiler.startProfiling()
      }
      while (globalPhase != terminalPhase && !reporter.hasErrors) {
        val startTime = currentTime
        phase = globalPhase

        if (opt.profCPUPhase) {
          inform("starting CPU profiling on phase " + globalPhase.name)
          profiler profile globalPhase.run
        }
        else globalPhase.run

        // Create a profiling generation for each phase's allocations
        if (opt.profileAny)
          profiler.advanceGeneration(globalPhase.name)

        // progress update
        informTime(globalPhase.description, startTime)
        phaseTimings(globalPhase) = currentTime - startTime

        // write icode to *.icode files
        if (opt.writeICode && (runIsAt(icodePhase) || opt.printPhase && runIsPast(icodePhase)))
          writeICode()

        // print trees
        if (opt.printPhase || opt.printLate && runIsAt(cleanupPhase)) {
          if (opt.showTrees) nodePrinters.printAll()
          else printAllUnits()
        }
        // print the symbols presently attached to AST nodes
        if (opt.showSymbols)
          trackerFactory.snapshot()

        // print members
        if (opt.showPhase)
          showMembers()

        // browse trees with swing tree viewer
        if (opt.browsePhase)
          treeBrowser browse (phase.name, units)

        // move the pointer
        globalPhase = globalPhase.next

        // run tree/icode checkers
        if (opt.checkPhase)
          runCheckers()

        // output collected statistics
        if (opt.printStats)
          statistics.print(phase)

        advancePhase
      }
      if (opt.profileAll)
        profiler.stopProfiling()

      if (opt.timings)
        inform(phaseTimings.formatted)

      // In case no phase was specified for -Xshow-class/object, show it now for sure.
      if (opt.noShow)
        showMembers()

      reportCompileErrors()
      symSource.keys foreach (x => resetPackageClass(x.owner))
      informTime("total", startTime)

      // save heap snapshot if requested
      if (opt.profileMem) {
        inform("Saving heap snapshot, this could take a while...")
        System.gc()
        profiler.captureSnapshot()
        inform("...done saving heap snapshot.")
        specializeTypes.printSpecStats()
      }

      // record dependency data
      if (!dependencyAnalysis.off)
        dependencyAnalysis.saveDependencyAnalysis()
    }

    /** Compile list of abstract files. */
    def compileFiles(files: List[AbstractFile]) {
      try compileSources(files map getSourceFile)
      catch { case ex: IOException => globalError(ex.getMessage()) }
    }

    /** Compile list of files given by their names */
    def compile(filenames: List[String]) {
      try {
        val sources: List[SourceFile] =
          if (isScriptRun && filenames.size > 1) returning(Nil)(_ => globalError("can only compile one script at a time"))
          else filenames map getSourceFile

        compileSources(sources)
      }
      catch { case ex: IOException => globalError(ex.getMessage()) }
    }

    /** Compile abstract file until `globalPhase`, but at least
     *  to phase "namer".
     */
    def compileLate(file: AbstractFile) {
      if (!compiledFiles(file.path))
        compileLate(new CompilationUnit(getSourceFile(file)))
    }

    /** Compile abstract file until `globalPhase`, but at least to phase "namer".
     */
    def compileLate(unit: CompilationUnit) {
      def stop(ph: Phase) = ph == null || ph.id >= (globalPhase.id max typerPhase.id)
      def loop(ph: Phase) {
        if (stop(ph)) refreshProgress
        else {
          reporter.withSource(unit.source) {
            atPhase(ph)(ph.asInstanceOf[GlobalPhase] applyPhase unit)
          }
          loop(ph.next match {
            case `ph`   => null   // ph == ph.next implies terminal, and null ends processing
            case x      => x
          })
        }
      }
      addUnit(unit)
      loop(firstPhase)
    }

    /**
     * Attempt to locate a source file providing the given name as a top-level
     * definition in the given context, and add it to the run via compileLate
     * if found.
     */
    def compileSourceFor(context : analyzer.Context, name : Name) = false

    /**
     * Attempt to locate a source file providing the given name as a top-level
     * definition with the given prefix, and add it to the run via compileLate
     * if found.
     */
    def compileSourceFor(qual : Tree, name : Name) = false

    /** Reset package class to state at typer (not sure what this
     *  is needed for?)
     */
    private def resetPackageClass(pclazz: Symbol) {
      atPhase(firstPhase) {
        pclazz.setInfo(atPhase(typerPhase)(pclazz.info))
      }
      if (!pclazz.isRoot) resetPackageClass(pclazz.owner)
    }

    /**
     * Re-orders the source files to
     *  1. ScalaObject
     *  2. LowPriorityImplicits / StandardEmbeddings (i.e. parents of Predef)
     *  3. the rest
     *
     * 1 is to avoid cyclic reference errors.
     * 2 is due to the following. When completing "Predef" (*), typedIdent is called
     * for its parents (e.g. "LowPriorityImplicits"). typedIdent checks whether
     * the symbol reallyExists, which tests if the type of the symbol after running
     * its completer is != NoType.
     * If the "namer" phase has not yet run for "LowPriorityImplicits", the symbol
     * has a SourcefileLoader as type. Calling "doComplete" on it does nothing at
     * all, because the source file is part of the files to be compiled anyway.
     * So the "reallyExists" test will return "false".
     * Only after the namer, the symbol has a lazy type which actually computes
     * the info, and "reallyExists" behaves as expected.
     * So we need to make sure that the "namer" phase is run on predef's parents
     * before running it on predef.
     *
     * (*) Predef is completed early when calling "mkAttributedRef" during the
     *   addition of "import Predef._" to sourcefiles. So this situation can't
     *   happen for user classes.
     *
     */
    private def coreClassesFirst(files: List[SourceFile]) = {
      val goLast = 4
      def rank(f: SourceFile) = {
        if (f.file.container.name != "scala") goLast
        else f.file.name match {
          case "ScalaObject.scala"            => 1
          case "LowPriorityImplicits.scala"   => 2
          case "StandardEmbeddings.scala"     => 2
          case "Predef.scala"                 => 3 /* Predef.scala before Any.scala, etc. */
          case _                              => goLast
        }
      }
      files sortBy rank
    }
  } // class Run

  def printAllUnits() {
    print("[[syntax trees at end of " + phase + "]]")
    atPhase(phase.next) { currentRun.units foreach treePrinter.print }
  }

  private def findMemberFromRoot(fullName: Name): Symbol = {
    val segs = nme.segments(fullName.toString, fullName.isTermName)
    if (segs.isEmpty) NoSymbol
    else findNamedMember(segs.tail, definitions.RootClass.info member segs.head)
  }

  private def findNamedMember(fullName: Name, root: Symbol): Symbol = {
    val segs = nme.segments(fullName.toString, fullName.isTermName)
    if (segs.isEmpty || segs.head != root.simpleName) NoSymbol
    else findNamedMember(segs.tail, root)
  }
  private def findNamedMember(segs: List[Name], root: Symbol): Symbol =
    if (segs.isEmpty) root
    else findNamedMember(segs.tail, root.info member segs.head)

  /** We resolve the class/object ambiguity by passing a type/term name.
   */
  def showDef(fullName: Name, declsOnly: Boolean, ph: Phase) = {
    val boringOwners = Set(definitions.AnyClass, definitions.AnyRefClass, definitions.ObjectClass)
    def phased[T](body: => T): T = afterPhase(ph)(body)
    def boringMember(sym: Symbol) = boringOwners(sym.owner)
    def symString(sym: Symbol) = if (sym.isTerm) sym.defString else sym.toString

    def members(sym: Symbol) = phased(sym.info.members filterNot boringMember map symString)
    def decls(sym: Symbol)   = phased(sym.info.decls.toList map symString)
    def bases(sym: Symbol)   = phased(sym.info.baseClasses map (x => x.kindString + " " + x.fullName))

    // make the type/term selections walking from the root.
    val syms = findMemberFromRoot(fullName) match {
      // The name as given was not found, so we'll sift through every symbol in
      // the run looking for plausible matches.
      case NoSymbol => phased(currentRun.symSource.keys map (sym => findNamedMember(fullName, sym)) filterNot (_ == NoSymbol) toList)
      // The name as given matched, so show only that.
      case sym      => List(sym)
    }

    syms foreach { sym =>
      val name        = "\n<<-- %s %s after phase '%s' -->>".format(sym.kindString, sym.fullName, ph.name)
      val baseClasses = bases(sym).mkString("Base classes:\n  ", "\n  ", "")
      val contents =
        if (declsOnly) decls(sym).mkString("Declarations:\n  ", "\n  ", "")
        else members(sym).mkString("Members (excluding Any/AnyRef unless overridden):\n  ", "\n  ", "")

      inform(List(name, baseClasses, contents) mkString "\n\n")
    }
  }

  /** Returns the file with the given suffix for the given class. Used for icode writing. */
  def getFile(clazz: Symbol, suffix: String): File = {
    val outDir = Path(
      settings.outputDirs.outputDirFor(clazz.sourceFile).path match {
        case ""   => "."
        case path => path
      }
    )
    val segments = clazz.fullName split '.'
    val dir      = segments.init.foldLeft(outDir)(_ / _).createDirectory()

    new File(dir.path, segments.last + suffix)
  }

  private def writeICode() {
    val printer = new icodes.TextPrinter(null, icodes.linearizer)
    icodes.classes.values.foreach((cls) => {
      val suffix = if (cls.symbol.hasModuleFlag) "$.icode" else ".icode"
      var file = getFile(cls.symbol, suffix)
//      if (file.exists())
//        file = new File(file.getParentFile(), file.getName() + "1")
      try {
        val stream = new FileOutputStream(file)
        printer.setWriter(new PrintWriter(stream, true))
        printer.printClass(cls)
        informProgress("wrote " + file)
      } catch {
        case ex: IOException =>
          if (opt.debug) ex.printStackTrace()
        globalError("could not write file " + file)
      }
    })
  }
  // In order to not outright break code which overrides onlyPresentation (like sbt 0.7.5.RC0)
  // I restored and deprecated it.  That would be enough to avoid the compilation
  // failure, but the override wouldn't accomplish anything.  So now forInteractive
  // and forScaladoc default to onlyPresentation, which is the same as defaulting
  // to false except in old code.  The downside is that this leaves us calling a
  // deprecated method: but I see no simple way out, so I leave it for now.
  def forJVM           = opt.jvm
  def forMSIL          = opt.msil
  def forInteractive   = onlyPresentation
  def forScaladoc      = onlyPresentation
  def createJavadoc    = false

  @deprecated("Use forInteractive or forScaladoc, depending on what you're after", "2.9.0")
  def onlyPresentation = false
}
