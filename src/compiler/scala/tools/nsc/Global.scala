/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.{ File, FileOutputStream, PrintWriter, IOException, FileNotFoundException }
import java.nio.charset.{ Charset, CharsetDecoder, IllegalCharsetNameException, UnsupportedCharsetException }
import compat.Platform.currentTime
import scala.tools.util.PathResolver
import scala.collection.{ mutable, immutable }
import io.{ SourceReader, AbstractFile, Path }
import reporters.{ Reporter, ConsoleReporter }
import util.{ NoPosition, Exceptional, ClassPath, SourceFile, NoSourceFile, Statistics, StatisticsInfo, BatchSourceFile, ScriptSourceFile, ScalaClassLoader, returning }
import scala.reflect.internal.pickling.{ PickleBuffer, PickleFormat }
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
import backend.jvm.{GenJVM, GenASM}
import backend.opt.{ Inliners, InlineExceptionHandlers, ClosureElimination, DeadCodeElimination }
import backend.icode.analysis._
import language.postfixOps
import reflect.internal.StdAttachments

class Global(var currentSettings: Settings, var reporter: Reporter) extends SymbolTable
                                                                       with ClassLoaders
                                                                       with ToolBoxes
                                                                       with CompilationUnits
                                                                       with Plugins
                                                                       with PhaseAssembly
                                                                       with Trees
                                                                       with FreeVars
                                                                       with TreePrinters
                                                                       with DocComments
                                                                       with Positions {

  override def settings = currentSettings

  import definitions.{ findNamedMember, findMemberFromRoot }

  // alternate constructors ------------------------------------------

  def this(reporter: Reporter) =
    this(new Settings(err => reporter.error(null, err)), reporter)

  def this(settings: Settings) =
    this(settings, new ConsoleReporter(settings))

  // fulfilling requirements
  // Renamed AbstractFile to AbstractFileType for backward compatibility:
  // it is difficult for sbt to work around the ambiguity errors which result.
  type AbstractFileType = scala.tools.nsc.io.AbstractFile

  def mkAttributedQualifier(tpe: Type, termSym: Symbol): Tree = gen.mkAttributedQualifier(tpe, termSym)

  def picklerPhase: Phase = if (currentRun.isDefined) currentRun.picklerPhase else NoPhase

  // platform specific elements

  type ThisPlatform = Platform { val global: Global.this.type }

  lazy val platform: ThisPlatform =
    if (forMSIL) new { val global: Global.this.type = Global.this } with MSILPlatform
    else new { val global: Global.this.type = Global.this } with JavaPlatform

  def classPath: ClassPath[platform.BinaryRepr] = platform.classPath
  def rootLoader: LazyType = platform.rootLoader

  // sub-components --------------------------------------------------

  /** Generate ASTs */
  type TreeGen = scala.tools.nsc.ast.TreeGen

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
  } with StatisticsInfo

  /** Print tree in detailed form */
  object nodePrinters extends {
    val global: Global.this.type = Global.this
  } with NodePrinters {
    var lastPrintedPhase: Phase = NoPhase
    var lastPrintedSource: String = ""
    infolevel = InfoLevel.Verbose

    def showUnit(unit: CompilationUnit) {
      print(" // " + unit.source)
      if (unit.body == null) println(": tree is null")
      else {
        val source = util.stringFromWriter(w => newTreePrinter(w) print unit.body)

        // treePrinter show unit.body
        if (lastPrintedSource == source)
          println(": tree is unchanged since " + lastPrintedPhase)
        else {
          lastPrintedPhase = phase.prev // since we're running inside "afterPhase"
          lastPrintedSource = source
          println("")
          println(source)
          println("")
        }
      }
    }
  }

  def withInfoLevel[T](infolevel: nodePrinters.InfoLevel.Value)(op: => T) = {
    val saved = nodePrinters.infolevel
    try {
      nodePrinters.infolevel = infolevel
      op
    } finally {
      nodePrinters.infolevel = saved
    }
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
  def registerContext(c: analyzer.Context) {
    lastSeenContext = c
  }

  /** Register top level class (called on entering the class)
   */
  def registerTopLevelSym(sym: Symbol) {}

// ------------------ Reporting -------------------------------------

  // not deprecated yet, but a method called "error" imported into
  // nearly every trait really must go.  For now using globalError.
  def error(msg: String)       = globalError(msg)
  def globalError(msg: String) = reporter.error(NoPosition, msg)
  def inform(msg: String)      = reporter.echo(msg)
  def warning(msg: String)     =
    if (opt.fatalWarnings) globalError(msg)
    else reporter.warning(NoPosition, msg)

  // Getting in front of Predef's asserts to supplement with more info.
  // This has the happy side effect of masking the one argument forms
  // of assert and require (but for now I've reproduced them here,
  // because there are a million to fix.)
  @inline final def assert(assertion: Boolean, message: => Any) {
    Predef.assert(assertion, supplementErrorMessage("" + message))
  }
  @inline final def assert(assertion: Boolean) {
    assert(assertion, "")
  }
  @inline final def require(requirement: Boolean, message: => Any) {
    Predef.require(requirement, supplementErrorMessage("" + message))
  }
  @inline final def require(requirement: Boolean) {
    require(requirement, "")
  }

  // Needs to call error to make sure the compile fails.
  override def abort(msg: String): Nothing = {
    error(msg)
    super.abort(msg)
  }

  @inline final def ifDebug(body: => Unit) {
    if (settings.debug.value)
      body
  }
  // Warnings issued only under -Ydebug.  For messages which should reach
  // developer ears, but are not adequately actionable by users.
  @inline final override def debugwarn(msg: => String) {
    if (settings.debug.value)
      warning(msg)
  }

  private def elapsedMessage(msg: String, start: Long) =
    msg + " in " + (currentTime - start) + "ms"

  def informComplete(msg: String): Unit    = reporter.withoutTruncating(inform(msg))
  def informProgress(msg: String)          = if (opt.verbose) inform("[" + msg + "]")
  def inform[T](msg: String, value: T): T  = returning(value)(x => inform(msg + x))
  def informTime(msg: String, start: Long) = informProgress(elapsedMessage(msg, start))

  def logError(msg: String, t: Throwable): Unit = ()

  def logAfterEveryPhase[T](msg: String)(op: => T) {
    log("Running operation '%s' after every phase.\n".format(msg) + describeAfterEveryPhase(op))
  }

  def shouldLogAtThisPhase = (
       (settings.log.isSetByUser)
    && ((settings.log containsPhase globalPhase) || (settings.log containsPhase phase))
  )
  // Over 200 closure objects are eliminated by inlining this.
  @inline final def log(msg: => AnyRef) {
    if (shouldLogAtThisPhase)
      inform("[log %s%s] %s".format(globalPhase, atPhaseStackMessage, msg))
  }

  @inline final override def debuglog(msg: => String) {
    if (settings.debug.value)
      log(msg)
  }

  def logThrowable(t: Throwable): Unit = globalError(throwableAsString(t))
  override def throwableAsString(t: Throwable) = util.stackTraceString(t)

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

    // behavior

    // debugging
    def checkPhase = wasActive(settings.check)
    def logPhase   = isActive(settings.log)

    // Write *.icode files the setting was given.
    def writeICode = settings.writeICode.isSetByUser && isActive(settings.writeICode)

    // showing/printing things
    def browsePhase   = isActive(settings.browse)
    def echoFilenames = opt.debug && (opt.verbose || currentRun.size < 5)
    def noShow        = settings.Yshow.isDefault
    def printLate     = settings.printLate.value
    def printPhase    = isActive(settings.Xprint)
    def showNames     = List(showClass, showObject).flatten
    def showPhase     = isActive(settings.Yshow)
    def showSymbols   = settings.Yshowsyms.value
    def showTrees     = settings.Xshowtrees.value || settings.XshowtreesCompact.value || settings.XshowtreesStringified.value
    val showClass     = optSetting[String](settings.Xshowcls) map (x => splitClassAndPhase(x, false))
    val showObject    = optSetting[String](settings.Xshowobj) map (x => splitClassAndPhase(x, true))
  }

  // The current division between scala.reflect.* and scala.tools.nsc.* is pretty
  // clunky.  It is often difficult to have a setting influence something without having
  // to create it on that side.  For this one my strategy is a constant def at the file
  // where I need it, and then an override in Global with the setting.
  override protected val etaExpandKeepsStar = settings.etaExpandKeepsStar.value
  // Here comes another one...
  override protected val enableTypeVarExperimentals = settings.Xexperimental.value

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
      if ((unit ne null) && unit.exists)
        lastSeenSourceFile = unit.source

      if (opt.echoFilenames)
        inform("[running phase " + name + " on " + unit + "]")

      val unit0 = currentUnit
      try {
        currentRun.currentUnit = unit
        if (!cancelled(unit)) {
          currentRun.informUnitStarting(this, unit)
          apply(unit)
        }
        currentRun.advanceUnit
      } finally {
        //assert(currentUnit == unit)
        currentRun.currentUnit = unit0
      }
    }
  }

  /** Switch to turn on detailed type logs */
  var printTypings = settings.Ytyperdebug.value
  var printInfers = settings.Yinferdebug.value

  // phaseName = "parser"
  object syntaxAnalyzer extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]()
    val runsRightAfter = None
  } with SyntaxAnalyzer

  // !!! I think we're overdue for all these phase objects being lazy vals.
  // There's no way for a Global subclass to provide a custom typer
  // despite the existence of a "def newTyper(context: Context): Typer"
  // which is clearly designed for that, because it's defined in
  // Analyzer and Global's "object analyzer" allows no override. For now
  // I only changed analyzer.
  //
  // factory for phases: namer, packageobjects, typer
  lazy val analyzer = new {
    val global: Global.this.type = Global.this
  } with Analyzer

  // phaseName = "patmat"
  object patmat extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("typer")
    // patmat doesn't need to be right after typer, as long as we run before supperaccesors
    // (sbt does need to run right after typer, so don't conflict)
    val runsRightAfter = None
  } with PatternMatching

  // phaseName = "superaccessors"
  object superAccessors extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("patmat")
    val runsRightAfter = None
  } with SuperAccessors

  // phaseName = "extmethods"
  object extensionMethods extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("superaccessors")
    val runsRightAfter = None
  } with ExtensionMethods

  // phaseName = "pickler"
  object pickler extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("extmethods")
    val runsRightAfter = None
  } with Pickler

  // phaseName = "refchecks"
  override object refChecks extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("pickler")
    val runsRightAfter = None
  } with RefChecks

  // phaseName = "uncurry"
  override object uncurry extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("refchecks")
    val runsRightAfter = None
  } with UnCurry

  // phaseName = "tailcalls"
  object tailCalls extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("uncurry")
    val runsRightAfter = None
  } with TailCalls

  // phaseName = "explicitouter"
  object explicitOuter extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("tailcalls")
    val runsRightAfter = None
  } with ExplicitOuter

  // phaseName = "specialize"
  object specializeTypes extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("")
    val runsRightAfter = Some("tailcalls")
  } with SpecializeTypes

  // phaseName = "erasure"
  override object erasure extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("explicitouter")
    val runsRightAfter = Some("explicitouter")
  } with Erasure

  // phaseName = "posterasure"
  object postErasure extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("erasure")
    val runsRightAfter = Some("erasure")
  } with PostErasure

  // phaseName = "lazyvals"
  object lazyVals extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("erasure")
    val runsRightAfter = None
  } with LazyVals

  // phaseName = "lambdalift"
  object lambdaLift extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("lazyvals")
    val runsRightAfter = None
  } with LambdaLift

  // phaseName = "constructors"
  object constructors extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("lambdalift")
    val runsRightAfter = None
  } with Constructors

  // phaseName = "flatten"
  object flatten extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("constructors")
    val runsRightAfter = None
  } with Flatten

  // phaseName = "mixin"
  object mixer extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("flatten", "constructors")
    val runsRightAfter = None
  } with Mixin

  // phaseName = "cleanup"
  object cleanup extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("mixin")
    val runsRightAfter = None
  } with CleanUp

  // phaseName = "icode"
  object genicode extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("cleanup")
    val runsRightAfter = None
  } with GenICode

  // phaseName = "inliner"
  object inliner extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("icode")
    val runsRightAfter = None
  } with Inliners

  // phaseName = "inlineExceptionHandlers"
  object inlineExceptionHandlers extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("inliner")
    val runsRightAfter = None
  } with InlineExceptionHandlers

  // phaseName = "closelim"
  object closureElimination extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("inlineExceptionHandlers")
    val runsRightAfter = None
  } with ClosureElimination

  // phaseName = "dce"
  object deadCode extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("closelim")
    val runsRightAfter = None
  } with DeadCodeElimination

  // phaseName = "jvm", FJBG-based version
  object genJVM extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("dce")
    val runsRightAfter = None
  } with GenJVM

  // phaseName = "jvm", ASM-based version
  object genASM extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("dce")
    val runsRightAfter = None
  } with GenASM

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
    val runsAfter = List("jvm", "msil")
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
    analyzer.NoContext.make(EmptyTree, Global.this.definitions.RootClass, newScope)
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
      patmat                  -> "translate match expressions",
      superAccessors          -> "add super accessors in traits and nested classes",
      extensionMethods        -> "add extension methods for inline classes",
      pickler                 -> "serialize symbol tables",
      refChecks               -> "reference/override checking, translate nested objects",
      uncurry                 -> "uncurry, translate function values to anonymous classes",
      tailCalls               -> "replace tail calls by jumps",
      specializeTypes         -> "@specialized-driven class and method specialization",
      explicitOuter           -> "this refs to outer pointers, translate patterns",
      erasure                 -> "erase types, add interfaces for traits",
      postErasure             -> "clean up erased inline classes",
      lazyVals                -> "allocate bitmaps, translate lazy vals into lazified defs",
      lambdaLift              -> "move nested functions to top level",
      constructors            -> "move field definitions into constructors",
      mixer                   -> "mixin composition",
      cleanup                 -> "platform-specific cleanups, generate reflective calls",
      genicode                -> "generate portable intermediate code",
      inliner                 -> "optimization: do inlining",
      inlineExceptionHandlers -> "optimization: inline exception handlers",
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
  private lazy val phaseTimings = new Phases.TimingModel   // tracking phase stats

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
  /** Summary of the per-phase values of nextFlags and newFlags, shown
   *  with -Xshow-phases if -Ydebug also given.
   */
  def phaseFlagDescriptions: String = {
    val width = phaseNames map (_.length) max
    val fmt   = "%" + width + "s  %2s  %s\n"

    val line1 = fmt.format("phase name", "id", "new flags")
    val line2 = fmt.format("----------", "--", "---------")
    val descs = phaseDescriptors.zipWithIndex map {
      case (ph, idx) =>
        def fstr1 = if (ph.phaseNewFlags == 0L) "" else "[START] " + Flags.flagsToString(ph.phaseNewFlags)
        def fstr2 = if (ph.phaseNextFlags == 0L) "" else "[END] " + Flags.flagsToString(ph.phaseNextFlags)
        val fstr = (
          if (ph.ownPhase.id == 1) Flags.flagsToString(Flags.InitialFlags)
          else if (ph.phaseNewFlags != 0L && ph.phaseNextFlags != 0L) fstr1 + " " + fstr2
          else fstr1 + fstr2
        )
        fmt.format(ph.phaseName, idx + 1, fstr)
    }
    line1 :: line2 :: descs mkString
  }

  /** Returns List of (phase, value) pairs, including only those
   *  where the value compares unequal to the previous phase's value.
   */
  def afterEachPhase[T](op: => T): List[(Phase, T)] = {
    phaseDescriptors.map(_.ownPhase).filterNot(_ eq NoPhase).foldLeft(List[(Phase, T)]()) { (res, ph) =>
      val value = afterPhase(ph)(op)
      if (res.nonEmpty && res.head._2 == value) res
      else ((ph, value)) :: res
    } reverse
  }

  /** Returns List of ChangeAfterPhase objects, encapsulating those
   *  phase transitions where the result of the operation gave a different
   *  list than it had when run during the previous phase.
   */
  def changesAfterEachPhase[T](op: => List[T]): List[ChangeAfterPhase[T]] = {
    val ops = ((NoPhase, Nil)) :: afterEachPhase(op)

    ops sliding 2 map {
      case (_, before) :: (ph, after) :: Nil =>
        val lost   = before filterNot (after contains _)
        val gained = after filterNot (before contains _)
        ChangeAfterPhase(ph, lost, gained)
      case _ => ???
    } toList
  }
  private def numberedPhase(ph: Phase) = "%2d/%s".format(ph.id, ph.name)

  case class ChangeAfterPhase[+T](ph: Phase, lost: List[T], gained: List[T]) {
    private def mkStr(what: String, xs: List[_]) = (
      if (xs.isEmpty) ""
      else xs.mkString(what + " after " + numberedPhase(ph) + " {\n  ", "\n  ", "\n}\n")
    )
    override def toString = mkStr("Lost", lost) + mkStr("Gained", gained)
  }

  def describeAfterEachPhase[T](op: => T): List[String] =
    afterEachPhase(op) map { case (ph, t) => "[after %-15s] %s".format(numberedPhase(ph), t) }

  def describeAfterEveryPhase[T](op: => T): String =
    describeAfterEachPhase(op) map ("  " + _ + "\n") mkString

  def printAfterEachPhase[T](op: => T): Unit =
    describeAfterEachPhase(op) foreach (m => println("  " + m))

  // ----------- Runs ---------------------------------------

  private var curRun: Run = null
  private var curRunId = 0

  /** A hook that lets subclasses of `Global` define whether a package or class should be kept loaded for the
   *  next compiler run. If the parameter `sym` is a class or object, and `clearOnNextRun(sym)` returns `true`,
   *  then the symbol is unloaded and reset to its state before the last compiler run. If the parameter `sym` is
   *  a package, and clearOnNextRun(sym)` returns `true`, the package is recursively searched for
   *  classes to drop.
   *
   *  Example: Let's say I want a compiler that drops all classes corresponding to the current project
   *  between runs. Then `keepForNextRun` of a toplevel class or object should return `true` if the
   *  class or object does not form part of the current project, `false` otherwise. For a package,
   *  clearOnNextRun should return `true` if no class in that package forms part of the current project,
   *  `false` otherwise.
   *
   *  @param    sym A class symbol, object symbol, package, or package class.
   */
  def clearOnNextRun(sym: Symbol) = false
    /* To try out clearOnNext run on the scala.tools.nsc project itself
     * replace `false` above with the following code

    settings.Xexperimental.value && { sym.isRoot || {
      sym.fullName match {
        case "scala" | "scala.tools" | "scala.tools.nsc" => true
        case _ => sym.owner.fullName.startsWith("scala.tools.nsc")
      }
    }}

     * Then, fsc -Xexperimental clears the nsc porject between successive runs of `fsc`.
     */

  /** Remove the current run when not needed anymore. Used by the build
   *  manager to save on the memory foot print. The current run holds on
   *  to all compilation units, which in turn hold on to trees.
   */
  private [nsc] def dropRun() {
    curRun = null
  }

  object typeDeconstruct extends {
    val global: Global.this.type = Global.this
  } with interpreter.StructuredTypeStrings

  /** There are common error conditions where when the exception hits
   *  here, currentRun.currentUnit is null.  This robs us of the knowledge
   *  of what file was being compiled when it broke.  Since I really
   *  really want to know, this hack.
   */
  private var lastSeenSourceFile: SourceFile = NoSourceFile

  /** Let's share a lot more about why we crash all over the place.
   *  People will be very grateful.
   */
  private var lastSeenContext: analyzer.Context = null

  /** The currently active run
   */
  def currentRun: Run              = curRun
  def currentUnit: CompilationUnit = if (currentRun eq null) NoCompilationUnit else currentRun.currentUnit
  def currentSource: SourceFile    = if (currentUnit.exists) currentUnit.source else lastSeenSourceFile

  // TODO - trim these to the absolute minimum.
  @inline final def afterErasure[T](op: => T): T        = afterPhase(currentRun.erasurePhase)(op)
  @inline final def afterExplicitOuter[T](op: => T): T  = afterPhase(currentRun.explicitouterPhase)(op)
  @inline final def afterFlatten[T](op: => T): T        = afterPhase(currentRun.flattenPhase)(op)
  @inline final def afterIcode[T](op: => T): T          = afterPhase(currentRun.icodePhase)(op)
  @inline final def afterMixin[T](op: => T): T          = afterPhase(currentRun.mixinPhase)(op)
  @inline final def afterPickler[T](op: => T): T        = afterPhase(currentRun.picklerPhase)(op)
  @inline final def afterRefchecks[T](op: => T): T      = afterPhase(currentRun.refchecksPhase)(op)
  @inline final def afterSpecialize[T](op: => T): T     = afterPhase(currentRun.specializePhase)(op)
  @inline final def afterTyper[T](op: => T): T          = afterPhase(currentRun.typerPhase)(op)
  @inline final def afterUncurry[T](op: => T): T        = afterPhase(currentRun.uncurryPhase)(op)
  @inline final def beforeErasure[T](op: => T): T       = beforePhase(currentRun.erasurePhase)(op)
  @inline final def beforeExplicitOuter[T](op: => T): T = beforePhase(currentRun.explicitouterPhase)(op)
  @inline final def beforeFlatten[T](op: => T): T       = beforePhase(currentRun.flattenPhase)(op)
  @inline final def beforeIcode[T](op: => T): T         = beforePhase(currentRun.icodePhase)(op)
  @inline final def beforeMixin[T](op: => T): T         = beforePhase(currentRun.mixinPhase)(op)
  @inline final def beforePickler[T](op: => T): T       = beforePhase(currentRun.picklerPhase)(op)
  @inline final def beforeRefchecks[T](op: => T): T     = beforePhase(currentRun.refchecksPhase)(op)
  @inline final def beforeSpecialize[T](op: => T): T    = beforePhase(currentRun.specializePhase)(op)
  @inline final def beforeTyper[T](op: => T): T         = beforePhase(currentRun.typerPhase)(op)
  @inline final def beforeUncurry[T](op: => T): T       = beforePhase(currentRun.uncurryPhase)(op)

  def explainContext(c: analyzer.Context): String = (
    if (c == null) "" else (
     """| context owners: %s
        |
        |Enclosing block or template:
        |%s""".format(
          c.owner.ownerChain.takeWhile(!_.isPackageClass).mkString(" -> "),
          nodePrinters.nodeToString(c.enclClassOrMethod.tree)
        )
    )
  )
  // Owners up to and including the first package class.
  private def ownerChainString(sym: Symbol): String = (
    if (sym == null) ""
    else sym.ownerChain.span(!_.isPackageClass) match {
      case (xs, pkg :: _) => (xs :+ pkg) mkString " -> "
      case _              => sym.ownerChain mkString " -> " // unlikely
    }
  )
  private def formatExplain(pairs: (String, Any)*): String = (
    pairs.toList collect { case (k, v) if v != null => "%20s: %s".format(k, v) } mkString "\n"
  )

  def explainTree(t: Tree): String = formatExplain(
  )

  /** Don't want to introduce new errors trying to report errors,
   *  so swallow exceptions.
   */
  override def supplementErrorMessage(errorMessage: String): String = try {
    val tree      = analyzer.lastTreeToTyper
    val sym       = tree.symbol
    val tpe       = tree.tpe
    val enclosing = lastSeenContext.enclClassOrMethod.tree

    val info1 = formatExplain(
      "while compiling"    -> currentSource.path,
      "during phase"       -> ( if (globalPhase eq phase) phase else "global=%s, atPhase=%s".format(globalPhase, phase) ),
      "library version"    -> scala.util.Properties.versionString,
      "compiler version"   -> Properties.versionString,
      "reconstructed args" -> settings.recreateArgs.mkString(" ")
    )
    val info2 = formatExplain(
      "last tree to typer" -> tree.summaryString,
      "symbol"             -> Option(sym).fold("null")(_.debugLocationString),
      "symbol definition"  -> Option(sym).fold("null")(_.defString),
      "tpe"                -> tpe,
      "symbol owners"      -> ownerChainString(sym),
      "context owners"     -> ownerChainString(lastSeenContext.owner)
    )
    val info3: List[String] = (
         ( List("== Enclosing template or block ==", nodePrinters.nodeToString(enclosing).trim) )
      ++ ( if (tpe eq null) Nil else List("== Expanded type of tree ==", typeDeconstruct.show(tpe)) )
      ++ ( if (!opt.debug) Nil else List("== Current unit body ==", nodePrinters.nodeToString(currentUnit.body)) )
      ++ ( List(errorMessage) )
    )

    ("\n" + info1) :: info2 :: info3 mkString "\n\n"
  }
  catch { case x: Exception => errorMessage }

  /** The id of the currently active run
   */
  override def currentRunId = curRunId

  def echoPhaseSummary(ph: Phase) = {
    /** Only output a summary message under debug if we aren't echoing each file. */
    if (opt.debug && !opt.echoFilenames)
      inform("[running phase " + ph.name + " on " + currentRun.size +  " compilation units]")
  }

  /** Collects for certain classes of warnings during this run. */
  class ConditionalWarning(what: String, option: Settings#BooleanSetting) {
    val warnings = new mutable.ListBuffer[(Position, String)]
    def warn(pos: Position, msg: String) =
      if (option.value) reporter.warning(pos, msg)
      else warnings += ((pos, msg))
    def summarize() =
      if (option.isDefault && warnings.nonEmpty)
        reporter.warning(NoPosition, "there were %d %s warnings; re-run with %s for details".format(warnings.size, what, option.name))
  }

  def newUnitParser(code: String)      = new syntaxAnalyzer.UnitParser(newCompilationUnit(code))
  def newUnitScanner(code: String)     = new syntaxAnalyzer.UnitScanner(newCompilationUnit(code))
  def newCompilationUnit(code: String) = new CompilationUnit(newSourceFile(code))
  def newSourceFile(code: String)      = new BatchSourceFile("<console>", code)

  /** A Run is a single execution of the compiler on a sets of units
   */
  class Run {
    /** Have been running into too many init order issues with Run
     *  during erroneous conditions.  Moved all these vals up to the
     *  top of the file so at least they're not trivially null.
     */
    var isDefined = false
    /** The currently compiled unit; set from GlobalPhase */
    var currentUnit: CompilationUnit = NoCompilationUnit

    // This change broke sbt; I gave it the thrilling name of uncheckedWarnings0 so
    // as to recover uncheckedWarnings for its ever-fragile compiler interface.
    val deprecationWarnings0 = new ConditionalWarning("deprecation", settings.deprecation)
    val uncheckedWarnings0 = new ConditionalWarning("unchecked", settings.unchecked)
    val featureWarnings = new ConditionalWarning("feature", settings.feature)
    val inlinerWarnings = new ConditionalWarning("inliner", settings.YinlinerWarnings)
    val allConditionalWarnings = List(deprecationWarnings0, uncheckedWarnings0, featureWarnings, inlinerWarnings)

    // for sbt's benefit
    def uncheckedWarnings: List[(Position, String)] = uncheckedWarnings0.warnings.toList
    def deprecationWarnings: List[(Position, String)] = deprecationWarnings0.warnings.toList

    var reportedFeature = Set[Symbol]()

    /** A flag whether macro expansions failed */
    var macroExpansionFailed = false

    /** To be initialized from firstPhase. */
    private var terminalPhase: Phase = NoPhase

    private val unitbuf = new mutable.ListBuffer[CompilationUnit]
    val compiledFiles   = new mutable.HashSet[String]

    /** A map from compiled top-level symbols to their source files */
    val symSource = new mutable.HashMap[Symbol, AbstractFile]

    /** A map from compiled top-level symbols to their picklers */
    val symData = new mutable.HashMap[Symbol, PickleBuffer]

    private var phasec: Int       = 0   // phases completed
    private var unitc: Int        = 0   // units completed this phase
    private var _unitbufSize = 0

    def size = _unitbufSize
    override def toString = "scalac Run for:\n  " + compiledFiles.toList.sorted.mkString("\n  ")

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
      definitions.init()

      // Flush the cache in the terminal phase: the chain could have been built
      // before without being used. (This happens in the interpreter.)
      terminal.reset

      // Each subcomponent supplies a phase, which are chained together.
      //   If -Ystop:phase is given, neither that phase nor any beyond it is added.
      //   If -Yskip:phase is given, that phase will be skipped.
      val phaseLinks = {
        val phs = (
          phaseDescriptors.tail
            takeWhile (pd => !stopPhase(pd.phaseName))
            filterNot (pd =>  skipPhase(pd.phaseName))
        )
        // Ensure there is a terminal phase at the end, since -Ystop may have limited the phases.
        if (phs.isEmpty || (phs.last ne terminal)) phs :+ terminal
        else phs
      }
      // Link them together.
      phaseLinks.foldLeft(parserPhase)((chain, ph) => ph newPhase chain)
      parserPhase
    }

    /** Reset all classes contained in current project, as determined by
     *  the clearOnNextRun hook
     */
    def resetProjectClasses(root: Symbol): Unit = try {
      def unlink(sym: Symbol) =
        if (sym != NoSymbol) root.info.decls.unlink(sym)
      if (settings.verbose.value) inform("[reset] recursing in "+root)
      val toReload = mutable.Set[String]()
      for (sym <- root.info.decls) {
        if (sym.isInitialized && clearOnNextRun(sym))
          if (sym.isPackage) {
            resetProjectClasses(sym.moduleClass)
            openPackageModule(sym.moduleClass)
          } else {
            unlink(sym)
            unlink(root.info.decls.lookup(
              if (sym.isTerm) sym.name.toTypeName else sym.name.toTermName))
            toReload += sym.fullName
              // note: toReload could be set twice with the same name
              // but reinit must happen only once per name. That's why
              // the following classPath.findClass { ... } code cannot be moved here.
          }
      }
      for (fullname <- toReload)
        classPath.findClass(fullname) match {
          case Some(classRep) =>
            if (settings.verbose.value) inform("[reset] reinit "+fullname)
            loaders.initializeFromClassPath(root, classRep)
          case _ =>
        }
    } catch {
      case ex: Throwable =>
        // this handler should not be nessasary, but it seems that `fsc`
        // eats exceptions if they appear here. Need to find out the cause for
        // this and fix it.
        inform("[reset] exception happened: "+ex);
        ex.printStackTrace();
        throw ex
    }

    // --------------- Miscellania -------------------------------

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

    private def currentProgress   = (phasec * size) + unitc
    private def totalProgress     = (phaseDescriptors.size - 1) * size // -1: drops terminal phase
    private def refreshProgress() = if (size > 0) progress(currentProgress, totalProgress)

    // ----- finding phases --------------------------------------------

    def phaseNamed(name: String): Phase =
      findOrElse(firstPhase.iterator)(_.name == name)(NoPhase)

    /** All phases as of 3/2012 here for handiness; the ones in
     *  active use uncommented.
     */
    val parserPhase                  = phaseNamed("parser")
    val namerPhase                   = phaseNamed("namer")
    // val packageobjectsPhase          = phaseNamed("packageobjects")
    val typerPhase                   = phaseNamed("typer")
    val inlineclassesPhase           = phaseNamed("inlineclasses")
    // val superaccessorsPhase          = phaseNamed("superaccessors")
    val picklerPhase                 = phaseNamed("pickler")
    val refchecksPhase               = phaseNamed("refchecks")
    // val selectiveanfPhase            = phaseNamed("selectiveanf")
    // val selectivecpsPhase            = phaseNamed("selectivecps")
    val uncurryPhase                 = phaseNamed("uncurry")
    // val tailcallsPhase               = phaseNamed("tailcalls")
    val specializePhase              = phaseNamed("specialize")
    val explicitouterPhase           = phaseNamed("explicitouter")
    val erasurePhase                 = phaseNamed("erasure")
    // val lazyvalsPhase                = phaseNamed("lazyvals")
    val lambdaliftPhase              = phaseNamed("lambdalift")
    // val constructorsPhase            = phaseNamed("constructors")
    val flattenPhase                 = phaseNamed("flatten")
    val mixinPhase                   = phaseNamed("mixin")
    val cleanupPhase                 = phaseNamed("cleanup")
    val icodePhase                   = phaseNamed("icode")
    // val inlinerPhase                 = phaseNamed("inliner")
    // val inlineExceptionHandlersPhase = phaseNamed("inlineExceptionHandlers")
    // val closelimPhase                = phaseNamed("closelim")
    // val dcePhase                     = phaseNamed("dce")
    val jvmPhase                     = phaseNamed("jvm")

    def runIsAt(ph: Phase)   = globalPhase.id == ph.id
    def runIsPast(ph: Phase) = globalPhase.id > ph.id

    isDefined = true

    // ----------- Units and top-level classes and objects --------


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

    def registerPickle(sym: Symbol): Unit = ()

    /** does this run compile given class, module, or case factory? */
    def compiles(sym: Symbol): Boolean =
      if (sym == NoSymbol) false
      else if (symSource.isDefinedAt(sym)) true
      else if (!sym.owner.isPackageClass) compiles(sym.enclosingTopLevelClass)
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

    // Similarly, this will only be created under -Yshow-syms.
    object trackerFactory extends SymbolTrackers {
      val global: Global.this.type = Global.this
      lazy val trackers = currentRun.units.toList map (x => SymbolTracker(x))
      def snapshot() = {
        inform("\n[[symbol layout at end of " + phase + "]]")
        afterPhase(phase) {
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
        allConditionalWarnings foreach (_.summarize)

        if (macroExpansionFailed)
          warning("some macros could not be expanded and code fell back to overridden methods;"+
                  "\nrecompiling with generated classfiles on the classpath might help.")
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
        checkDeprecatedSettings(newCompilationUnit(""))
        reportCompileErrors()
        return
      }

      compileUnits(sources map (new CompilationUnit(_)), firstPhase)
    }

    /** Compile list of units, starting with phase `fromPhase`
     */
    def compileUnits(units: List[CompilationUnit], fromPhase: Phase) {
      try compileUnitsInternal(units, fromPhase)
      catch { case ex =>
        globalError(supplementErrorMessage("uncaught exception during compilation: " + ex.getClass.getName))
        throw ex
      }
    }

    private def compileUnitsInternal(units: List[CompilationUnit], fromPhase: Phase) {
      units foreach addUnit
      val startTime = currentTime

      reporter.reset()
      checkDeprecatedSettings(unitbuf.head)
      globalPhase = fromPhase

     while (globalPhase.hasNext && !reporter.hasErrors) {
        val startTime = currentTime
        phase = globalPhase
        globalPhase.run

        // progress update
        informTime(globalPhase.description, startTime)
        phaseTimings(globalPhase) = currentTime - startTime

        // write icode to *.icode files
        if (opt.writeICode)
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

      if (traceSymbolActivity)
        units map (_.body) foreach (traceSymbols recordSymbolsInTree _)

      // In case no phase was specified for -Xshow-class/object, show it now for sure.
      if (opt.noShow)
        showMembers()

      reportCompileErrors()
      symSource.keys foreach (x => resetPackageClass(x.owner))
      informTime("total", startTime)

      // record dependency data
      if (!dependencyAnalysis.off)
        dependencyAnalysis.saveDependencyAnalysis()

      // Clear any sets or maps created via perRunCaches.
      perRunCaches.clearAll()

      // Reset project
      if (!stopPhase("namer")) {
        atPhase(namerPhase) {
          resetProjectClasses(definitions.RootClass)
        }
      }
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
      val maxId = math.max(globalPhase.id, typerPhase.id)
      addUnit(unit)

      firstPhase.iterator takeWhile (_.id < maxId) foreach (ph =>
        atPhase(ph)(ph.asInstanceOf[GlobalPhase] applyPhase unit)
      )
      refreshProgress
    }

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
     *  1. This Space Intentionally Left Blank
     *  2. LowPriorityImplicits / EmbeddedControls (i.e. parents of Predef)
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
          case "LowPriorityImplicits.scala"   => 2
          case "StandardEmbeddings.scala"     => 2
          case "EmbeddedControls.scala"       => 2
          case "Predef.scala"                 => 3 /* Predef.scala before Any.scala, etc. */
          case _                              => goLast
        }
      }
      files sortBy rank
    }
  } // class Run

  def printAllUnits() {
    print("[[syntax trees at end of %25s]]".format(phase))
    afterPhase(phase)(currentRun.units foreach { unit =>
      nodePrinters showUnit unit
    })
  }

  /** We resolve the class/object ambiguity by passing a type/term name.
   */
  def showDef(fullName: Name, declsOnly: Boolean, ph: Phase) = {
    val boringOwners = Set[Symbol](definitions.AnyClass, definitions.AnyRefClass, definitions.ObjectClass)
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

  def getFile(source: AbstractFile, segments: Array[String], suffix: String): File = {
    val outDir = Path(
      settings.outputDirs.outputDirFor(source).path match {
        case ""   => "."
        case path => path
      }
    )
    val dir      = segments.init.foldLeft(outDir)(_ / _).createDirectory()
    new File(dir.path, segments.last + suffix)
  }

  /** Returns the file with the given suffix for the given class. Used for icode writing. */
  def getFile(clazz: Symbol, suffix: String): File = getFile(clazz.sourceFile, clazz.fullName split '.', suffix)

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
  override def forMSIL = opt.msil
  def forInteractive   = onlyPresentation
  def forScaladoc      = onlyPresentation
  def createJavadoc    = false

  @deprecated("Use forInteractive or forScaladoc, depending on what you're after", "2.9.0")
  def onlyPresentation = false
}

object Global {
  def apply(settings: Settings, reporter: Reporter): Global = new Global(settings, reporter)
}
