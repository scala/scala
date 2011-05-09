/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.{ File, FileOutputStream, PrintWriter, IOException, FileNotFoundException }
import java.nio.charset.{ Charset, IllegalCharsetNameException, UnsupportedCharsetException }
import compat.Platform.currentTime

import io.{ SourceReader, AbstractFile, Path }
import reporters.{ Reporter, ConsoleReporter }
import util.{ ClassPath, SourceFile, Statistics, BatchSourceFile, ScriptSourceFile, returning }
import collection.mutable.{ HashSet, HashMap, ListBuffer }
import reflect.generic.{ PickleBuffer }

import symtab.{ Flags, SymbolTable, SymbolLoaders }
import symtab.classfile.Pickler
import dependencies.DependencyAnalysis
import plugins.Plugins
import ast._
import ast.parser._
import typechecker._
import transform._

import backend.icode.{ ICodes, GenICode, Checkers }
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
    this(new Settings(err => reporter.error(null,err)),
         reporter)

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

  /** Print tree in detailed form */
  object nodePrinters extends {
    val global: Global.this.type = Global.this
  } with NodePrinters {
    infolevel = InfoLevel.Verbose
  }
  val nodeToString = nodePrinters.nodeToString

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

  /** Tree checker (used for testing and debugging) */
  object checker extends {
    val global: Global.this.type = Global.this
  } with TreeCheckers

  /** ICode generator */
  object icodes extends {
    val global: Global.this.type = Global.this
  } with ICodes

  /** ICode analysis for optimization */
  object analysis extends {
    val global: Global.this.type = Global.this
  } with TypeFlowAnalysis

  /** Copy propagation for optimization */
  object copyPropagation extends {
    val global: Global.this.type = Global.this
  } with CopyPropagation

  /** Icode verification */
  object checkers extends {
    val global: Global.this.type = Global.this
  } with Checkers

  /** Some statistics (normally disabled) */
  object statistics extends {
    val global: Global.this.type = Global.this
  } with Statistics

  util.Statistics.enabled = settings.Ystatistics.value

  /** Computing pairs of overriding/overridden symbols */
  object overridingPairs extends {
    val global: Global.this.type = Global.this
  } with OverridingPairs

  /** Representing ASTs as graphs */
  object treeBrowsers extends {
    val global: Global.this.type = Global.this
  } with TreeBrowsers

  val treeBrowser = treeBrowsers.create()

  // ------------ Hooks for interactive mode-------------------------

  /** Called from parser, which signals hereby that a method definition has been parsed.
   */
  def signalParseProgress(pos: Position) {}

  /** Called every time an AST node is successfully typechecked in typerPhase.
   */
  def signalDone(context: analyzer.Context, old: Tree, result: Tree) {}

  /** Register new context; called for every created context
   */
  def registerContext(c: analyzer.Context) {}

  /** Register top level class (called on entering the class)
   */
  def registerTopLevelSym(sym: Symbol) {}

// ------------------ Reporting -------------------------------------

  def error(msg: String) = reporter.error(NoPosition, msg)
  def warning(msg: String) =
    if (settings.Xwarnfatal.value) reporter.error(NoPosition, msg)
    else reporter.warning(NoPosition, msg)
  def inform(msg: String) = reporter.info(NoPosition, msg, true)
  def inform[T](msg: String, value: T): T = { inform(msg+value); value }

  //reporter.info(null, msg, true)

  def informProgress(msg: String) =
    if (settings.verbose.value) inform("[" + msg + "]")

  def informTime(msg: String, start: Long) =
    informProgress(msg + " in " + (currentTime - start) + "ms")

  def log(msg: AnyRef) {
    if (settings.log contains phase.name) inform("[log " + phase + "] " + msg)
  }

  class ThrowableWithPosition(val pos: Int, val error: Throwable) extends Throwable

  def tryWith[T](pos: Int, body: => T): T =
    try body
    catch {
      case e : ThrowableWithPosition  => throw e
      case te: TypeError              => throw te
      case e : RuntimeException       => throw new ThrowableWithPosition(pos, e)
    }

  def catchWith[T](source : SourceFile, body : => T) : T =
    try body
    catch {
      case e : ThrowableWithPosition =>
        logError("POS: " + source.dbg(e.pos), e)
        throw e.error
    }

  def logError(msg: String, t: Throwable): Unit = ()

// ------------ File interface -----------------------------------------

  private val reader: SourceReader = {
    def stdCharset: Charset = {
      settings.encoding.value = Properties.sourceEncoding // A mandatory charset
      Charset.forName(settings.encoding.value)
    }
    val charset =
      try {
        Charset.forName(settings.encoding.value)
      } catch {
        case _: IllegalCharsetNameException =>
          error("illegal charset name '" + settings.encoding.value + "'")
          stdCharset
        case _: UnsupportedCharsetException =>
          error("unsupported charset '" + settings.encoding.value + "'")
          stdCharset
      }
    try {
      val clazz = Class.forName(settings.sourceReader.value)
      val ccon  = clazz.getConstructor(classOf[java.nio.charset.CharsetDecoder], classOf[Reporter])
      ccon.newInstance(charset.newDecoder(), reporter).asInstanceOf[SourceReader]
      //new SourceReader(charset.newDecoder())
    } catch {
      case e =>
        error("exception while trying to instantiate source reader \""+settings.sourceReader.value+"\" ");
        new SourceReader(charset.newDecoder(), reporter)
    }
  }

  if (settings.make.value != "all")
    settings.dependenciesFile.value match {
      case "none" => ()
      case x =>
        val depFilePath = Path(x)
        if (depFilePath.exists) {
          /** The directory where file lookup should start */
          val rootPath = depFilePath.parent
          def toFile(path: String) = AbstractFile.getFile(rootPath resolve Path(path))
          dependencyAnalysis.loadFrom(AbstractFile.getFile(depFilePath), toFile)
        }
    }

  if (settings.verbose.value || settings.Ylogcp.value)
    inform("[Classpath = " + classPath.asClasspathString + "]")

  /** True if -Xscript has been set, indicating a script run.
   */
  def isScriptRun = settings.script.value != ""

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

  val phaseWithId = new Array[Phase](MaxPhases)
  for (i <- List.range(0, MaxPhases)) phaseWithId(i) = NoPhase

  abstract class GlobalPhase(prev: Phase) extends Phase(prev) {
    phaseWithId(id) = this
    def run { currentRun.units foreach applyPhase }

    def apply(unit: CompilationUnit): Unit

    private val isErased = prev.name == "erasure" || prev.erasedTypes
    override def erasedTypes: Boolean = isErased
    private val isFlat = prev.name == "flatten" || prev.flatClasses
    override def flatClasses: Boolean = isFlat
    private val isDevirtualized = prev.name == "devirtualize" || prev.devirtualized
    override def devirtualized: Boolean = isDevirtualized  // (part of DEVIRTUALIZE)
    private val isSpecialized = prev.name == "specialize" || prev.specialized
    override def specialized: Boolean = isSpecialized

    /** Is current phase cancelled on this unit? */
    def cancelled(unit: CompilationUnit) = {
      // run the typer only if in `createJavadoc` mode
      val maxJavaPhase = if (createJavadoc) currentRun.typerPhase.id else currentRun.namerPhase.id
      reporter.cancelled || unit.isJava && this.id > maxJavaPhase
    }

    final def applyPhase(unit: CompilationUnit) {
      if (settings.debug.value) inform("[running phase " + name + " on " + unit + "]")
      val unit0 = currentRun.currentUnit
      try {
        currentRun.currentUnit = unit
        if (!cancelled(unit))
          reporter.withSource(unit.source) { apply(unit) }
        currentRun.advanceUnit
      } finally {
        //assert(currentRun.currentUnit == unit)
        currentRun.currentUnit = unit0
      }
    }
  }

  // phaseName = "parser"
  object syntaxAnalyzer extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]()
    val runsRightAfter = None
  } with SyntaxAnalyzer

  // factory method for
  // phaseName = "namer"
  // phaseName = "parser"
  object analyzer extends {
    val global: Global.this.type = Global.this
  } with Analyzer

  /** Switch to turn on detailed type logs */
  var printTypings = settings.Ytyperdebug.value

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

//  object devirtualize extends {
//    val global: Global.this.type = Global.this
//  } with DeVirtualize

  // phaseName = "liftcode"
  object liftcode extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("refchecks")
    val runsRightAfter = None
  } with LiftCode

  // phaseName = "uncurry"
  object uncurry extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("refchecks","liftcode")
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
    val global: Global.this.type = Global.this
    final val FLAGS_PER_WORD = 32
    val runsAfter = List[String]("erasure")
    val runsRightAfter = None
  } with LazyVals

  // phaseName = "lambdalift"
  object lambdaLift extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("lazyvals")
    val runsRightAfter = None
  } with LambdaLift

  // phaseName = "detach"
//  object detach extends {
//    val global: Global.this.type = Global.this
//    val runsAfter = List("lambdalift")
//    val runsRightAfter = Some("lambdalift")
//  } with Detach

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
    val runsAfter = List[String]("flatten","constructors")
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

// object icodePrinter extends backend.icode.Printers {
//   val global: Global.this.type = Global.this
// }

  // phaseName = "???"
  object scalaPrimitives extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]()
    val runsRightAfter = None
  } with ScalaPrimitives

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

  object dependencyAnalysis extends {
    val global: Global.this.type = Global.this
    val runsAfter = List("jvm")
    val runsRightAfter = None
  } with DependencyAnalysis

  // phaseName = "terminal"
  object terminal extends {
    val global: Global.this.type = Global.this
    val phaseName = "terminal"
    val runsAfter = List[String]("jvm","msil")
    val runsRightAfter = None
  } with SubComponent {
    private var cache: Option[GlobalPhase] = None

    def newPhase(prev: Phase): GlobalPhase = {
      if (cache.isEmpty) cache = Some(new TerminalPhase(prev))
      cache.get
    }

    def reset() {
      cache = None
    }

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

  object icodeChecker extends checkers.ICodeChecker()

  object typer extends analyzer.Typer(
    analyzer.NoContext.make(EmptyTree, Global.this.definitions.RootClass, new Scope))

  /* Add the internal compiler phases to the phases set
   */
  protected def computeInternalPhases() {
    phasesSet += syntaxAnalyzer             // The parser
    phasesSet += analyzer.namerFactory      //   note: types are there because otherwise
    phasesSet += analyzer.packageObjects    //   consistency check after refchecks would fail.
    phasesSet += analyzer.typerFactory
    phasesSet += superAccessors			       // add super accessors
    phasesSet += pickler			       // serialize symbol tables
    phasesSet += refchecks			       // perform reference and override checking, translate nested objects

//    if (false && settings.YvirtClasses)
//	phasesSet += devirtualize		       // Desugar virtual classes4

    phasesSet += uncurry                    // uncurry, translate function values to anonymous classes
    phasesSet += tailCalls                  // replace tail calls by jumps
    phasesSet += specializeTypes
    phasesSet += explicitOuter              // replace C.this by explicit outer pointers, eliminate pattern matching
    phasesSet += erasure                    // erase types, add interfaces for traits
    phasesSet += lazyVals
    phasesSet += lambdaLift                 // move nested functions to top level
    // if (forJVM && settings.Xdetach.value)
    //   phasesSet += detach                // convert detached closures

    phasesSet += constructors               // move field definitions into constructors
    phasesSet += mixer                      // do mixin composition
    phasesSet += cleanup                    // some platform-specific cleanups
    phasesSet += genicode                   // generate portable intermediate code
    phasesSet += inliner                    // optimization: do inlining
    phasesSet += closureElimination         // optimization: get rid of uncalled closures
    phasesSet += deadCode                   // optimization: get rid of dead cpde
    phasesSet += terminal                   // The last phase in the compiler chain
  }

  protected def computePlatformPhases() = platform.platformPhases foreach (phasesSet += _)

  /* Helper method for sequncing the phase assembly
   */
  private def computePhaseDescriptors: List[SubComponent] = {
    computeInternalPhases()       // Global.scala
    computePlatformPhases()       // backend/Platform.scala
    computePluginPhases()         // plugins/Plugins.scala
    buildCompilerFromPhasesSet()  // PhaseAssembly.scala
  }

  /* The phase descriptor list */
  lazy val phaseDescriptors: List[SubComponent] = computePhaseDescriptors

  /* The set of phase objects that is the basis for the compiler phase chain */
  protected val phasesSet : HashSet[SubComponent] = new HashSet[SubComponent]

  /** The names of the phases. */
  lazy val phaseNames = {
    new Run // force some initialization
    phaseDescriptors map (_.phaseName)
  }

  /** A description of the phases that will run */
  def phaseDescriptions: String =
    phaseNames mkString "\n" // todo: + " - " + phase.description

  // ----------- Runs ---------------------------------------

  /** Remove the current run when not needed anymore. Used by the build
   *  manager to save on the memory foot print. The current run holds on
   *  to all compilation units, which in turn hold on to trees.
   */
  private [nsc] def dropRun() {
    curRun = null
  }

  private var curRun: Run = null
  private var curRunId = 0

  /** The currently active run
   */
  def currentRun: Run = curRun

  /** The id of the currently active run
   */
  override def currentRunId = curRunId

  /** A Run is a single execution of the compiler on a sets of units
   */
  class Run {

    var isDefined = false

    private val firstPhase = {
      // ----------- Initialization code -------------------------
      curRunId += 1
      assert(curRunId > 0)
      curRun = this
      //Console.println("starting run: " + id)

      // Can not take the phaseDescriptors.head even though its the syntaxAnalyzer, this will implicitly
      // call definitions.init which uses phase and needs it to be != NoPhase
      val phase1 = syntaxAnalyzer.newPhase(NoPhase)
      phase = phase1
      definitions.init  // needs phase to be defined != NoPhase,
                        // that's why it is placed here.

      // The first phase in the compiler phase chain
      var p: Phase = phase1

      // Reset the cache in terminal, the chain could have been build before where nobody used it
      // This happens in the interpreter
      terminal.reset

      // Each subcomponent is asked to deliver a newPhase that is chained together. If -Ystop:phasename is
      // given at command-line, this will stop with that phasename
      for (pd <- phaseDescriptors.tail.takeWhile(pd => !(stopPhase(pd.phaseName))))
        if (!(settings.skip contains pd.phaseName)) p = pd.newPhase(p)

      // Ensure there is a terminal phase at the end, Normally there will then be two terminal phases at the end
      // if -Ystop:phasename was given, this makes sure that there is a terminal phase at the end
      p = terminal.newPhase(p)

      phase1
    }

    // --------------- Miscellania -------------------------------

    /** The currently compiled unit; set from GlobalPhase */
    var currentUnit: CompilationUnit = _

    /** Flags indicating whether deprecation warnings occurred */
    var deprecationWarnings: Boolean = false
    var uncheckedWarnings: Boolean = false

    def cancel { reporter.cancelled = true }

    // ------------------ Progress tracking -------------------------

    def progress(current: Int, total: Int) {}

    private var phasec: Int = 0
    private var unitc: Int = 0

    /** take note that phase is completed
     *  (for progress reporting)
     */
    def advancePhase {
      unitc = 0
      phasec += 1
      refreshProgress
    }
    /** take note that a phase on a unit is completed
     *  (for progress reporting)
     */
    def advanceUnit {
      unitc += 1
      refreshProgress
    }
    private def refreshProgress =
      if (compiledFiles.size > 0)
        progress((phasec * compiledFiles.size) + unitc,
                 (phaseDescriptors.length-1) * compiledFiles.size) // terminal phase not part of the progress display

    // ----- finding phases --------------------------------------------

    def phaseNamed(name: String): Phase = {
      var p: Phase = firstPhase
      while (p.next != p && p.name != name) p = p.next
      if (p.name != name) NoPhase else p
    }

    val namerPhase = phaseNamed("namer")
    val typerPhase = phaseNamed("typer")
    val picklerPhase = phaseNamed("pickler")
    val refchecksPhase = phaseNamed("refchecks")
    val uncurryPhase = phaseNamed("uncurry")

    val explicitOuterPhase = phaseNamed("explicitouter")
    val erasurePhase = phaseNamed("erasure")
    val flattenPhase = phaseNamed("flatten")
    val mixinPhase = phaseNamed("mixin")
    val icodePhase = phaseNamed("icode")

    isDefined = true

    /** A test whether compilation should stop at phase with given name */
    protected def stopPhase(name : String) = settings.stop.contains(name)

    // ----------- Units and top-level classes and objects --------

    private var unitbuf = new ListBuffer[CompilationUnit]
    var compiledFiles = new HashSet[String]

    /** add unit to be compiled in this run */
    private def addUnit(unit: CompilationUnit) {
//      unit.parseSettings()
      unitbuf += unit
      compiledFiles += unit.source.file.path
    }

    /* An iterator returning all the units being compiled in this run */
    def units: Iterator[CompilationUnit] = unitbuf.iterator

    /** A map from compiled top-level symbols to their source files */
    val symSource = new HashMap[Symbol, AbstractFile]

    /** A map from compiled top-level symbols to their picklers */
    val symData = new HashMap[Symbol, PickleBuffer]

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

    /** Compile list of source files */
    def compileSources(_sources: List[SourceFile]) {
      val depSources = dependencyAnalysis.filter(_sources.distinct) // bug #1268, scalac confused by duplicated filenames
      val sources = coreClassesFirst(depSources)
      if (reporter.hasErrors)
        return  // there is a problem already, e.g. a
                // plugin was passed a bad option
      val startTime = currentTime
      reporter.reset
      for (source <- sources) addUnit(new CompilationUnit(source))

      globalPhase = firstPhase
      while (globalPhase != terminal.newPhase(NoPhase) && !reporter.hasErrors) {
        val startTime = currentTime
        phase = globalPhase
        globalPhase.run
        if (settings.Xprint contains globalPhase.name)
          if (settings.writeICode.value && globalPhase.id >= icodePhase.id) writeICode()
          else if (settings.Xshowtrees.value) nodePrinters.printAll()
          else printAllUnits()
        if (settings.printLate.value && globalPhase.name == "cleanup")
          printAllUnits()

        if (settings.browse contains globalPhase.name) treeBrowser.browse(units)
        informTime(globalPhase.description, startTime)
        globalPhase = globalPhase.next

        if (settings.check contains globalPhase.prev.name) {
          if (globalPhase.prev.checkable) {
            phase = globalPhase
            if (globalPhase.id >= icodePhase.id) icodeChecker.checkICodes
            else checker.checkTrees
          }
          else if (!settings.check.doAllPhases) {
            warning("It is not possible to check the result of the "+globalPhase.name+" phase")
          }
        }
        if (settings.Ystatistics.value) statistics.print(phase)
        advancePhase
      }

      //println(narrowCount+" narrowings")

      if (settings.Xshowcls.value != "")
        showDef(newTermName(settings.Xshowcls.value), false)
      if (settings.Xshowobj.value != "")
        showDef(newTermName(settings.Xshowobj.value), true)

      if (reporter.hasErrors) {
        for ((sym, file) <- symSource.iterator) {
          sym.reset(new loaders.SourcefileLoader(file))
          if (sym.isTerm) sym.moduleClass.reset(loaders.moduleClassLoader)
        }
      } else {
        //assert(symData.isEmpty || !settings.stop.value.isEmpty || !settings.skip.value.isEmpty, symData)
        if (deprecationWarnings) {
          warning("there were deprecation warnings; re-run with " + settings.deprecation.name + " for details")
        }
        if (uncheckedWarnings) {
          warning("there were unchecked warnings; re-run with " + settings.unchecked.name + " for details")
        }
      }
      for ((sym, file) <- symSource.iterator) resetPackageClass(sym.owner)
      informTime("total", startTime)

      if (!dependencyAnalysis.off) {
        settings.dependenciesFile.value match {
          case "none" =>
          case x =>
            val depFilePath = Path(x)
            if (!depFilePath.exists)
              dependencyAnalysis.dependenciesFile = AbstractFile.getFile(depFilePath.createFile())

            /** The directory where file lookup should start */
            val rootPath = depFilePath.parent.normalize
            def fromFile(file: AbstractFile): String =
              rootPath.relativize(Path(file.file).normalize).path

            dependencyAnalysis.saveDependencies(fromFile)
        }
      }
    }

    /** Compile list of abstract files */
    def compileFiles(files: List[AbstractFile]) {
      try compileSources(files map getSourceFile)
      catch { case ex: IOException => error(ex.getMessage()) }
    }

    /** Compile list of files given by their names */
    def compile(filenames: List[String]) {
      try {
        val sources: List[SourceFile] =
          if (isScriptRun && filenames.size > 1) returning(Nil)(_ => error("can only compile one script at a time"))
          else filenames map getSourceFile

        compileSources(sources)
      }
      catch { case ex: IOException => error(ex.getMessage()) }
    }

    /** Compile abstract file until `globalPhase`, but at least
     *  to phase "namer".
     */
    def compileLate(file: AbstractFile) {
      if (compiledFiles eq null) {
        val msg = "No class file for " + file +
                  " was found\n(This file cannot be loaded as a source file)"
        inform(msg)
        throw new FatalError(msg)
      } else if (!(compiledFiles contains file.path)) {
        compileLate(new CompilationUnit(getSourceFile(file)))
      }
    }

    /** Compile abstract file until `globalPhase`, but at least
     *  to phase "namer".
     */
    def compileLate(unit: CompilationUnit) {
      addUnit(unit)
      var localPhase = firstPhase.asInstanceOf[GlobalPhase]
      while (localPhase != null && (localPhase.id  < globalPhase.id || localPhase.id < typerPhase.id)/* && !reporter.hasErrors*/) {
        val oldSource = reporter.getSource
        reporter.withSource(unit.source) {
          atPhase(localPhase)(localPhase.applyPhase(unit))
        }
        val newLocalPhase = localPhase.next.asInstanceOf[GlobalPhase]
        localPhase = if (localPhase == newLocalPhase) null else newLocalPhase
      }
      refreshProgress
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
     * for its parents (e.g. "LowPriorityImplicits"). typedIdent checks wethter
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
      def inScalaFolder(f: SourceFile) =
        f.file.container.name == "scala"
      var scalaObject: Option[SourceFile] = None
      val res = new ListBuffer[SourceFile]
      for (file <- files) file.file.name match {
        case "ScalaObject.scala" if inScalaFolder(file) => scalaObject = Some(file)
        case "LowPriorityImplicits.scala" if inScalaFolder(file) => file +=: res
        case "StandardEmbeddings.scala" if inScalaFolder(file) => file +=: res
        case _ => res += file
      }
      for (so <- scalaObject) so +=: res
      res.toList
    }
  } // class Run

  def printAllUnits() {
    print("[[syntax trees at end of " + phase + "]]")
    atPhase(phase.next) {
      for (unit <- currentRun.units) treePrinter.print(unit)
    }
  }

  def showDef(name: Name, module: Boolean) {
    def getSym(name: Name, module: Boolean): Symbol = {
      var i = name.length - 1
      while (i != 0 && name(i) != '#' && name(i) != '.') i -= 1
      if (i == 0)
        definitions.getModule(name)
      else {
        val root = getSym(name.subName(0, i), name(i) == '.')
        var selector = name.subName(i+1, name.length)
        if (module) selector = selector.toTypeName
        root.info.member(selector)
      }
    }
    val sym = getSym(name, module)
    inform("" + sym.name + ":" +(if (module) sym.tpe.typeSymbol.info else sym.info))
  }

  /** Returns the file with the given suffix for the given class. Used for icode writing. */
  def getFile(clazz: Symbol, suffix: String): File = {
    val outdirname = settings.outputDirs.outputDirFor(clazz.sourceFile)
    var outdir = new File(if (outdirname.path == "") "." else outdirname.path)
    val filename = clazz.fullName
    var start = 0
    var end = filename.indexOf('.', start)
    while (end >= start) {
      outdir = new File(outdir, filename.substring(start, end))
      if (!outdir.exists()) outdir.mkdir()
      start = end + 1
      end = filename.indexOf('.', start)
    }
    new File(outdir, filename.substring(start) + suffix)
  }

  private def writeICode() {
    val printer = new icodes.TextPrinter(null, icodes.linearizer)
    icodes.classes.values.foreach((cls) => {
      val suffix = if (cls.symbol hasFlag Flags.MODULE) "$.icode" else ".icode"
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
          if (settings.debug.value) ex.printStackTrace()
        error("could not write file " + file)
      }
    })
  }

  def forJVM : Boolean = settings.target.value startsWith "jvm"
  def forMSIL: Boolean = settings.target.value == "msil"
  def forInteractive   = onlyPresentation
  def forScaladoc      = onlyPresentation
  @deprecated("Use forInteractive or forScaladoc, depending on what you're after")
  def onlyPresentation = false
  def createJavadoc = false
}
