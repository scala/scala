/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.{File, FileOutputStream, PrintWriter}
import java.io.{IOException, FileNotFoundException}
import java.nio.charset._
import compat.Platform.currentTime
import scala.tools.nsc.io.{SourceReader, AbstractFile}
import scala.tools.nsc.reporters._
import scala.tools.nsc.util.{ClassPath, SourceFile, BatchSourceFile, OffsetPosition}

import scala.collection.mutable.{HashSet, HashMap, ListBuffer}

import symtab._
import symtab.classfile.{PickleBuffer, Pickler}
import dependencies.{DependencyAnalysis}
import util.Statistics
import plugins.Plugins
import ast._
import ast.parser._
import typechecker._
import transform._
import backend.icode.{ICodes, GenICode, Checkers}
import backend.ScalaPrimitives
import backend.jvm.GenJVM
import backend.msil.GenMSIL
import backend.opt.{Inliners, ClosureElimination, DeadCodeElimination}
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

  //def this() = this(new Settings, new ConsoleReporter)

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
      typer.typed(mkAttributedCastUntyped(tree, pt))
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

  /** Computing pairs of overriding/overridden symbols */
  object overridingPairs extends {
    val global: Global.this.type = Global.this
  } with OverridingPairs

  /** Representing ASTs as graphs */
  object treeBrowsers extends {
    val global: Global.this.type = Global.this
  } with TreeBrowsers

  val treeBrowser = treeBrowsers.create()


//  val copy = new LazyTreeCopier()

  /** A map of all doc comments, indexed by symbols.
   *  Only active in onlyPresentation mode
   */
  val comments =
    if (onlyPresentation) new HashMap[Symbol,String]
    else null

  /** A map of argument names for methods
   *  !!! can be dropped once named method arguments are in !!!
   */
  val methodArgumentNames =
    if (onlyPresentation) new HashMap[Symbol,List[List[Symbol]]]
    else null

  // ------------ Hooks for interactive mode-------------------------

  /** Return a position correponding to tree startaing at `start`, with tip
   *  at `mid`, and ending at `end`. ^ batch mode errors point at tip.
   */
  def rangePos(source: SourceFile, start: Int, mid: Int, end: Int) = OffsetPosition(source, mid)

  /** Called every time an AST node is succesfully typedchecked in typerPhase.
   */
  def signalDone(context: analyzer.Context, old: Tree, result: Tree) {}

  /** Register new context; called for every created context
   */
  def registerContext(c: analyzer.Context) {}

// ------------------ Reporting -------------------------------------

  import nsc.util.NoPosition
  def error(msg: String) = reporter.error(NoPosition, msg)
  def warning(msg: String) = reporter.warning(NoPosition, msg)
  def inform(msg: String) = Console.err.println(msg)
  def inform[T](msg: String, value: T): T = { inform(msg+value); value }

  //reporter.info(null, msg, true)

  def informProgress(msg: String) =
    if (settings.verbose.value) inform("[" + msg + "]")

  def informTime(msg: String, start: Long) =
    informProgress(msg + " in " + (currentTime - start) + "ms")

  def log(msg: AnyRef) {
    if (settings.log contains phase.name) inform("[log " + phase + "] " + msg)
  }

  class ErrorWithPosition(val pos: Int, val error: Throwable) extends Error

  def tryWith[T](pos: Int, body: => T): T = try {
    body
  } catch {
    case e : ErrorWithPosition => throw e
    case te: TypeError => throw te
    case e : Error            => throw new ErrorWithPosition(pos, e)
    case e : RuntimeException => throw new ErrorWithPosition(pos, e)
  }

  def catchWith[T](source : SourceFile, body : => T) : T = try {
    body
  } catch {
    case e : ErrorWithPosition =>
      logError("POS: " + source.dbg(e.pos), e)
      throw e.error
  }

  def logError(msg: String, t: Throwable): Unit = ()

  def abort(msg: String) = throw new Error(msg)

// ------------ File interface -----------------------------------------

  private val reader: SourceReader = {
    def stdCharset: Charset = {
      settings.encoding.value = Properties.encodingString // A mandatory charset
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

  settings.dependenciesFile.value match {
    case "none" => ()
    case x =>
      val jfile = new java.io.File(x)
      if (!jfile.exists) jfile.createNewFile

      dependencyAnalysis.loadFrom(AbstractFile.getFile(jfile))
  }



  lazy val classPath0 = new ClassPath(false && onlyPresentation)

  lazy val classPath =
    if (forMSIL)
      new classPath0.Build(settings.sourcepath.value, settings.outdir.value)
    else
      new classPath0.Build(settings.classpath.value, settings.sourcepath.value,
                           settings.outdir.value, settings.bootclasspath.value,
                           settings.extdirs.value, settings.Xcodebase.value)
  /* .NET's equivalent of a classpath */
  lazy val assemrefs = {
    import java.util.{StringTokenizer}
    val set = new HashSet[File]
    val assems = new StringTokenizer(settings.assemrefs.value, File.pathSeparator)
    while (assems.hasMoreTokens())
      set += new java.io.File(assems.nextToken())
    set
  }

  if (settings.verbose.value) {
    inform("[Classpath = " + classPath + "]")
    if (forMSIL) inform("[AssemRefs = " + settings.assemrefs.value + "]")
  }

  def getSourceFile(f: AbstractFile): SourceFile =
    new BatchSourceFile(f, reader.read(f))

  def getSourceFile(name: String): SourceFile = {
    val f = AbstractFile.getFile(name)
    if (f eq null) throw new FileNotFoundException(
      "source file '" + name + "' could not be found")
    getSourceFile(f)
  }

  def getSourceFile(clazz: Symbol): SourceFile = {
    val ret = classPath.root.find(clazz.fullNameString(File.separatorChar), false)
    if (!ret.isSourceFile) throw new FileNotFoundException(
      "source file for " + clazz + " could not be found")
    getSourceFile(ret.sourceFile)
  }

  lazy val loaders : SymbolLoaders { val global : Global.this.type } = new {
    val global: Global.this.type = Global.this
  } with SymbolLoaders

  def rootLoader: LazyType =
    if (forMSIL) new loaders.NamespaceLoader(classPath.root)
    else new loaders.PackageLoader(classPath.root /* getRoot() */)

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

    /** Is current phase cancelled on this unit? */
    def cancelled(unit: CompilationUnit) =
      reporter.cancelled ||
      unit.isJava && this.id > currentRun.namerPhase.id

    final def applyPhase(unit: CompilationUnit) {
      if (settings.debug.value) inform("[running phase " + name + " on " + unit + "]")
      val unit0 = currentRun.currentUnit
      currentRun.currentUnit = unit
      reporter.setSource(unit.source)
      if (!cancelled(unit)) apply(unit)
      currentRun.advanceUnit
      assert(currentRun.currentUnit == unit)
      currentRun.currentUnit = unit0
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
  var printTypings = false

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

 //  object checkDefined extends {
 //    val global: Global.this.type = Global.this
 //  } with CheckDefined

  // phaseName = "explicitouter"
  object explicitOuter extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("tailcalls")
    val runsRightAfter = None
  } with ExplicitOuter

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

  // phaseName = "msil"
  object genMSIL extends {
    val global: Global.this.type = Global.this
    val runsAfter = List[String]("dce")
    val runsRightAfter = None
  } with GenMSIL

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
    analyzer.NoContext.make(EmptyTree, Global.this.definitions.RootClass, newScope))

  /* Add the internal compiler phases to the phases set
   */
  protected def computeInternalPhases() {
    phasesSet += syntaxAnalyzer                        // The parser
    phasesSet += analyzer.namerFactory                 // note: types are there because otherwise
    phasesSet += analyzer.typerFactory                 // consistency check after refchecks would fail.
    phasesSet += superAccessors			       // add super accessors
    phasesSet += pickler			       // serialize symbol tables
    phasesSet += refchecks			       // perform reference and override checking, translate nested objects

//    if (false && settings.Xexperimental.value)
//	phasesSet += devirtualize		       // Desugar virtual classes4

    phasesSet += uncurry			       // uncurry, translate function values to anonymous classes
    phasesSet += tailCalls			       // replace tail calls by jumps
    phasesSet += explicitOuter			       // replace C.this by explicit outer pointers, eliminate pattern matching
    phasesSet += erasure			       // erase generic types to Java 1.4 types, add interfaces for traits
    phasesSet += lazyVals			       //
    phasesSet += lambdaLift			       // move nested functions to top level
//    if (forJVM && settings.Xdetach.value)
//      phasesSet += detach			       // convert detached closures
    phasesSet += constructors			       // move field definitions into constructors
    phasesSet += mixer				       // do mixin composition
    phasesSet += cleanup			       // some platform-specific cleanups
    phasesSet += genicode			       // generate portable intermediate code
    phasesSet += inliner			       // optimization: do inlining
    phasesSet += closureElimination		       // optimization: get rid of uncalled closures
    phasesSet += deadCode			       // optimization: get rid of dead cpde
    phasesSet += terminal                              // The last phase in the compiler chain

    if (! forMSIL) {
      phasesSet += flatten			       // get rid of inner classes
    }
    if (forJVM) {
      phasesSet += liftcode			       // generate reified trees
      phasesSet += genJVM			       // generate .class files
      phasesSet += dependencyAnalysis
    }
    if (forMSIL) {
      phasesSet += genMSIL			       // generate .msil files
    }
  }


  /* Helper method for sequncing the phase assembly
   */
  private def computePhaseDescriptors: List[SubComponent] = {
    computeInternalPhases()	       // Global.scala
    computePluginPhases()	       // plugins/Plugins.scala
    buildCompilerFromPhasesSet()       // PhaseAssembly.scala
  }

  /* Simple option value to hold the compiler phase chain */
  private var phasesCache: Option[List[SubComponent]] = None

  /* The set of phase objects that is the basis for the compiler phase chain */
  protected val phasesSet : HashSet[SubComponent] = new HashSet[SubComponent]

  /** A accessor for the phase descriptor list (List of SubComponents), Only calculate the list once */
  def phaseDescriptors = {
    if (phasesCache.isEmpty) phasesCache = Some(computePhaseDescriptors)
    phasesCache.get
  }

  /** A description of the phases that will run */
  def phaseDescriptions: String = {
    new Run // force some initialization
    val messages =
      for (phase <- phaseDescriptors)
	yield phase.phaseName //todo: + " - " + phase.description
    messages.mkString("\n")
  }

  // ----------- Runs ---------------------------------------

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

    private val firstPhase = {
      // ----------- Initialization code -------------------------
      curRunId += 1
      assert(curRunId > 0)
      curRun = this
      //Console.println("starting run: " + id)

      // Can not take the phaseDescriptors.head even though its the syntaxAnalyser, this will implicitly
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
      if (fileset.size > 0)
        progress((phasec * fileset.size) + unitc,
                 (phaseDescriptors.length-1) * fileset.size) // terminal phase not part of the progress display

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

    val explicitOuterPhase = phaseNamed("explicitouter")
    val erasurePhase = phaseNamed("erasure")
    val flattenPhase = phaseNamed("flatten")
    val mixinPhase = phaseNamed("mixin")
    val icodePhase = phaseNamed("icode")

    /** A test whether compilation should stop at phase with given name */
    protected def stopPhase(name : String) = settings.stop.contains(name)

    // ----------- Units and top-level classes and objects --------

    private var unitbuf = new ListBuffer[CompilationUnit]
    private var fileset = new HashSet[AbstractFile]

    /** add unit to be compiled in this run */
    private def addUnit(unit: CompilationUnit) {
      unitbuf += unit
      fileset += unit.source.file
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

    // --------------- Compilation methods ----------------------------

    /** Compile list of source files */
    def compileSources(_sources: List[SourceFile]) {
      val sources = dependencyAnalysis.filter(_sources.removeDuplicates) // bug #1268, scalac confused by duplicated filenames
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
        if (settings.print contains globalPhase.name)
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
        if (settings.statistics.value) statistics.print(phase)
        advancePhase
      }

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

      if (!dependencyAnalysis.off)
        dependencyAnalysis.saveDependencies()
    }

    /** Compile list of abstract files */
    def compileFiles(files: List[AbstractFile]) {
      try {
        compileSources(files map getSourceFile)
      } catch {
        case ex: IOException => error(ex.getMessage())
      }
    }

    /** Compile list of files given by their names */
    def compile(filenames: List[String]) {
      try {
        val scriptMain = settings.script.value
        // Are we compiling a script?
        if (scriptMain != "") {
          if(filenames.length != 1)
            error("can only compile one script at a time")
          val scriptFile =
	    ScriptRunner.wrappedScript(scriptMain, filenames.head, getSourceFile)
          compileSources(List(scriptFile))
        // No we are compiling regular source files
	} else {
          compileSources(filenames map getSourceFile)
	}
      } catch {
        case ex: IOException => error(ex.getMessage())
      }
    }

    /** Compile abstract file until `globalPhase`, but at least
     *  to phase "namer".
     */
    def compileLate(file: AbstractFile) {
      if (fileset eq null) {
        val msg = "No class file for " + file +
                  " was found\n(This file cannot be loaded as a source file)"
        inform(msg)
        throw new FatalError(msg)
      }
      else if (!(fileset contains file)) {
        compileLate(new CompilationUnit(getSourceFile(file)))
      }
    }

    /** Compile abstract file until `globalPhase`, but at least
     *  to phase "namer".
     */
    def compileLate(unit: CompilationUnit) {
      addUnit(unit)
      var localPhase = firstPhase.asInstanceOf[GlobalPhase]
      while (localPhase != null && (localPhase.id < globalPhase.id || localPhase.id <= namerPhase.id) && !reporter.hasErrors) {
        val oldSource = reporter.getSource
        reporter.setSource(unit.source)
        atPhase(localPhase)(localPhase.applyPhase(unit))
        val newLocalPhase = localPhase.next.asInstanceOf[GlobalPhase]
        localPhase = if (localPhase == newLocalPhase) null else newLocalPhase
        reporter.setSource(oldSource)
      }
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

  /** Returns the file with the given suffix for the given class. */
  def getFile(clazz: Symbol, suffix: String): File = {
    val outdirname = settings.outputDirs.outputDirFor(clazz.sourceFile)
    var outdir = new File(if (outdirname.path == "") "." else outdirname.path)
    val filename = clazz.fullNameString('.')
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
  def onlyPresentation = false
  private val unpickleIDEHook0 : (( => Type) => Type) = f => f
  def unpickleIDEHook : (( => Type) => Type) = unpickleIDEHook0

  def doPickleHash = false
}
