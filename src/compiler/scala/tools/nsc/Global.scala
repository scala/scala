/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
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
import scala.tools.nsc.util.{ClassPath, SourceFile, BatchSourceFile}

import scala.collection.mutable.{HashSet, HashMap, ListBuffer}

import symtab._
import symtab.classfile.{PickleBuffer, Pickler}
import util.Statistics
import plugins.Plugins
import ast._
import ast.parser._
import typechecker._
//import matching.TransMatcher
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
{
  // alternate constructors ------------------------------------------
  def this(reporter: Reporter) =
    this(new Settings(err => reporter.error(null,err)),
         reporter)

  def this(settings: Settings) =
    this(settings, new ConsoleReporter(settings))

  //def this() = this(new Settings, new ConsoleReporter)

  // sub-components --------------------------------------------------
  object nodePrinters extends NodePrinters {
    val global: Global.this.type = Global.this
    infolevel = InfoLevel.Verbose
  }
  val nodeToString = nodePrinters.nodeToString

  object gen extends TreeGen {
    val global: Global.this.type = Global.this
    def mkAttributedCast(tree: Tree, pt: Type): Tree =
      typer.typed(mkAttributedCastUntyped(tree, pt))
  }

  object constfold extends ConstantFolder {
    val global: Global.this.type = Global.this
  }

  object checker extends TreeCheckers {
    val global: Global.this.type = Global.this
  }

  object icodes extends ICodes {
    val global: Global.this.type = Global.this
  }

  object analysis extends TypeFlowAnalysis {
    val global: Global.this.type = Global.this
  }

  object copyPropagation extends CopyPropagation {
    val global: Global.this.type = Global.this
  }

  object checkers extends Checkers {
    val global: Global.this.type = Global.this
  }

  object statistics extends Statistics {
    val global: Global.this.type = Global.this
  }

  object overridingPairs extends OverridingPairs {
    val global: Global.this.type = Global.this
  }

  object treeBrowsers extends TreeBrowsers {
    val global: Global.this.type = Global.this
  }
  val treeBrowser = treeBrowsers.create()


//  val copy = new LazyTreeCopier()

  val comments =
    if (onlyPresentation) new HashMap[Symbol,String]
    else null
  val methodArgumentNames =
    if (onlyPresentation) new HashMap[Symbol,List[List[Symbol]]]
    else null

// reporting -------------------------------------------------------
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
    if (settings.logAll.value || (settings.log contains phase.name))
      inform("[log " + phase + "] " + msg)
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

// file interface -------------------------------------------------------

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
      val ccon  = clazz.getConstructor(Array[Class[T] forSome { type T }](classOf[java.nio.charset.CharsetDecoder], classOf[Reporter]))
      ccon.newInstance(Array[AnyRef] (charset.newDecoder(), reporter)).asInstanceOf[SourceReader];
      //new SourceReader(charset.newDecoder())
    } catch {
      case e =>
        error("exception while trying to instantiate source reader \""+settings.sourceReader.value+"\" ");
        new SourceReader(charset.newDecoder(), reporter)
    }
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

  val loaders = new SymbolLoaders {
    lazy val global: Global.this.type = Global.this
  }

  def rootLoader: LazyType =
    if (forMSIL) new loaders.NamespaceLoader(classPath.root)
    else new loaders.PackageLoader(classPath.root /* getRoot() */)

// Phases ------------------------------------------------------------

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

  class TerminalPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = "terminal"
    def apply(unit: CompilationUnit) {}
  }

  object syntaxAnalyzer extends SyntaxAnalyzer {
    val global: Global.this.type = Global.this
  }

  object analyzer extends Analyzer {
    val global: Global.this.type = Global.this
  }

  object superAccessors extends SuperAccessors {
    val global: Global.this.type = Global.this
  }

  object pickler extends Pickler {
    val global: Global.this.type = Global.this
  }

  object refchecks extends RefChecks {
    val global: Global.this.type = Global.this
  }

  object devirtualize extends DeVirtualize {
    val global: Global.this.type = Global.this
  }

  object liftcode extends LiftCode {
    val global: Global.this.type = Global.this
  }

  object uncurry extends UnCurry {
    val global: Global.this.type = Global.this
  }

  object tailCalls extends TailCalls {
    val global: Global.this.type = Global.this
  }

  //object transMatcher extends TransMatcher {
  //  val global: Global.this.type = Global.this
  //}

//  object checkDefined extends CheckDefined {
//    val global: Global.this.type = Global.this
//  }

  object explicitOuter extends ExplicitOuter {
    val global: Global.this.type = Global.this
  }

  object erasure extends Erasure {
    val global: Global.this.type = Global.this
  }

  object lazyVals extends LazyVals {
    val global: Global.this.type = Global.this
    final val FLAGS_PER_WORD = 32
  }

  object lambdaLift extends LambdaLift {
    val global: Global.this.type = Global.this
  }

  object constructors extends Constructors {
    val global: Global.this.type = Global.this
  }

  object flatten extends Flatten {
    val global: Global.this.type = Global.this
  }
/*
  object detach extends Detach {
    val global: Global.this.type = Global.this
  }
*/
  object mixer extends Mixin {
    val global: Global.this.type = Global.this
  }

  object cleanup extends CleanUp {
    val global: Global.this.type = Global.this
  }

  object sampleTransform extends SampleTransform {
    val global: Global.this.type = Global.this
  }

  object genicode extends GenICode {
    val global: Global.this.type = Global.this
  }
/*
  object icodePrinter extends backend.icode.Printers {
    val global: Global.this.type = Global.this
  }
*/
  object scalaPrimitives extends ScalaPrimitives {
    val global: Global.this.type = Global.this
  }

  object inliner extends Inliners {
    val global: Global.this.type = Global.this
  }

  object closureElimination extends ClosureElimination {
    val global: Global.this.type = Global.this
  }

  object deadCode extends DeadCodeElimination {
    val global: Global.this.type = Global.this
  }

  object genJVM extends GenJVM {
    val global: Global.this.type = Global.this
  }

  object genMSIL extends GenMSIL {
    val global: Global.this.type = Global.this
  }

  object icodeChecker extends checkers.ICodeChecker()

  object typer extends analyzer.Typer(
    analyzer.NoContext.make(EmptyTree, Global.this.definitions.RootClass, newScope))

  /** The built-in components.  The full list of components, including
   *  plugins, is computed in the Plugins trait.
   */
  protected def builtInPhaseDescriptors: List[SubComponent] = List(
    analyzer.namerFactory: SubComponent, // note: types are there because otherwise
    analyzer.typerFactory: SubComponent, // consistency check after refchecks would fail.
    superAccessors,  // add super accessors
    pickler         // serialize symbol tables
  ) ::: (
    // Desugar virtual classes
    if (settings.Xexperimental.value) List(devirtualize) else List()
  ) ::: List(
    refchecks        // perform reference and override checking, translate nested objects
  ) ::: (
    if (forJVM) List(liftcode) else List() // generate reified trees
  ) ::: List(
    uncurry,         // uncurry, translate function values to anonymous classes
    tailCalls,       // replace tail calls by jumps
    explicitOuter,   // replace C.this by explicit outer pointers, eliminate pattern matching
//    checkDefined,
    erasure,         // erase generic types to Java 1.4 types, add interfaces for traits
    lazyVals,
    lambdaLift,      // move nested functions to top level
//    detach,
    constructors     // move field definitions into constructors
  ) ::: (
    if (forMSIL) List() else List(flatten) // get rid of inner classes
  ) ::: List(
    mixer,           // do mixin composition
    cleanup,         // some platform-specific cleanups

    genicode,        // generate portable intermediate code
    inliner,         // optimization: do inlining
    closureElimination, // optimization: get rid of uncalled closures
    deadCode,           // optimization: get rid of dead cpde
    if (forMSIL) genMSIL else genJVM, // generate .class files
    sampleTransform
  )


  private var phasesCache: Option[List[SubComponent]] = None

  def phaseDescriptors = {
    if (phasesCache.isEmpty)
      phasesCache = Some(computePhaseDescriptors)
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


  protected def insertBefore(c: SubComponent, cs: List[SubComponent], before: SubComponent): List[SubComponent] = cs match {
    case List() => List(c)
    case c1 :: cs1 => if (c1 == before) c :: cs else c1 :: insertBefore(c, cs1, before)
  }

  private var curRun: Run = null
  def currentRun: Run = curRun
  private var curRunId = 0
  override def currentRunId = curRunId

  private var runCount = 0

  class Run {
    curRunId += 1
    assert(curRunId > 0)
    //Console.println("starting run: " + id)
    var currentUnit: CompilationUnit = _
    curRun = this
    private val firstPhase = syntaxAnalyzer.newPhase(NoPhase)
    phase = firstPhase
    definitions.init  // needs phase to be defined != NoPhase,
                      // that's why it is placed here.

    /** Deprecation warnings occurred */
    var deprecationWarnings: Boolean = false
    var uncheckedWarnings: Boolean = false

    private var p: Phase = firstPhase

    protected def stopPhase(name : String) =
      if (onlyPresentation) name == "superaccessors"
      else settings.stop.contains(name)
   //  protected def stopPhase(name : String) = settings.stop.contains(name)

    for (pd <- phaseDescriptors.takeWhile(pd => !(stopPhase(pd.phaseName))))
      if (!(settings.skip contains pd.phaseName)) p = pd.newPhase(p)

    def cancel { reporter.cancelled = true }

    // progress tracking
    def progress(current: Int, total: Int) {}

    private var phasec: Int = 0
    private var unitc: Int = 0
    def advancePhase {
      unitc = 0
      phasec += 1
      refreshProgress
    }
    def advanceUnit {
      unitc += 1
      refreshProgress
    }
    private def refreshProgress =
      if (fileset.size > 0)
        progress((phasec * fileset.size) + unitc,
                 (phaseDescriptors.length+1) * fileset.size)

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

    private var unitbuf = new ListBuffer[CompilationUnit]
    private var fileset = new HashSet[AbstractFile]

    lazy val terminalPhase : Phase = {
      //var q : Phase = firstPhase
      //while (q != null && !stopPhase(q.name)) q = q.next
      //if (q == null)
      new TerminalPhase(p)
      //else q
    }
    private def addUnit(unit: CompilationUnit) {
      unitbuf += unit
      fileset += unit.source.file
    }

    def units: Iterator[CompilationUnit] = unitbuf.elements

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

    def compileSources(sources: List[SourceFile]) {
      if (reporter.hasErrors)
        return  // there is a problem already, e.g. a
                // plugin was passed a bad option
      val startTime = currentTime
      reporter.reset
      for (source <- sources) addUnit(new CompilationUnit(source))

      globalPhase = firstPhase
      while (globalPhase != terminalPhase && !reporter.hasErrors) {
        val startTime = currentTime
        phase = globalPhase
        globalPhase.run
        if (settings.print contains globalPhase.name)
          if (globalPhase.id >= icodePhase.id) writeICode()
          else if (settings.Xshowtrees.value) nodePrinters.printAll()
          else printAllUnits()
        if (settings.printLate.value && globalPhase.name == "cleanup")
          printAllUnits()

        if (settings.browse contains globalPhase.name) treeBrowser.browse(units)
        informTime(globalPhase.description, startTime)
        globalPhase = globalPhase.next
        if (settings.check contains globalPhase.name) {
          phase = globalPhase
          if (globalPhase.id >= icodePhase.id) icodeChecker.checkICodes
          else checker.checkTrees
        }
        if (settings.statistics.value) statistics.print(phase)
        advancePhase
      }

      if (settings.Xshowcls.value != "")
        showDef(newTermName(settings.Xshowcls.value), false)
      if (settings.Xshowobj.value != "")
        showDef(newTermName(settings.Xshowobj.value), true)

      if (reporter.hasErrors) {
        for ((sym, file) <- symSource.elements) {
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
      for ((sym, file) <- symSource.elements) resetPackageClass(sym.owner)
      //units foreach (.clear())
      informTime("total", startTime)
    }

    def compileLate(file: AbstractFile) {
      if (fileset eq null) {
        val msg = "No class file for " + file +
                  " was found\n(This file cannot be loaded as a source file)"
        inform(msg)
        throw new FatalError(msg)
      }
      else if (!(fileset contains file)) {
        val unit = new CompilationUnit(getSourceFile(file))
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
    }

    def compileFiles(files: List[AbstractFile]) {
      try {
        compileSources(files map getSourceFile)
      } catch {
        case ex: IOException => error(ex.getMessage())
      }
    }

    def compile(filenames: List[String]) {
      try {
        val scriptMain = settings.script.value
        if (scriptMain != "" && filenames.length != 1)
          error("can only compile one script at a time")
        val sources = filenames map (
          if (scriptMain != "")
            (x => ScriptRunner.wrappedScript(scriptMain, x, getSourceFile _))
          else
            getSourceFile)
        compileSources(sources)
      } catch {
        case ex: IOException => error(ex.getMessage())
      }
    }

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
    val outdirname = settings.outdir.value
    var outdir = new File(if (outdirname == "") "." else outdirname)
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

  def forCLDC: Boolean = settings.target.value == "cldc"
  def forJVM : Boolean = settings.target.value startsWith "jvm"
  def forMSIL: Boolean = settings.target.value == "msil"
  def onlyPresentation = inIDE
  private val unpickleIDEHook0 : (( => Type) => Type) = f => f
  def unpickleIDEHook : (( => Type) => Type) = unpickleIDEHook0

  def doPickleHash = false
}
