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
import scala.tools.nsc.util.{ClassPath, Position, SourceFile}

import scala.collection.mutable.{HashSet, HashMap, ListBuffer}

import symtab._
import symtab.classfile.{PickleBuffer, Pickler, ICodeReader}
import util.Statistics
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
                                                             with Trees
                                                             with CompilationUnits
{
  // alternate constructors ------------------------------------------
  def this(reporter: Reporter) =
    this(new Settings(err => reporter.error(null,err)),
         reporter)

  def this(settings: Settings) =
    this(settings, new ConsoleReporter(settings))

  //def this() = this(new Settings, new ConsoleReporter)

  // sub-components --------------------------------------------------
  object treePrinters extends TreePrinters {
    val global: Global.this.type = Global.this
  }
  val treePrinter = treePrinters.create()

  object treeBrowsers extends TreeBrowsers {
    val global: Global.this.type = Global.this
  }
  val treeBrowser = treeBrowsers.create()

  object treeInfo extends TreeInfo {
    val global: Global.this.type = Global.this
  }

  object gen extends TreeGen {
    val global: Global.this.type = Global.this
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

  object icodeReader extends ICodeReader {
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

  val copy = new LazyTreeCopier()

  val comments =
    if (onlyPresentation) new HashMap[Symbol,String]
    else null

// reporting -------------------------------------------------------

  def error(msg: String) = reporter.error(null, msg)
  def warning(msg: String) = reporter.warning(null, msg)
  def inform(msg: String) = Console.err.println(msg)
  def inform[T](msg: String, value: T): T = { inform(msg+value); value }

  //reporter.info(null, msg, true)

  def informProgress(msg: String) =
    if (settings.verbose.value) inform("[" + msg + "]")

  def informTime(msg: String, start: Long) =
    informProgress(msg + " in " + (currentTime - start) + "ms")

  def log(msg: AnyRef): unit =
    if (settings.log contains phase.name) inform("[log " + phase + "] " + msg)

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

  def logError(msg: String, t: Throwable): Unit = {}

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
    new SourceReader(charset.newDecoder())
  }

  val classPath0 = new ClassPath(false && onlyPresentation)

  val classPath = new classPath0.Build(
    if (forMSIL) "" else settings.classpath.value,
    settings.sourcepath.value,
    settings.outdir.value,
    if (forMSIL) "" else settings.bootclasspath.value,
    if (forMSIL) "" else settings.extdirs.value)

  if (settings.verbose.value) {
    inform("[Classpath = " + classPath+"]")
  }

  def getSourceFile(f: AbstractFile): SourceFile =
    new SourceFile(f, reader.read(f))

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

  object loaders extends SymbolLoaders {
    val global: Global.this.type = Global.this
  }

  def rootLoader: LazyType = if (forMSIL) new loaders.NamespaceLoader(classPath.root)
                             else new loaders.PackageLoader(classPath.root /* getRoot() */)

  val migrateMsg = "migration problem when moving from Scala version 1.0 to version 2.0:\n"

// Phases ------------------------------------------------------------

  var globalPhase: Phase = NoPhase

  val MaxPhases = 64

  val phaseWithId = new Array[Phase](MaxPhases);
  { for (val i <- List.range(0, MaxPhases)) phaseWithId(i) = NoPhase }

  abstract class GlobalPhase(prev: Phase) extends Phase(prev) {
    phaseWithId(id) = this
    def run { currentRun.units foreach applyPhase }

    def apply(unit: CompilationUnit): unit
    private val isErased = prev.name == "erasure" || prev.erasedTypes
    override def erasedTypes: boolean = isErased
    private val isFlat = prev.name == "flatten" || prev.flatClasses
    override def flatClasses: boolean = isFlat
    final def applyPhase(unit: CompilationUnit): unit = {
      if (settings.debug.value) inform("[running phase " + name + " on " + unit + "]")
      val unit0 = currentRun.currentUnit
      currentRun.currentUnit = unit
      if (!reporter.cancelled) apply(unit)
      currentRun.advanceUnit
      assert(currentRun.currentUnit == unit)
      currentRun.currentUnit = unit0
    }
  }

  class TerminalPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = "terminal"
    def apply(unit: CompilationUnit): unit = {}
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

  def phaseDescriptors: List[SubComponent] = List(
    analyzer.namerFactory: SubComponent, // note: types are there because otherwise
    analyzer.typerFactory: SubComponent, // consistency check after refchecks would fail.
    superAccessors,
    pickler,
    refchecks,
    liftcode,
    uncurry,
    tailCalls,
    explicitOuter,
//    checkDefined,
    erasure,
    lambdaLift,
//    detach,
    constructors,
    flatten,
    mixer,
    cleanup,
    genicode,
    inliner,
    closureElimination,
    deadCode,
    if (forMSIL) genMSIL else genJVM,
    sampleTransform)

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
    curRunId = curRunId + 1
    assert(curRunId > 0)
    //Console.println("starting run: " + id)
    var currentUnit: CompilationUnit = _
    curRun = this
    val firstPhase = syntaxAnalyzer.newPhase(NoPhase)
    phase = firstPhase
    definitions.init; // needs firstPhase and phase to be defined != NoPhase,
                      // that's why it is placed here.
    icodes.init

    /** Deprecation warnings occurred */
    var deprecationWarnings: boolean = false
    var uncheckedWarnings: boolean = false

    private var p: Phase = firstPhase

    for (val pd <- phaseDescriptors.takeWhile(pd => !(settings.stop contains pd.phaseName)))
      if (!(settings.skip contains pd.phaseName)) p = pd.newPhase(p)

    def cancel { reporter.cancelled = true }

    // progress tracking
    def progress(current: Int, total: Int): Unit = {}

    private var phasec: Int = 0
    private var unitc: Int = 0
    def advancePhase: Unit = {
      unitc = 0
      phasec = phasec + 1
      refreshProgress
    }
    def advanceUnit: Unit = {
      unitc = unitc + 1
      refreshProgress
    }
    private def refreshProgress = if (fileset.size > 0)
      progress((phasec * fileset.size) + unitc,
               (phaseDescriptors.length+1) * fileset.size)

    def phaseNamed(name: String): Phase = {
      var p: Phase = firstPhase
      while (p.next != p && p.name != name) p = p.next
      if (p.name != name) NoPhase else p
    }

    val namerPhase = phaseNamed("namer")
    val typerPhase = phaseNamed("typer")
    val refchecksPhase = phaseNamed("refchecks")

    val explicitOuterPhase = phaseNamed("explicitouter")
    val erasurePhase = phaseNamed("erasure")
    val flattenPhase = phaseNamed("flatten")
    val mixinPhase = phaseNamed("mixin")
    val icodePhase = phaseNamed("icode")

    private var unitbuf = new ListBuffer[CompilationUnit]
    private var fileset = new HashSet[AbstractFile]

    val terminalPhase : Phase =
      if (onlyPresentation) typerPhase.next.next
      else new TerminalPhase(p)

    private def addUnit(unit: CompilationUnit): unit = {
      unitbuf += unit
      fileset += unit.source.getFile()
    }

    def units: Iterator[CompilationUnit] = unitbuf.elements

    /** A map from compiled top-level symbols to their source files */
    val symSource = new HashMap[Symbol, AbstractFile]

    /** A map from compiled top-level symbols to their picklers */
    val symData = new HashMap[Symbol, PickleBuffer]

    /** does this run compile given class, module, or case factory? */
    def compiles(sym: Symbol): boolean =
      if (sym == NoSymbol) false
      else if (symSource.isDefinedAt(sym)) true
      else if (!sym.owner.isPackageClass) compiles(sym.toplevelClass)
      else if (sym.isModuleClass) compiles(sym.sourceModule)
      else false

    def compileSources(sources: List[SourceFile]): unit = {
      val startTime = currentTime
      reporter.reset
      for (val source <- sources)
        addUnit(new CompilationUnit(source))

      globalPhase = firstPhase
      while (globalPhase != terminalPhase && !reporter.hasErrors) {
        val startTime = currentTime
        phase = globalPhase
        globalPhase.run
        if (settings.print contains globalPhase.name)
          if (globalPhase.id >=  icodePhase.id) writeICode()
          else treePrinter.printAll()
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
        for (val (sym, file) <- symSource.elements) {
          sym.reset(new loaders.SourcefileLoader(file))
          if (sym.isTerm) sym.moduleClass.reset(loaders.moduleClassLoader)
        }
      } else {
        //assert(symData.isEmpty || !settings.stop.value.isEmpty || !settings.skip.value.isEmpty, symData)
        if (deprecationWarnings) {
          warning("there were deprecation warnings; re-run with -deprecation for details")
        }
        if (uncheckedWarnings) {
          warning("there were unchecked warnings; re-run with -unchecked for details")
        }
      }
      for (val (sym, file) <- symSource.elements) resetPackageClass(sym.owner)
      //units foreach (.clear())
      informTime("total", startTime)
    }

    def compileLate(file: AbstractFile): unit =
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
        while ((localPhase.id < globalPhase.id || localPhase.id <= namerPhase.id) && !reporter.hasErrors) {
          atPhase(localPhase)(localPhase.applyPhase(unit))
          localPhase = localPhase.next.asInstanceOf[GlobalPhase]
        }
        refreshProgress
      }

    def compileFiles(files: List[AbstractFile]): unit =
      try {
        compileSources(files map getSourceFile)
      } catch {
        case ex: IOException => error(ex.getMessage())
      }

    def compile(filenames: List[String]): unit =
      try {
        if (settings.Xscript.value && filenames.length != 1)
          error("can only compile one script at a time")
        val sources = filenames map (
          if (settings.Xscript.value)
            (x => ScriptRunner.wrappedScript(x, &getSourceFile))
          else
            getSourceFile)
        compileSources(sources)
      } catch {
        case ex: IOException => error(ex.getMessage())
      }

    private def resetPackageClass(pclazz: Symbol): unit = {
      atPhase(firstPhase) {
        pclazz.setInfo(atPhase(typerPhase)(pclazz.info))
      }
      if (!pclazz.isRoot) resetPackageClass(pclazz.owner)
    }
  } // class Run

  def showDef(name: Name, module: boolean): unit = {
    def getSym(name: Name, module: boolean): Symbol = {
      var i = name.length - 1
      while (i != 0 && name(i) != '#' && name(i) != '.') i = i - 1
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
    inform("" + sym.name + ":" +(if (module) sym.tpe.symbol.info else sym.info))
  }

  /** Returns the file with the given suffix for the given class. */
  def getFile(clazz: Symbol, suffix: String) = {
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

  private def writeSymblFile(clazz: Symbol, pickled: PickleBuffer) = {
    val file = getFile(clazz, ".symbl")
    try {
      val stream = new FileOutputStream(file)
      stream.write(pickled.bytes, 0, pickled.writeIndex)
      stream.close()
      informProgress("wrote " + file)
    } catch {
      case ex: IOException =>
      if (settings.debug.value) ex.printStackTrace()
      error("could not write file " + file)
    }
  }

  private def writeICode(): Unit = {
    val printer = new icodes.TextPrinter(null, icodes.linearizer)
    icodes.classes.values.foreach((cls) => {
      val suffix = if (cls.symbol.hasFlag(Flags.MODULE)) "$.icode" else ".icode"
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
  def forMSIL: Boolean = settings.target.value == "msil"
  def onlyPresentation = settings.doc.value
  // position stuff
  final val positionConfiguration: PositionConfiguration = initConfig;
  protected def initConfig : PositionConfiguration = posConfig;

  private object posConfig extends PositionConfiguration {
    type PositionType = Int
    def coercePosToInt(pos: Int): Int = pos
    def coerceIntToPos(pos: Int): Int = pos
    val NoPos: Int = Position.NOPOS
    val FirstPos: Int = Position.FIRSTPOS
  }
  final type PositionType = positionConfiguration.PositionType
  final val FirstPos = {
    val posConfig : PositionConfiguration = positionConfiguration;
    posConfig.FirstPos.asInstanceOf[PositionType];
  }
  final def NoPos = positionConfiguration.NoPos
  final def coerceIntToPos(pos: Int): PositionType =
    positionConfiguration.coerceIntToPos(pos)
  implicit final def coercePosToInt(pos: PositionType): Int =
    positionConfiguration.coercePosToInt(pos)
}
