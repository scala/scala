/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc;

import java.io._;
import java.nio.charset._;
import scala.tools.util._;
import scala.collection.mutable.{HashSet,HashMap}

import symtab._;
import symtab.classfile.{PickleBuffer, Pickler};
import util.{ListBuffer, Statistics};
import ast._;
import ast.parser._;
import typechecker._;
import matching.TransMatcher;
import transform._;
import backend.icode.{ICodes, GenICode, Checkers};
import backend.ScalaPrimitives;
import backend.jvm.BytecodeGenerators;

class Global(val settings: Settings, val reporter: Reporter) extends SymbolTable
                                                             with Trees
                                                             with CompilationUnits
{

  // sub-components --------------------------------------------------

  object treePrinters extends TreePrinters {
    val global: Global.this.type = Global.this
  }
  val treePrinter = treePrinters.create();

  object treeBrowsers extends TreeBrowsers {
    val global: Global.this.type = Global.this
  }
  val treeBrowser = treeBrowsers.create();

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

  object checkers extends Checkers {
    val global: Global.this.type = Global.this
  }

  object statistics extends Statistics {
    val global: Global.this.type = Global.this
  }

  object overridingPairs extends OverridingPairs {
    val global: Global.this.type = Global.this
  }

  val copy = new LazyTreeCopier();

// reporting -------------------------------------------------------

  def error(msg: String) = reporter.error(null, msg);
  def warning(msg: String) = reporter.warning(null, msg);
  def inform(msg: String) = reporter.info(null, msg, true);

  def informProgress(msg: String) =
    if (settings.verbose.value) inform("[" + msg + "]");

  def informTime(msg: String, start: long) =
    informProgress(msg + " in " + (System.currentTimeMillis() - start) + "ms");

  def log(msg: Object): unit =
    if (settings.log contains phase.name) inform("[log " + phase + "] " + msg);

  def abort(msg: String) = throw new Error(msg);

// file interface -------------------------------------------------------

  private val reader: SourceReader = {
    def stdCharset: Charset = {
      settings.encoding.value = "ISO-8859-1"; // A mandatory charset
      Charset.forName(settings.encoding.value);
    }
    val charset =
      try {
        Charset.forName(settings.encoding.value);
      } catch {
        case _: IllegalCharsetNameException =>
          error("illegal charset name '" + settings.encoding.value + "'");
          stdCharset
        case _: UnsupportedCharsetException =>
          error("unsupported charset '" + settings.encoding.value + "'");
          stdCharset
      }
    new SourceReader(charset.newDecoder());
  }

  val classPath = new ClassPath(
    settings.classpath.value,
    settings.sourcepath.value,
    settings.bootclasspath.value,
    settings.extdirs.value);

  if (settings.verbose.value) {
    System.out.println("classpath = " + classPath);
  }

  def getSourceFile(f: AbstractFile): SourceFile =
    new SourceFile(f, reader.read(f));

  def getSourceFile(name: String): SourceFile = {
    val f = AbstractFile.getFile(name);
    if (f == null) throw new FileNotFoundException(
      "source file '" + name + "' could not be found");
    getSourceFile(f)
  }

  def getSourceFile(clazz: Symbol): SourceFile = {
    val f = classPath.getRoot().lookupPath(
      clazz.fullNameString(File.separatorChar) + ".scala", false);
    if (f == null) throw new FileNotFoundException(
      "source file for " + clazz + " could not be found");
    getSourceFile(f)
  }

  object loaders extends SymbolLoaders {
    val global: Global.this.type = Global.this
  }

  def rootLoader: LazyType = new loaders.PackageLoader(classPath.getRoot());

// Phases ------------------------------------------------------------

  var globalPhase: Phase = NoPhase;

  val MaxPhases = 64;

  val phaseWithId = new Array[Phase](MaxPhases);
  { for (val i <- List.range(0, MaxPhases)) phaseWithId(i) = NoPhase }

  abstract class GlobalPhase(prev: Phase) extends Phase(prev) {
    phaseWithId(id) = this;
    def run: unit = currentRun.units foreach applyPhase;
    def apply(unit: CompilationUnit): unit;
    private val isErased = prev.name == "erasure" || prev.erasedTypes;
    override def erasedTypes: boolean = isErased;
    private val isFlat = prev.name == "flatten" || prev.flatClasses;
    override def flatClasses: boolean = isFlat;
    def applyPhase(unit: CompilationUnit): unit = {
      if (settings.debug.value) inform("[running phase " + name + " on " + unit + "]");
      apply(unit)
    }
  }

  object syntaxAnalyzer extends SyntaxAnalyzer {
    val global: Global.this.type = Global.this
  }

  object analyzer extends Analyzer {
    val global: Global.this.type = Global.this;
  }

  object pickler extends Pickler {
    val global: Global.this.type = Global.this
  }

  object refchecks extends RefChecks {
    val global: Global.this.type = Global.this;
  }

  object uncurry extends UnCurry {
    val global: Global.this.type = Global.this;
  }

  object tailCalls extends TailCalls {
    val global: Global.this.type = Global.this;
  }

  object transMatcher extends TransMatcher {
    val global: Global.this.type = Global.this;
  }

  object explicitOuter extends ExplicitOuter {
    val global: Global.this.type = Global.this;
  }

  object erasure extends Erasure {
    val global: Global.this.type = Global.this;
  }

  object lambdaLift extends LambdaLift {
    val global: Global.this.type = Global.this;
  }

  object flatten extends Flatten {
    val global: Global.this.type = Global.this;
  }

  object constructors extends Constructors {
    val global: Global.this.type = Global.this;
  }

  object mixin extends Mixin {
    val global: Global.this.type = Global.this;
  }

  object sampleTransform extends SampleTransform {
    val global: Global.this.type = Global.this;
  }

  object genicode extends GenICode {
    val global: Global.this.type = Global.this;
  }

  object icodePrinter extends backend.icode.Printers {
    val global: Global.this.type = Global.this;
  }

  object scalaPrimitives extends ScalaPrimitives {
    val global: Global.this.type = Global.this;
  }

  object genJVM extends BytecodeGenerators {
    val global: Global.this.type = Global.this;
  }

  object icodeChecker extends checkers.ICodeChecker();

  object typer extends analyzer.Typer(
    analyzer.NoContext.make(EmptyTree, Global.this.definitions.RootClass, new Scope()));

  def phaseDescriptors: List[SubComponent] = List(
    analyzer.namerFactory,
    analyzer.typerFactory,
    pickler,
//    syntheticMethods,
    refchecks,
    uncurry,
    tailCalls,
    transMatcher,
    explicitOuter,
    erasure,
    lambdaLift,
    flatten,
    constructors,
    mixin,
    genicode,
    genJVM,
    sampleTransform);

  private var curRun: Run = NoRun;
  override def currentRun: Run = curRun;

  class Run extends CompilerRun {
    curRun = this;
    override val firstPhase = syntaxAnalyzer.newPhase(NoPhase);
    phase = firstPhase;
    definitions.init; // needs firstPhase and phase to be defined != NoPhase,
                      // that's why it is placed here.

    private var p: Phase = firstPhase;
    private var stopped = false;
    for (val pd <- phaseDescriptors) {
      if (!stopped) {
        if (!(settings.skip contains pd.phaseName)) p = pd.newPhase(p);
        stopped = settings.stop contains pd.phaseName;
      }
    }

    override val terminalPhase = new GlobalPhase(p) {
      def name = "terminal";
      def apply(unit: CompilationUnit): unit = {}
    }

    override def phaseNamed(name: String): Phase = {
      var p: Phase = firstPhase;
      while (p.next != p && p.name != name) p = p.next;
      if (p.name != name) NoPhase else p
    }

    override val namerPhase = phaseNamed("namer");
    override val typerPhase = phaseNamed("typer");
    override val refchecksPhase = phaseNamed("refchecks");
    override val erasurePhase = phaseNamed("erasure");
    override val flattenPhase = phaseNamed("flatten");

    private var unitbuf = new ListBuffer[CompilationUnit];
    private var fileset = new HashSet[AbstractFile];

    private def addUnit(unit: CompilationUnit): unit = {
      unitbuf += unit;
      fileset += unit.source.getFile();
    }

    def units: Iterator[CompilationUnit] = unitbuf.elements;

    /** A map from compiled top-level symbols to their source files */
    val symSource = new HashMap[Symbol, AbstractFile];

    /** A map from compiled top-level symbols to their picklers */
    val symData = new HashMap[Symbol, PickleBuffer];

    def compileSources(sources: List[SourceFile]): unit = {
      val startTime = System.currentTimeMillis();
      reporter.resetCounters();

      for (val source <- sources)
	addUnit(new CompilationUnit(source));

      globalPhase = firstPhase;
      while (globalPhase != terminalPhase && reporter.errors() == 0) {
	val startTime = System.currentTimeMillis();
	phase = globalPhase;
	globalPhase.run;
	if (settings.print contains globalPhase.name) treePrinter.printAll();
	if (settings.browse contains globalPhase.name) treeBrowser.browse(units);
	informTime(globalPhase.description, startTime);
	globalPhase = globalPhase.next;
	if (settings.check contains globalPhase.name) {
          phase = globalPhase;
          if (globalPhase.name == "terminal") icodeChecker.checkICodes;
          else checker.checkTrees;
	}
	if (settings.statistics.value) statistics.print(phase);
      }

      if (settings.Xshowcls.value != "") showDef(newTermName(settings.Xshowcls.value), false);
      if (settings.Xshowobj.value != "") showDef(newTermName(settings.Xshowobj.value), true);
      if (settings.Xshowicode.value) writeICode();

      if (reporter.errors() == 0) {
	for (val Pair(sym, pickled) <- symData.elements.toList) {
	  sym setPos Position.NOPOS;
	  if (symData contains sym) {
	    symData -= sym;
	    symData -= sym.linkedSym;
	    writeSymblFile(sym, pickled)
	  }
	  resetPackageClass(sym.owner);
	}
      } else {
	for (val Pair(sym, file) <- symSource.elements) {
	  sym.reset(new loaders.SourcefileLoader(file));
	  if (sym.isTerm) sym.moduleClass.reset(loaders.errorLoader);
	}
      }
      informTime("total", startTime);
    }

    def compileLate(file: AbstractFile): unit =
      if (fileset == null)
	throw new FatalError("No symbol file for " + file + " was found\n(This file cannot be loaded as a source file)");
      else if (!(fileset contains file)) {
	val unit = new CompilationUnit(getSourceFile(file));
	addUnit(unit);
	var localPhase = firstPhase.asInstanceOf[GlobalPhase];
	while (localPhase.id < globalPhase.id || localPhase.id <= namerPhase.id) {
	  atPhase(localPhase)(localPhase.applyPhase(unit));
	  localPhase = localPhase.next.asInstanceOf[GlobalPhase];
	}
      }

    def compileFiles(files: List[AbstractFile]): unit =
      try {
	compileSources(files map getSourceFile)
      } catch {
	case ex: IOException => error(ex.getMessage());
      }

    def compile(filenames: List[String]): unit =
      try {
	compileSources(filenames map getSourceFile)
      } catch {
	case ex: IOException => error(ex.getMessage());
      }

    private def resetPackageClass(pclazz: Symbol): unit = {
      assert(pclazz.isPackageClass, pclazz);
      atPhase(firstPhase) {
	pclazz.setInfo(atPhase(typerPhase)(pclazz.info))
      }
      if (!pclazz.isRoot) resetPackageClass(pclazz.owner);
    }
  }

  def showDef(name: Name, module: boolean): unit = {
    def getSym(name: Name, module: boolean): Symbol = {
      var i = name.length - 1;
      while (i != 0 && name(i) != '#' && name(i) != '.') i = i - 1;
      if (i == 0)
        definitions.getModule(name)
      else {
        val root = getSym(name.subName(0, i), name(i) == '.');
        var selector = name.subName(i+1, name.length);
        if (module) selector = selector.toTypeName;
        root.info.member(selector)
      }
    }
    val sym = getSym(name, module);
    System.err.println("" + sym.name + ":" +
		       (if (module) sym.tpe.symbol.info else sym.info))
  }

  /** Returns the file with the given suffix for the given class. */
  def getFile(clazz: Symbol, suffix: String) = {
    val outdirname = settings.outdir.value;
    var outdir = new File(if (outdirname == "") "." else outdirname);
    val filename = clazz.fullNameString('.');
    var start = 0;
    var end = filename.indexOf('.', start);
    while (end >= start) {
      outdir = new File(outdir, filename.substring(start, end));
      if (!outdir.exists()) outdir.mkdir();
      start = end + 1;
      end = filename.indexOf('.', start);
    }
    new File(outdir, filename.substring(start) + suffix)
  }

  private def writeSymblFile(clazz: Symbol, pickled: PickleBuffer) = {
    val file = getFile(clazz, ".symbl");
    try {
      val stream = new FileOutputStream(file);
      stream.write(pickled.bytes, 0, pickled.writeIndex);
      stream.close();
      informProgress("wrote " + file);
    } catch {
      case ex: IOException =>
      if (settings.debug.value) ex.printStackTrace();
      error("could not write file " + file);
    }
  }

  private def writeICode(): Unit = {
    val printer = new icodePrinter.TextPrinter(null);
    icodes.classes.foreach((cls) => {
      val file = getFile(cls.symbol, ".icode");
      try {
        val stream = new FileOutputStream(file);
        printer.setWriter(new PrintWriter(stream, true));
        printer.printClass(cls);
        informProgress("wrote " + file);
      } catch {
        case ex: IOException =>
          if (settings.debug.value) ex.printStackTrace();
        error("could not write file " + file);
      }
    });
  }
}
