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
import util.ListBuffer;
import ast._;
import ast.parser._;
import typechecker._;
import matching.TransMatcher;
import transform._;
import backend.icode.{ICodes, GenICode};

class Global(val settings: Settings, val reporter: Reporter) extends SymbolTable
                                                             with Trees
                                                             with CompilationUnits
                                                             with ICodes {

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

  val copy = new LazyTreeCopier();

  type AttrInfo = Pair[Type, List[Any]];

  /** A map from symbols to their attributes */
  val attributes = new HashMap[Symbol, List[AttrInfo]];

// reporting -------------------------------------------------------

  def error(msg: String) = reporter.error(null, msg);
  def warning(msg: String) = reporter.warning(null, msg);
  private def inform(msg: String) = reporter.info(null, msg, true);

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

  override def phase_=(p: Phase): unit = {
    assert(p.id <= globalPhase.id + 1);
    super.phase_=(p)
  }

  abstract class GlobalPhase(prev: Phase) extends Phase(prev) {
    def run: unit = units foreach applyPhase;
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

  object syntheticMethods extends SyntheticMethods {
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

  object sampleTransform extends SampleTransform {
    val global: Global.this.type = Global.this;
  }

  object genicode extends GenICode {
    val global: Global.this.type = Global.this;
  }

  object icodePrinter extends backend.icode.Printers {
    val global: Global.this.type = Global.this;
  }

  def phaseDescriptors: List[SubComponent] = List(
    analyzer.namerFactory, // needs to be first
    analyzer.typerFactory, // needs to be second
    pickler,
    syntheticMethods,
    refchecks,
    uncurry,
    tailCalls,
    transMatcher,
    explicitOuter,
    erasure,
    lambdaLift,
    flatten,
    constructors,
    if (settings.Xshowicode.value) genicode
    else sampleTransform);

  val parserPhase = syntaxAnalyzer.newPhase(NoPhase);
  val firstPhase = parserPhase;
  phase = parserPhase;
  definitions.init; // needs firstPhase and phase to be defined != NoPhase,
	            // that's why it is placed here.

  private var p = phase;
  private var stopped = false;
  for (val pd <- phaseDescriptors) {
    if (!stopped) {
      if (!(settings.skip contains pd.phaseName)) p = pd.newPhase(p);
      stopped = settings.stop contains pd.phaseName;
    }
  }

  val terminalPhase = new GlobalPhase(p) {
    def name = "terminal";
    def apply(unit: CompilationUnit): unit = {}
  }

  def phaseNamed(name: String): Phase = {
    var p: Phase = firstPhase;
    while (p.next != p && p.name != name) p = p.next;
    if (p.name != name) NoPhase else p
  }

  val namerPhase = phaseNamed("namer");
  val typerPhase = phaseNamed("typer");
  val refchecksPhase = phaseNamed("refchecks");
  val erasurePhase = phaseNamed("erasure");

  val typer = new analyzer.Typer(analyzer.NoContext.make(EmptyTree, definitions.RootClass, new Scope())) {
    override def typed(tree: Tree, mode: int, pt: Type): Tree = {
      if (settings.debug.value) log("typing [[" + tree + "]]");
      val result = super.typed(tree, mode, pt);
      if (settings.debug.value) log(" ==> " + result + ":" + result.tpe);
      result
    }
  }
  val infer = typer.infer;

// Units and how to compile them -------------------------------------

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
    unitbuf.clear;
    fileset.clear;
    symSource.clear;
    symData.clear;
    reporter.resetCounters();
    for (val source <- sources)
      addUnit(new CompilationUnit(source));

    globalPhase = NoPhase.next;
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
        checker.checkTrees;
      }
    }
    if (settings.Xshowcls.value != "") showDef(newTermName(settings.Xshowcls.value), false);
    if (settings.Xshowobj.value != "") showDef(newTermName(settings.Xshowobj.value), true);
    if (settings.Xshowicode.value) printICode();

    if (reporter.errors() == 0) {
      for (val Pair(sym, pickled) <- symData.elements.toList) {
	sym.pos = Position.NOPOS;
	if (symData contains sym) {
	  symData -= sym;
	  symData -= sym.linkedSym;
	  writeSymblFile(sym, pickled)
	}
      }
    } else {
      for (val Pair(sym, file) <- symSource.elements) {
	sym.reset(new loaders.SourcefileLoader(file));
	if (sym.isTerm) sym.moduleClass.reset(loaders.errorLoader);
      }
    }
    informTime("total", startTime);
    informStatistics;
  }

  def compileLate(file: AbstractFile): unit =
    if (fileset == null)
      throw new FatalError("No symbol file for " + file + " was found\n(This file cannot be loaded as a source file)");
    else if (!(fileset contains file)) {
      val unit = new CompilationUnit(getSourceFile(file));
      addUnit(unit);
      var localPhase = parserPhase.asInstanceOf[GlobalPhase];
      while (localPhase.id < globalPhase.id || localPhase.id <= parserPhase.next.id) {
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
  private def getFile(clazz: Symbol, suffix: String) = {
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

  private def printICode(): Unit = {
    val printer = new icodePrinter.TextPrinter(new PrintWriter(System.out, true));
    classes.foreach(printer.printClass);
  }

  private def informStatistics = {
    inform("#identifiers : " + analyzer.idcnt);
    inform("#selections  : " + analyzer.selcnt);
    inform("#applications: " + analyzer.appcnt);
    inform("#implicits   : " + analyzer.implcnt);
    inform("#typecreates : " + accesses);
    inform("#uniquetypes : " + uniques);
    inform("#collisions  : " + collisions);
  }
}
