package scala.tools.nsc;

import java.io._;
import java.nio.charset._;
import scala.tools.util._;
import scala.collection.mutable.{HashSet,HashMap,ListBuffer}

import symtab._;
import symtab.classfile.Pickle;
import util._;
import ast._;
import ast.parser._;

class Global(val settings: Settings, val reporter: Reporter)
 extends SymbolTable
    with Trees
    with CompilationUnits {

// sub-components --------------------------------------------------

  object treePrinters extends TreePrinters {
    val global: Global.this.type = Global.this
  }
  val treePrinter = treePrinters.create();

  object treeInfo extends TreeInfo {
    val global: Global.this.type = Global.this
  }

// reporting -------------------------------------------------------

  def informTime(msg: String, start: long) = {
    val end = System.currentTimeMillis();
    reporter.info(null, "[" + msg + " in " + (end - start) + "ms]", false);
  }

  def error(msg: String) = reporter.error(null, msg);
  def warning(msg: String) = reporter.warning(null, msg);
  def inform(msg: String) = reporter.info(null, msg, true);

  def log(msg: String) =
    if (settings.log contains phase.name) inform("[log " + phase + "] " + msg);

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

  private object loaders extends SymbolLoaders {
    val global: Global.this.type = Global.this
  }

  def rootLoader: LazyType = loaders.packageLoader(classPath.getRoot());

// Phases ------------------------------------------------------------

  object parserPhase extends ParserPhase(NoPhase) {
    val global: Global.this.type = Global.this
  }
  val analyzerPhase = new StdPhase(parserPhase) {
    def name = "analyzer";
    val global: Global.this.type = Global.this;
    def apply(unit: CompilationUnit): unit = {}
  }

  val terminalPhase = new StdPhase(analyzerPhase) {
    def name = "terminal";
    val global: Global.this.type = Global.this;
    def apply(unit: CompilationUnit): unit = {}
  }

  definitions.init;

// Units and how to compile them -------------------------------------

  private var unitbuf = new ListBuffer[CompilationUnit];
  private var fileset = new HashSet[AbstractFile];

  private def addUnit(unit: CompilationUnit): unit = {
    unitbuf += unit;
    fileset += unit.source.getFile();
  }

  def units: Seq[CompilationUnit] = unitbuf;

  /** A map from compiled top-level symbols to their source files */
  val symSource = new HashMap[Symbol, AbstractFile];

  /** A map from compiled top-level symbols to their picklers */
  val symData = new HashMap[Symbol, Pickle];

  def compileSources(sources: List[SourceFile]): unit = {
    val startTime = System.currentTimeMillis();
    unitbuf.clear;
    fileset.clear;
    symSource.clear;
    symData.clear;
    reporter.resetCounters();
    for (val source <- sources)
      addUnit(new CompilationUnit(source));
    phase = NoPhase.next;
    while (phase != terminalPhase && reporter.errors() == 0) {
      val startTime = System.currentTimeMillis();
      if (!(settings.skip contains phase.name)) phase.run;
      if (settings.print contains phase.name) treePrinter.printAll();
      informTime(phase.description, startTime);
      phase = if (settings.stop contains phase.name) terminalPhase else phase.next;
    }
    if (settings.Xshowcls.value != "") showDef(newTermName(settings.Xshowcls.value), false);
    if (settings.Xshowobj.value != "") showDef(newTermName(settings.Xshowobj.value), true);

    if (reporter.errors() != 0)
      for (val Pair(sym, file) <- symSource.elements)
	sym.reset(loaders.sourcefileLoader(file));
    informTime("total", startTime);
  }

  def compileLate(file: AbstractFile, mixinOnly: boolean): unit = {
    if (!(fileset contains file)) {
      val unit = new CompilationUnit(getSourceFile(file), mixinOnly);
      addUnit(unit);
      atPhase(parserPhase) { parserPhase.apply(unit) }
      //atPhase(analyzerPhase) { analyzerPhase.lateEnter(unit) }
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
        root.info.lookup(selector)
      }
    }
    val sym = getSym(name, module);
    System.err.println("" + sym.name + ":" +
		       (if (module) sym.moduleClass.info else sym.info))
  }
}
