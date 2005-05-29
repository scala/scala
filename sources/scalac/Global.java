/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac;

import ch.epfl.lamp.util.CodePrinter;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.IllegalCharsetNameException;
import java.nio.charset.UnsupportedCharsetException;
import java.util.*;

import scala.tools.util.AbstractFile;
import scala.tools.util.ClassPath;
import scala.tools.util.Position;
import scala.tools.util.SourceFile;
import scala.tools.util.SourceReader;
import scala.tools.util.Reporter;
import scala.tools.util.Timer;
import scala.tools.util.DummyTimer;
import scala.tools.util.ReporterTimer;

import scalac.ast.*;
import scalac.ast.parser.*;
import scalac.ast.printer.TreePrinter;
import scalac.atree.AConstant;
import scalac.atree.ATreePrinter;
import scalac.backend.Primitives;
// !!! <<< Interpreter stuff
import scalac.symtab.*;
// !!! >>> Interpreter stuff
import scalac.symtab.Definitions;
import scalac.symtab.classfile.PackageParser;
import scalac.symtab.classfile.CLRPackageParser;
import scalac.typechecker.AnalyzerPhase;
import scalac.typechecker.Infer;
import scalac.transformer.ICodePhase;
import scalac.util.*;

/** The global environment of a compiler run
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
public abstract class Global {

    public static Global instance;

    /** global options
     */
    public final boolean noimports;
    public final boolean nopredefs;
    public final boolean separate;
    //public final boolean optimize;
    public final boolean debug;
    public final boolean debuginfo;
    public final boolean explaintypes;
    public final boolean uniqid;
    public final boolean newMatch;
    public final boolean xmlPreserveWS;
    public final boolean runTimeTypes;

    public final boolean printtypes;
    public final boolean printtokens;

    public final String outpath;
    public final String encoding;
    public final String target;

    /** the message reporter
     */
    public final Reporter reporter;

    /** the timer
     */
    public final Timer timer;

    /** the source file charset
     */
    private final Charset charset;

    /** the source file decoder
     */
    private final CharsetDecoder decoder;

    /** the source file reader
     */
    private final SourceReader reader;

    /** the class path
     */
    public final ClassPath classPath;

    /** the global tree factory
     */
    public final TreeFactory make;

    /** the tree generator
     */
    public final TreeGen treeGen;

    /** the global tree printer
     */
    public final PrintWriter writer;
    public final TreePrinter treePrinter;

    /** documentation comments of symbols
     */
    public final Map/*<Symbol, String>*/ mapSymbolComment = new HashMap();

    /** attributes of symbols
     */
    protected final Map/*<Symbol, AttributeInfo>*/ mapSymbolAttr = new HashMap();

    /** views associated with (upper-bounded) type parameters
     */
    public final Map/*<Symbol, Symbol>*/ viewOfTypeParam = new HashMap();

    /** Pickled info of top-level symbols
     */
    public final Map/*<Symbol, Pickle>*/ symdata = new HashMap();


    /** The compiler command arguments
     */
    public final CompilerCommand args;

    /** The set of currently compiled top-level symbols
     */
    public HashMap/*<Symbol,Sourcefile>*/ compiledNow = new HashMap();

    /** The set of already compiled sourcefiles
     */
    private HashSet/*<SourceFile>*/ compiledUnits = new HashSet();

    /** the current phase
     */
    public Phase currentPhase;

    /** the global definitions
     */
    public Definitions definitions;

    /** the global primitives
     */
    public final Primitives primitives;

    /** compilation phases.
     */
    public final CompilerPhases PHASE;

    /** The current compilation loop or null */
    private CompilationLoop loop;

    /** compilation targets
     */
    public static final String TARGET_INT;
    public static final String TARGET_JVM;
    public static final String TARGET_MSIL;
    public static final String TARGET_JVMFROMICODE;

    public static final String[] TARGETS = new String[] {
        TARGET_INT          = "int",
        TARGET_JVM          = "jvm",
        TARGET_MSIL         = "msil",
	TARGET_JVMFROMICODE = "jvmfromicode"
    };

    /** tree printers
     */
    public static final String PRINTER_TEXT;
    public static final String PRINTER_HTML;
    public static final String PRINTER_SWING;

    public static final String[] PRINTERS = new String[] {
        PRINTER_TEXT = "text",
        PRINTER_HTML = "html",
	PRINTER_SWING = "swing",
    };

    public static Timer getTimer(Reporter reporter) {
        return reporter.verbose()
            ? (Timer)new ReporterTimer(reporter)
            : (Timer)DummyTimer.object;
    }

    /** hooks for installing printers
     */
    public abstract Infer newInfer();
    public abstract TreePrinter newTextTreePrinter(PrintWriter writer);
    public abstract TreePrinter newHTMLTreePrinter(PrintWriter writer);
    public abstract TreePrinter newSwingTreePrinter(PrintWriter writer);

    /**
     * Creates an instance variable.
     *
     * @param args
     * @param interpret
     */
    public Global(CompilerCommand args, Timer timer, boolean interpret) {
        assert Debug.initialize() || true;
        if (Global.instance != null) // jaco bug: can't use assert here
            /* throw */ Debug.abort("duplicate creation of Global");
        Global.instance = this;
        this.args = args;
        this.reporter = args.reporter();
        this.timer = timer;
        this.noimports = args.noimports.value;
        this.nopredefs = args.nopredefs.value;
        //this.optimize = args.optimize.optimize;
        this.debug = args.debug.value;
        this.debuginfo = args.debuginfo.value;
        this.uniqid = args.uniqid.value;
        this.newMatch = args.Xnewmatch.value;
        this.xmlPreserveWS = args.XpreserveWS.value;
        this.runTimeTypes = args.XrunTimeTypes.value;
        this.explaintypes = args.explaintypes.value;
        this.printtypes = args.types.value;
        this.printtokens = args.print.tokens;
        this.classPath = args.classpath();
	reporter.info(null, "classpath = " + classPath, false);
        this.outpath = args.outpath();
        String encoding = args.encoding.value;
        Charset charset = null;
        try {
            charset = Charset.forName(encoding);
        } catch (IllegalCharsetNameException exception) {
            args.encoding.error("illegal charset name '" + encoding + "'");
        } catch (UnsupportedCharsetException exception) {
            args.encoding.error("unsupported charset '" + encoding + "'");
        }
        if (charset == null) {
            encoding = "ISO-8859-1"; // A mandatory charset
            charset = Charset.forName(encoding);
        }
        this.encoding = encoding;
        this.charset = charset;
        this.decoder = charset.newDecoder();
        this.reader = new SourceReader(decoder);
        this.target = interpret ? TARGET_INT : args.target.value.intern();
        this.separate = args.separate.value.equals("yes") ||
            args.separate.value.equals("default") && !this.target.equals(TARGET_INT);
        String printFile = args.printfile.value;
        OutputStream stream;
        try {
            stream = "-".equals(printFile)
                ? System.out
                : new FileOutputStream(printFile);
        } catch (FileNotFoundException e) {
            error("unable to open file " + printFile + ". Printing on console");
            stream = System.out;
        }
        this.writer = new PrintWriter(stream, debug);
        if (args.printer.value.equals(PRINTER_HTML)) {
            this.treePrinter = newHTMLTreePrinter(writer);
	} else if (args.printer.value.equals(PRINTER_SWING)) {
	    this.treePrinter = newSwingTreePrinter(writer);
	} else {
            if (!args.printer.value.equals(PRINTER_TEXT))
                error("unknown printer kind: " +  args.printer.value);
            this.treePrinter = newTextTreePrinter(writer);
        }
        this.make = new DefaultTreeFactory();
        this.PHASE = args.phases;
        args.phases.WHOLEPROG.addSkipFlag(); // !!!
        // if (!optimize) PHASE.remove(args.phases.OPTIMIZE);
        // TODO: Enable TailCall for other backends when they handle LabelDefs
	if (target != TARGET_JVMFROMICODE) args.phases.ICODE.addSkipFlag();
        PHASE.freeze();
        PhaseDescriptor[] descriptors = PHASE.phases();
        int i = 0;
        for (; i < descriptors.length; i++) {
            if (!descriptors[i].hasSkipFlag()) descriptors[i].create(this);
            if (descriptors[i] == PHASE.ANALYZER) { i++; break; }
        }
        this.treeGen = ((AnalyzerPhase)PHASE.ANALYZER.phase()).gen;
        this.primitives = new Primitives(this);
        for (; i < descriptors.length; i++)
            if (!descriptors[i].hasSkipFlag()) descriptors[i].create(this);
        assert descriptors.length >= 2
            && descriptors[0] == PHASE.INITIAL
            && descriptors[descriptors.length - 1] == PHASE.TERMINAL:
            Debug.show(descriptors);
        assert !PHASE.INITIAL.hasSkipFlag();
        this.currentPhase = PHASE.INITIAL.phase();
    }

    /**
     * Moves to next phase.
     */
    public void nextPhase() {
        assert currentPhase.next != null;
        currentPhase = currentPhase.next;
    }

    /** Move to previous phase
     */
    public void prevPhase() {
        assert currentPhase.prev != null;
        currentPhase = currentPhase.prev;
    }

    /** Creates a virtual source file with given name and content. */
    public SourceFile getSourceFile(String sourcename, String content) {
        return new SourceFile(sourcename, content.toCharArray());
    }

    /** Reads and returns the source file in file with given name. */
    public SourceFile getSourceFile(String filename) throws IOException {
        AbstractFile file = AbstractFile.getFile(filename);
        if (file == null) throw new FileNotFoundException(
            "source file '" + filename + "' could not be found");
        return getSourceFile(file);
    }

    /** Reads and returns the source file in given abstract file. */
    public SourceFile getSourceFile(AbstractFile file) throws IOException {
        return new SourceFile(file, reader.read(file));
    }

    /** Reads and returns the source file of given clasz. */
    public SourceFile getSourceFile(Symbol clasz) throws IOException {
        assert clasz.isClass() && clasz.isStatic(): Debug.show(clasz);
        AbstractFile file = classPath.getRoot().lookupPath(
            SourceRepresentation.externalizeFileName(clasz, ".scala"), false);
        if (file == null) throw new FileNotFoundException(
            "source file for " + clasz + " could not be found");
        return getSourceFile(file);
    }

    /** Returns the root symbol loader. */
    public SymbolLoader getRootLoader() {
        return target == TARGET_MSIL
            ? new CLRPackageParser(this, classPath.getRoot())
            : new PackageParser(this, classPath.getRoot());
    }

    /** the top-level compilation process
     */
    public CompilationUnit[] compile(String[] files, boolean console) {
        reporter.resetCounters();
        // parse files
        List units = new ArrayList(files.length);
        for (int i = 0; i < files.length; i++) {
            try {
		SourceFile source = getSourceFile(files[i]);
                units.add(new CompilationUnit(this, source, console));
		compiledUnits.add(source);
            } catch (IOException exception) {
                error(exception.getMessage());
            }
        }
        CompilationUnit[] array = new CompilationUnit[units.size()];
        return compile((CompilationUnit[])units.toArray(array));
    }

    /**
     * The top-level compilation process.
     *
     * @param filename
     * @param input
     * @param console
     */
    public CompilationUnit[] compile(String filename, String input, boolean console) {
        reporter.resetCounters();
        SourceFile source = getSourceFile(filename, input);
	compiledUnits.add(source);
        CompilationUnit[] units = {new CompilationUnit(this, source, console)};
        return compile(units);
    }

    /** compile all compilation units
     */
    private CompilationUnit[] compile(CompilationUnit[] units) {
        this.currentPhase = PHASE.INITIAL.phase();
        treePrinter.begin();
        this.loop = new CompilationLoop(this);
        loadFunctions();
        units = loop.compile(units);
        this.loop = null;
        if (reporter.errors() != 0) {
            imports.clear();
            for (Iterator i = compiledNow.entrySet().iterator(); i.hasNext();) {
                Map.Entry entry = (Map.Entry)i.next();
                Symbol clasz = (Symbol)entry.getKey();
                AbstractFile file = ((SourceFile)entry.getValue()).getFile();
                clasz.reset(new SourceCompleter(this, file));
            }
        }
        compiledNow.clear();
        treePrinter.end();
        this.currentPhase = PHASE.TERMINAL.phase();
        return units;
    }

    protected abstract void loadFunctions();

    /** Compiles an additional source file. */
    public void compileLate(SourceFile source, boolean mixinOnly) {
        assert loop != null: source;
	if (!compiledUnits.contains(source)) {
	    compiledUnits.add(source);
	    CompilationUnit unit = new CompilationUnit(this, source, false, mixinOnly);
            loop.insert(unit);
	}
    }

    public void addAttribute(Symbol sym, Symbol aSym, AConstant[] params) {
        AttributeInfo attr = getAttributes(sym);
        attr = new AttributeInfo(aSym, params, attr);
        //mapSymbolAttr.put(sym, attr);
        setAttribute(sym, attr);
    }

    public void addAttribute(Symbol sym, Symbol aSym) {
        addAttribute(sym, aSym, AConstant.EMPTY_ARRAY);
    }

    public void setAttribute(Symbol sym, AttributeInfo attr) {
        mapSymbolAttr.put(sym, attr);
        if (sym.isModule() && !sym.isModuleClass())
            mapSymbolAttr.put(sym.moduleClass(), attr);
    }

    public AttributeInfo getAttributes(Symbol sym) {
        return (AttributeInfo)mapSymbolAttr.get(sym);
    }

    public AttributeInfo removeAttributes(Symbol sym) {
        return (AttributeInfo)mapSymbolAttr.remove(sym);
    }

    public abstract void dump(CompilationUnit[] units);

    void print(CompilationUnit[] units) {
        if (currentPhase.id == PHASE.MAKEBOXINGEXPLICIT.id()) {
            boolean html = args.printer.value.equals(PRINTER_HTML);
            if (html) writer.println("<pre>");
            ATreePrinter printer = new ATreePrinter(new CodePrinter(writer));
            boolean next = currentPhase.next != null;
            if (next) currentPhase = currentPhase.next;
            printer.printUnits(units);
            if (next) currentPhase = currentPhase.prev;
            if (html) writer.println("</pre>");
        } else if (currentPhase.id == PHASE.ICODE.id()) {
            Phase phase = currentPhase;
            boolean html = args.printer.value.equals(PRINTER_HTML);
            if (html) writer.println("<pre>");

            boolean next = currentPhase.next != null;
            if (next) currentPhase = currentPhase.next;
            ((ICodePhase)phase).print(units, new CodePrinter(writer));
            if (next) currentPhase = currentPhase.prev;
            if (html) writer.println("</pre>");
        } else {
            // go to next phase to print symbols with their new type
            boolean next = currentPhase.next != null;
            if (next) currentPhase = currentPhase.next;
            treePrinter.print(units);
            if (next) currentPhase = currentPhase.prev;
        }
    }

    // !!! <<< Interpreter stuff
    public static final String CONSOLE_S = "$console$";
    private static final Name
        SHOW_DEFINITION_N       = Name.fromString("showDefinition"),
        SHOW_VALUE_DEFINITION_N = Name.fromString("showValueDefinition"),
        SET_EVALUATION_RESULT_N = Name.fromString("setEvaluationResult");
    private Symbol INTERPRETER;
    private Symbol PRINTER;
    private Symbol SHOW_VALUE;
    private Symbol SET_EVALUATION_RESULT;
    private Symbol SHOW_DEFINITION;
    private Symbol SHOW_VALUE_DEFINITION;

    private Symbol INTERPRETER() {
        if (INTERPRETER == null)
            INTERPRETER = definitions.getModule("scala.runtime.InterpreterSupport");
        return INTERPRETER;
    }

    private Symbol SHOW_DEFINITION() {
        if (SHOW_DEFINITION == null)
            SHOW_DEFINITION = INTERPRETER().lookup(SHOW_DEFINITION_N);
        return SHOW_DEFINITION;
    }

    private Symbol SHOW_VALUE_DEFINITION() {
        if (SHOW_VALUE_DEFINITION == null)
            SHOW_VALUE_DEFINITION = INTERPRETER().lookup(SHOW_VALUE_DEFINITION_N);
        return SHOW_VALUE_DEFINITION;
    }

    private Symbol SET_EVALUATION_RESULT() {
        if (SET_EVALUATION_RESULT == null)
            SET_EVALUATION_RESULT = INTERPRETER().lookup(SET_EVALUATION_RESULT_N);
        return SET_EVALUATION_RESULT;
    }

    private int module = 0;
    private List imports = new ArrayList();
    public Symbol console;

    void fix1(CompilationUnit[] units) {
        for (int i = 0; i < units.length; i++) {
            if (units[i].console) fix1(units[i]);
        }
        nextPhase();
        for (int i = 0; i < imports.size(); i++) {
            Symbol module = (Symbol)imports.get(i);
            ((scalac.typechecker.AnalyzerPhase)PHASE.ANALYZER.phase()).addConsoleImport(module);
        }
        prevPhase();
    }

    private void fix1(CompilationUnit unit) {
        unit.body = new Tree[] {
            make.ModuleDef(Position.FIRSTPOS,
                0, Name.fromString(CONSOLE_S+module), Tree.Empty,
                make.Template(Position.FIRSTPOS, new Tree[]{
                    make.Apply(Position.FIRSTPOS,
                        make.Select(Position.FIRSTPOS,
                            make.Ident(Position.FIRSTPOS, Names.scala),
                            Names.AnyRef.toTypeName()),
                        new Tree[0])},
                    unit.body))};
        module++;
    }

    void fix2(CompilationUnit[] units) {
        for (int i = 0; i < units.length; i++) {
            if (units[i].console) fix2(units[i]);
        }
    }

    private void fix2(CompilationUnit unit) {
        imports.clear();
        for (int i = 0; i < unit.body.length; i++) {
            switch (unit.body[i]) {
            case ModuleDef(_, _, _, Tree.Template impl):
                Symbol symbol = unit.body[i].symbol();
                if (!symbol.name.toString().startsWith(CONSOLE_S)) break;
                console = symbol;
                if (impl.body.length <= 0) break;
                imports.add(unit.body[i].symbol());
                Tree last = impl.body[impl.body.length - 1];
                if (last != Tree.Empty && last.isTerm()) {
                    impl.body[impl.body.length - 1] =
                        treeGen.Apply(last.pos,
                            treeGen.Select(last.pos,
                                treeGen.mkGlobalRef(last.pos, INTERPRETER()),
                                SET_EVALUATION_RESULT()),
                            new Tree[] {
                                last,
                                treeGen.mkStringLit(
                                    last.pos, show(last.getType()))});
                }
                TreeList body = new TreeList();
                for (int j = 0; j < impl.body.length; j++)
                    fix2(body, impl.body[j]);
                impl.body = body.toArray();
                break;
            }
        }
    }

    private void fix2(TreeList body, Tree tree) {
        body.append(tree);
        switch (tree) {
        case PatDef(_, _, _): // !!! impossible (removed by analyzer)
            assert false : Debug.show(tree);
            return;
        case ClassDef(_, _, _, _, _, _):
        case PackageDef(_, _):
        case ModuleDef(_, _, _, _):
        case DefDef(_, _, _, _, _, _):
        case AbsTypeDef(_, _, _, _):
        case AliasTypeDef(_, _, _, _):
            if (!mustShow(tree.symbol())) return;
            body.append(
                treeGen.Apply(tree.pos,
                    treeGen.Select(tree.pos,
                        treeGen.mkGlobalRef(tree.pos, INTERPRETER()),
                        SHOW_DEFINITION()),
                    new Tree[] {
                        treeGen.mkStringLit(tree.pos, show(tree.symbol()))}));
            return;
        case ValDef(_, _, _, _):
            if (!mustShow(tree.symbol())) return;
            body.append(
                treeGen.Apply(tree.pos,
                    treeGen.Select(tree.pos,
                        treeGen.mkGlobalRef(tree.pos, INTERPRETER()),
                        SHOW_VALUE_DEFINITION()),
                    new Tree[] {
                        treeGen.mkStringLit(tree.pos, show(tree.symbol())),
                        treeGen.Ident(tree.pos, tree.symbol())}));
            return;
        default:
            return;
        }
    }

    private boolean mustShow(Symbol symbol) {
        return !symbol.isAccessor();
    }

    private String show(Symbol symbol) {
        return new InterpreterPrinter().printSignature(symbol).toString();
    }

    private String show(Type type) {
        return new InterpreterPrinter().printType(type).toString();
    }

    private class InterpreterPrinter extends SymbolTablePrinter {
        public InterpreterPrinter() {
            super("  ");
        }
        public String getSymbolName(Symbol symbol) {
            String string = super.getSymbolName(symbol);
            if (!debug) {
                int index = string.indexOf('$');
                if (index > 0) string = string.substring(0, index);
            }
            return string;
        }
    }

    // !!! >>> Interpreter stuff

    /** issue a global error
     */
    public void error(String message) {
        reporter.error(null, message);
    }

    /** issue a global warning
     */
    public void warning(String message) {
        reporter.warning(null, message);
    }

    /** issue an operation note
     */
    public void operation(String message) {
        reporter.info(null, "[" + message + "]", false);
    }

    /** issue a debug message from currentPhase
     */
    // the boolean return value is here to let one write "assert log( ... )"
    public boolean log(String message) {
        if (log()) {
            reporter.info(null, "[log " + currentPhase + "] " + message, true);
        }
        return true;
    }

    /** return true if logging is switched on for the current phase
     */
    public boolean log() {
        return currentPhase.descriptor.hasLogFlag();
    }

}
