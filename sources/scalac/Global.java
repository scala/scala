/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac;

import ch.epfl.lamp.util.Position;
import ch.epfl.lamp.util.SourceFile;

import java.io.*;
import java.util.*;
import scalac.util.*;
import scalac.ast.*;
import scalac.ast.parser.*;
import scalac.symtab.Definitions;
import scalac.ast.printer.*;
import scalac.backend.Primitives;
// !!! <<< Interpreter stuff
import scalac.symtab.*;
// !!! >>> Interpreter stuff


/** The global environment of a compiler run
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
public class  Global {

    public static Global instance;

    /** global options
     */
    public final boolean noimports;
    public final boolean nopredefs;
    public final boolean separate;
    //public final boolean optimize;
    public final boolean debug;
    public final boolean explaintypes;
    public final boolean uniqid;

    public final boolean printtypes;
    public final boolean printtokens;

    public final String outpath;
    public final String target;

    /** the message reporter
     */
    public final Reporter reporter;

    /** a stack for maintaining timestamps
     */
    private final Stack startTimes = new Stack();

    /** all compilation units
     */
    public Unit[] units;

    /** the class path
     */
    public final ClassPath classPath;

    /** the global tree factory
     */
    public final TreeFactory make;

    /** the fresh name creator
     */
    public final FreshNameCreator freshNameCreator;

    /** the tree generator
     */
    public final TreeGen treeGen;

    /** the unique-id generator
     */
    public final UniqueID uniqueID;

    /** the global tree printer
     */
    public final TreePrinter printer;
    public OutputStream printStream;
    public final TreePrinter debugPrinter;

    /** documentation comments of trees
     */
    public final Map/*<Tree, String>*/ mapTreeComment = new HashMap();

    /** documentation comments of symbols
     */
    public final Map/*<Symbol, String>*/ mapSymbolComment = new HashMap();

    public final Map/*<FullName, Pickle>*/ symdata = new HashMap();

    /** scaladoc option (with docmodule and docmodulepath)
     */
    public final boolean doc;
    public final String docmodule;
    public final String docmodulePath;

    /** The set of currenttly compiled top-level symbols
     */
    public HashMap/*<Symbol,Sourcefile>*/ compiledNow = new HashMap();

    /** the current phase
     */
    public Phase currentPhase;

    /** the global definitions
     */
    public Definitions definitions;

    /** the global primitives
     */
    public Primitives primitives;

    /** compilation phases.
     */
    public final CompilerPhases PHASE;
    public final Phase[] phases;

    public static final int POST_ANALYZER_PHASE_ID = 3;

    /** compilation targets
     */
    public static final String TARGET_INT;
    public static final String TARGET_JVM;
    public static final String TARGET_JVM_BCEL;
    public static final String TARGET_MSIL;

    public static final String[] TARGETS = new String[] {
        TARGET_INT      = "int".intern(),
        TARGET_JVM      = "jvm".intern(),
        TARGET_JVM_BCEL = "jvm-bcel".intern(),
        TARGET_MSIL     = "msil".intern(),
    };

    /** tree printers
     */
    public static final String PRINTER_TEXT;
    public static final String PRINTER_HTML;

    public static final String[] PRINTERS = new String[] {
        PRINTER_TEXT = "text".intern(),
        PRINTER_HTML = "html".intern(),
    };

    public Global(CompilerCommand args) {
        this(args, false);
    }

    public Global(CompilerCommand args, boolean interpret) {
        if (Global.instance != null) { // jaco bug: can't use assert here
            new Error("Duplicate creation of Global").printStackTrace();
            System.exit(1);
        };
        Global.instance = this;
        this.reporter = args.reporter();
        this.start(); // timestamp to compute the total time
        this.noimports = args.noimports.value;
        this.nopredefs = args.nopredefs.value;
        //this.optimize = args.optimize.optimize;
        this.debug = args.debug.value;
        this.uniqid = args.uniqid.value;
        this.explaintypes = args.explaintypes.value;
        this.printtypes = args.types.value;
        this.printtokens = args.print.tokens;
        this.classPath = args.classpath();
        this.outpath = args.outpath();
        this.target = interpret ? TARGET_INT : args.target.value.intern();
        this.separate = args.separate.value.equals("yes") ||
	    args.separate.value.equals("default") && !this.target.equals(TARGET_INT);
        this.uniqueID = new UniqueID();
        String printFile = args.printfile.value;
        try {
            this.printStream = "-".equals(printFile)
                ? System.out
                : new FileOutputStream(printFile);
        } catch (FileNotFoundException e) {
            error("unable to open file " + printFile + ". Printing on console");
            this.printStream = System.out;
        }
        String printerName = args.printer.value.intern();
        if (printerName == PRINTER_TEXT)
            this.printer = new TextTreePrinter(printStream);
        else
            this.printer = new HTMLTreePrinter(printStream);
        this.debugPrinter = new TextTreePrinter(System.err, true);
	this.doc = args.doc.value;
	this.docmodule = args.docmodule.value;
	this.docmodulePath = args.docmodulePath.value;
        this.freshNameCreator = new FreshNameCreator();
        this.make = new DefaultTreeFactory();
        this.PHASE = args.phases;
        // if (!optimize) PHASE.remove(args.phases.OPTIMIZE);
        if (target != TARGET_MSIL) PHASE.remove(args.phases.GENMSIL);
        if (target != TARGET_JVM) PHASE.remove(args.phases.GENJVM);
        if (target != TARGET_JVM_BCEL) PHASE.remove(args.phases.GENJVM_BCEL);
        PHASE.freeze();
        PhaseDescriptor[] descriptors = PHASE.phases();
        this.phases = new Phase[descriptors.length];
        this.currentPhase = phases[0] = descriptors[0].create(this);
        this.definitions = new Definitions(this);
        this.primitives = new Primitives(this);
        this.treeGen = new TreeGen(this, make);
        for (int i = 1; i < descriptors.length; i++)
            this.currentPhase = phases[i] = descriptors[i].create(this);
        this.currentPhase = phases[0];

        assert PHASE.ANALYZER.id() + 1 == POST_ANALYZER_PHASE_ID;
    }

    /** Move to next phase
     */
    public void nextPhase() {
        currentPhase = phases[currentPhase.id + 1];
    }

    /** Move to previous phase
     */
    public void prevPhase() {
        currentPhase = phases[currentPhase.id - 1];
    }

    /** the top-level compilation process
     */
    public void compile(String[] files, boolean console) {
        reporter.resetCounters();
        // parse files
        List units = new ArrayList(files.length);
        for (int i = 0; i < files.length; i++) {
            String file = files[i];
            try {
                units.add(new Unit(this, new SourceFile(file), console));
            } catch (FileNotFoundException e) {
                error("file " + file + " not found");
            } catch (IOException e) {
                error(e.toString());
            }
        }
        this.units = (Unit[])units.toArray(new Unit[units.size()]);
        compile();
    }

    /** the top-level compilation process
     */
    public void compile(String filename, String input, boolean console) {
        reporter.resetCounters();
        SourceFile source = new SourceFile(filename, input.getBytes());
        units = new Unit[]{new Unit(this, source, console)};
        compile();
    }

    /** compile all compilation units
     */
    private void compile() {
        printer.begin();

        // apply successive phases and pray that it works
        for (int i = 0; i < phases.length && reporter.errors() == 0; ++i) {
            currentPhase = phases[i];
            start();
            currentPhase.apply(units);
            stop(currentPhase.descriptor.taskDescription());
            if (currentPhase.descriptor.hasPrintFlag())
                currentPhase.print(this);
            if (currentPhase.descriptor.hasGraphFlag())
                currentPhase.graph(this);
            if (currentPhase.descriptor.hasCheckFlag())
                currentPhase.check(this);
            if (currentPhase == PHASE.PARSER.phase()) fix1();
            if (currentPhase == PHASE.ANALYZER.phase()) fix2();
            if (currentPhase == PHASE.ANALYZER.phase() && doc) {
		DocModule.apply(this);
		operation("stopped after phase " + currentPhase);
                break;
	    }
        }
        if (reporter.errors() != 0) {
            imports.clear();
            for (Iterator it = compiledNow.keySet().iterator(); it.hasNext();) {
                Symbol sym = (Symbol) it.next();
                sym.reset(new SourceCompleter(this));
            }
        }
	symdata.clear();
        compiledNow.clear();
        printer.end();
    }

    /** transform a unit and stop at the current compilation phase
     */
    public void transformUnit(Unit unit) {
       	Phase oldCurrentPhase = currentPhase;
    	int i = PHASE.REFCHECK.id(); // or PHASE.UNCURRY.id?
        while ((i < oldCurrentPhase.id) && (reporter.errors() == 0)) {
            currentPhase = phases[i];
            start();
            currentPhase.apply(new Unit[] {unit}); // !!! pb with Analyzer
            stop(currentPhase.descriptor.taskDescription());
            if (currentPhase.descriptor.hasPrintFlag())
                currentPhase.print(this);
            if (currentPhase.descriptor.hasGraphFlag())
                currentPhase.graph(this);
            if (currentPhase.descriptor.hasCheckFlag())
                currentPhase.check(this);
        }
        currentPhase = oldCurrentPhase;
    }

    // !!! <<< Interpreter stuff
    public static final String CONSOLE_S = "$console$";
    private static final Name
        CONSOLE_N               = Name.fromString(CONSOLE_S),
        INTERPRETER_N           = Name.fromString("scala.runtime.InterpreterSupport"),
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
            INTERPRETER = definitions.getModule(INTERPRETER_N);
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

    private void fix1() {
        for (int i = 0; i < units.length; i++) {
            if (units[i].console) fix1(units[i]);
        }
        for (int i = 0; i < imports.size(); i++) {
            Symbol module = (Symbol)imports.get(i);
            ((scalac.typechecker.AnalyzerPhase)PHASE.ANALYZER.phase()).addConsoleImport(this, module);
        }
    }

    private void fix1(Unit unit) {
        unit.body = new Tree[] {
            make.ModuleDef(Position.FIRSTPOS,
                0, Name.fromString(CONSOLE_S+module), Tree.Empty,
                make.Template(Position.FIRSTPOS, new Tree[]{
                    make.Apply(Position.FIRSTPOS,
                        make.Select(Position.FIRSTPOS,
                            make.Ident(Position.FIRSTPOS, Names.scala),
                            Names.Object.toTypeName()),
                        new Tree[0])},
                    unit.body))};
        module++;
    }

    private void fix2() {
        for (int i = 0; i < units.length; i++) {
            if (units[i].console) fix2(units[i]);
        }
    }

    private void fix2(Unit unit) {
        imports.clear();
        for (int i = 0; i < unit.body.length; i++) {
            switch (unit.body[i]) {
            case ModuleDef(_, _, _, Tree.Template impl):
                Symbol symbol = unit.body[i].symbol();
                if (!symbol.name.startsWith(CONSOLE_N)) break;
                console = symbol;
                if (impl.body.length <= 0) break;
                imports.add(unit.body[i].symbol());
                Tree last = impl.body[impl.body.length - 1];
                if (last.isTerm()) {
                    impl.body[impl.body.length - 1] =
                        treeGen.Apply(last.pos,
                            treeGen.Select(last.pos,
                                treeGen.mkRef(last.pos, INTERPRETER()),
                                SET_EVALUATION_RESULT()),
                            new Tree[] {
                                last,
                                make.Literal(last.pos,
                                    show(last.type())).setType(
                                        definitions.JAVA_STRING_TYPE)});
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
                        treeGen.mkRef(tree.pos, INTERPRETER()),
                        SHOW_DEFINITION()),
                    new Tree[] {
                        make.Literal(tree.pos, show(tree.symbol())).setType(
                            definitions.JAVA_STRING_TYPE)}));
            return;
        case ValDef(_, _, _, _):
            if (!mustShow(tree.symbol())) return;
            body.append(
                treeGen.Apply(tree.pos,
                    treeGen.Select(tree.pos,
                        treeGen.mkRef(tree.pos, INTERPRETER()),
                        SHOW_VALUE_DEFINITION()),
                    new Tree[] {
                        make.Literal(tree.pos, show(tree.symbol())).setType(
                            definitions.JAVA_STRING_TYPE),
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
        public String getSymbolFullName(Symbol symbol) {
            String string = super.getSymbolFullName(symbol);
            if (!debug) {
                int index = string.indexOf('$');
                if (index > 0) string = string.substring(0, index);
            }
            return string;
        }
    }

    // !!! >>> Interpreter stuff

    /** stop the compilation process immediately
     */
    public Error fail() {
        throw new ApplicationError();
    }

    /** stop the compilation process immediately
     */
    public Error fail(String message) {
        throw new ApplicationError(message);
    }

    /** stop the compilation process immediately
     */
    public Error fail(String message, Object object) {
        throw new ApplicationError(message, object);
    }

    /** stop the compilation process immediately
     */
    public Error fail(Object value) {
        throw new ApplicationError(value);
    }

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
        reporter.inform("[" + message + "]");
    }

    /** issue a debug message from currentPhase
     */
    // the boolean return value is here to let one write "assert log( ... )"
    public boolean log(String message) {
        if (log()) {
            reporter.report("[log " + currentPhase + "] " + message);
        }
        return true;
    }

    /** return true if logging is switched on for the current phase
     */
    public boolean log() {
        return currentPhase.descriptor.hasLogFlag();
    }

    /** start a new timer
     */
    public void start() {
        startTimes.push(new Long(System.currentTimeMillis()));
    }

    /** issue timing information
     */
    public void stop(String message) {
        long start = ((Long)startTimes.pop()).longValue();
        reporter.inform("[" + message + " in " +
                (System.currentTimeMillis() - start) + "ms]");
    }
}
