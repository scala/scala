/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac;

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
    //public final boolean optimize;
    public final boolean debug;
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

    /** The set of currenttly compiled top-level symbols
     */
    public HashMap/*<Symbol,Sourcefile>*/ compiledNow = new HashMap();

    /** the current phase
     */
    public PhaseDescriptor currentPhase;

    /** the global definitions
     */
    public Definitions definitions;

    /** the global primitives
     */
    public Primitives primitives;

    /** compilation phases.
     */
    public final PhaseRepository PHASE;
    public final PhaseDescriptor[] phases;

    public final int POST_ANALYZER_PHASE_ID = 3;

    /** compilation targets
     */
    public static final String TARGET_INT;
    public static final String TARGET_JVM;
    public static final String TARGET_MSIL;

    public static final String[] TARGETS = new String[] {
        TARGET_INT  = "int".intern(),
        TARGET_JVM  = "jvm".intern(),
        TARGET_MSIL = "msil".intern(),
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
        this.printtypes = args.types.value;
        this.printtokens = args.print.tokens;
        this.classPath = args.classpath();
        this.outpath = args.outpath();
        this.target = interpret ? TARGET_INT : args.target.value.intern();
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
        this.freshNameCreator = new FreshNameCreator();
        this.make = new TreeCreator();
        this.currentPhase = PhaseDescriptor.INITIAL;
        this.definitions = new Definitions(this);
        this.primitives = new Primitives(this);
        this.treeGen = new TreeGen(this, make);
        this.PHASE = args.phases;
        List phases = new ArrayList();
        phases.add(PHASE.INITIAL);
        phases.add(PHASE.PARSER);
        phases.add(PHASE.ANALYZER);
        phases.add(PHASE.REFCHECK);
        phases.add(PHASE.UNCURRY);
	/*
        if (optimize) {
            phases.add(PHASE.OPTIMIZE);
        } */
        phases.add(PHASE.TRANSMATCH);
        phases.add(PHASE.LAMBDALIFT);
        phases.add(PHASE.EXPLICITOUTER);
        phases.add(PHASE.ADDACCESSORS);
        phases.add(PHASE.ADDINTERFACES);
        phases.add(PHASE.EXPANDMIXIN);
        phases.add(PHASE.ERASURE);
	if (target == TARGET_INT || target == TARGET_MSIL || target == TARGET_JVM) {
            phases.add(PHASE.ADDCONSTRUCTORS);
        }
	/*
        if (target == TARGET_JAVA) phases.add(PHASE.GENJAVA);
        if (target == TARGET_MSIL) phases.add(PHASE.GENMSIL);
	*/
        if (target == TARGET_JVM) phases.add(PHASE.GENJVM);
        phases.add(PHASE.TERMINAL);
        this.phases = new PhaseDescriptor[phases.size()];
        for (int i = 0; i < phases.size(); i++) {
            PhaseDescriptor phase = (PhaseDescriptor)phases.get(i);
            this.phases[i] = phase;
            if (i > 0) this.phases[i - 1].flags |= phase.flags >>> 16;
            phase.initialize(this, i);
            assert phase.id == i;
        }
	assert PHASE.ANALYZER.id + 1 == POST_ANALYZER_PHASE_ID;
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
                units.add(new Unit(this, new Sourcefile(file), console));
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
    public void compile(String input, boolean console) {
        reporter.resetCounters();
        Sourcefile source = new Sourcefile(input.getBytes());
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
            if ((currentPhase.flags & PhaseDescriptor.SKIP) != 0) {
                operation("skipping phase " + currentPhase.name());
            } else {
                start();
                currentPhase.apply(this);
                stop(currentPhase.taskDescription());
            }
            if ((currentPhase.flags & PhaseDescriptor.PRINT) != 0)
                currentPhase.print(this);
            if ((currentPhase.flags & PhaseDescriptor.GRAPH) != 0)
                currentPhase.graph(this);
            if ((currentPhase.flags & PhaseDescriptor.CHECK) != 0)
                currentPhase.check(this);
            if ((currentPhase.flags & PhaseDescriptor.STOP) != 0) {
                operation("stopped after phase " + currentPhase.name());
                break;
            }
            if (currentPhase == PHASE.PARSER) fix1();
            if (currentPhase == PHASE.ANALYZER) fix2();
        }
        if (reporter.errors() != 0) {
	    imports.clear();
	    for (Iterator it = compiledNow.keySet().iterator(); it.hasNext();) {
		Symbol sym = (Symbol) it.next();
		Sourcefile f = (Sourcefile) compiledNow.get(sym);
		sym.reset(new SourceCompleter(this, f.filename));
	    }
	}
	compiledNow.clear();
        printer.end();
    }
    // !!! <<< Interpreter stuff
    public static final String CONSOLE_S = "$console$";
    private static final Name
        CONSOLE_N               = Name.fromString(CONSOLE_S),
        INTERPRETER_N           = Name.fromString("Interpreter"),
        SCALA_INTERPRETER_N     = Name.fromString("scala.Interpreter"),
        SHOW_DEFINITION_N       = Name.fromString("showDefinition"),
        SHOW_VALUE_DEFINITION_N = Name.fromString("showValueDefinition"),
        SHOW_VALUE_N            = Name.fromString("showValue");
    private Symbol INTERPRETER;
    private Symbol SHOW_VALUE;
    private Symbol SHOW_DEFINITION;
    private Symbol SHOW_VALUE_DEFINITION;

    private Symbol INTERPRETER() {
        if (INTERPRETER == null)
            INTERPRETER = definitions.getModule(SCALA_INTERPRETER_N);
        return INTERPRETER;
    }

    private Symbol SHOW_VALUE() {
        if (SHOW_VALUE == null)
            SHOW_VALUE = INTERPRETER().lookup(SHOW_VALUE_N);
        return SHOW_VALUE;
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

    private int module = 0;
    private List imports = new ArrayList();

    private void fix1() {
        for (int i = 0; i < units.length; i++) {
            if (units[i].console) fix1(units[i]);
        }
        for (int i = 0; i < imports.size(); i++) {
            Symbol module = (Symbol)imports.get(i);
            PHASE.ANALYZER.addConsoleImport(this, module);
        }
    }

    private void fix1(Unit unit) {
        unit.body = new Tree[] {
            make.ModuleDef(0, 0, Name.fromString(CONSOLE_S+module), Tree.Empty,
                make.Template(0, new Tree[]{
                    make.Apply(0,
                        make.Select(0,
                            make.Ident(0, Names.scala),
                            Names.Object.toConstrName()),
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
            case ModuleDef(_, Name name, _, Tree.Template impl):
                if (!name.startsWith(CONSOLE_N)) break;
                if (impl.body.length <= 0) break;
                imports.add(unit.body[i].symbol());
                Tree last = impl.body[impl.body.length - 1];
                if (last.isTerm()) {
                    impl.body[impl.body.length - 1] =
                        treeGen.Apply(
                            treeGen.Select(
                                treeGen.mkRef(0, INTERPRETER()),
                                SHOW_VALUE()),
                            new Tree[] {
                                last,
                                make.Literal(0, show(last.type())).setType(
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
        case TypeDef(_, _,_ ):
            if (!mustShow(tree.symbol())) return;
            body.append(
                treeGen.Apply(
                    treeGen.Select(
                        treeGen.mkRef(0, INTERPRETER()),
                        SHOW_DEFINITION()),
                    new Tree[] {
                        make.Literal(0, show(tree.symbol())).setType(
                            definitions.JAVA_STRING_TYPE)}));
            return;
        case ValDef(_, _, _, _):
            if (!mustShow(tree.symbol())) return;
            body.append(
                treeGen.Apply(
                    treeGen.Select(
                        treeGen.mkRef(0, INTERPRETER()),
                        SHOW_VALUE_DEFINITION()),
                    new Tree[] {
                        make.Literal(0, show(tree.symbol())).setType(
                            definitions.JAVA_STRING_TYPE),
                        treeGen.Ident(tree.symbol())}));
            return;
        default:
            return;
        }
    }

    private boolean mustShow(Symbol symbol) {
        return !symbol.isAccessor();
    }

    private String show(Symbol symbol) {
        return symbol.defString();
    }

    private String show(Type type) {
        return type.toString();
    }

    /*
    private StringBuffer appendPrefix(StringBuffer buffer, Type prefix) {
        if (prefix.isSameAs(definitions.ROOT_TYPE)) return buffer;
        if (prefix.isSameAs(definitions.SCALA_TYPE)) return buffer;
        if (prefix.symbol().name.startsWith(CONSOLE_N)) return buffer;
        return append(buffer, prefix).append('.');
    }
    */

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
        reporter.error("error: " + message);
    }

    /** issue a global warning
     */
    public void warning(String message) {
        reporter.warning("warning: " + message);
    }

    /** issue a global note (if in verbose mode)
     */
    public void note(String message) {
        reporter.note("note: " + message);
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
	    reporter.report("[log " + currentPhase.name() + "] " + message);
        }
        return true;
    }

    /** return true if logging is switched on for the current phase
     */
    public boolean log() {
	return (currentPhase.flags & PhaseDescriptor.LOG) != 0;
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
