/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac;

import java.util.List;
import java.util.ArrayList;

import scalac.util.ClassPath;
import scalac.util.Reporter;
import scalac.util.CommandParser;
import scalac.util.ArgumentParser;
import scalac.util.OptionParser;
import scalac.util.BooleanOptionParser;
import scalac.util.StringOptionParser;
import scalac.util.ChoiceOptionParser;
import scalac.util.HelpOptionParser;
import scalac.util.VersionOptionParser;
//import scalac.util.OptimizeOptionParser;
import scalac.util.PhaseSetOptionParser;
import scalac.util.PrintOptionParser;
import scalac.util.UnknownOptionParser;
import scalac.util.ScalaFileArgumentParser;
import scalac.util.UnknownArgumentParser;
import scalac.util.Strings;

public class CompilerCommand extends CommandParser {

    public final PhaseRepository phases;

    public final BooleanOptionParser nowarn;
    public final BooleanOptionParser verbose;
    public final BooleanOptionParser debug;
    public final BooleanOptionParser explaintypes;
    public final BooleanOptionParser uniqid;
    public final BooleanOptionParser types;
    public final BooleanOptionParser prompt;
    //public final OptimizeOptionParser optimize;
    public final StringOptionParser classpath;
    public final StringOptionParser sourcepath;
    public final StringOptionParser bootclasspath;
    public final StringOptionParser extdirs;
    public final StringOptionParser outpath;
    public final ChoiceOptionParser target;
    public final BooleanOptionParser noimports;
    public final BooleanOptionParser nopredefs;
    public final PhaseSetOptionParser skip;
    public final PhaseSetOptionParser check;
    public final PrintOptionParser print;
    public final ChoiceOptionParser printer;
    public final StringOptionParser printfile;
    public final PhaseSetOptionParser graph;
    public final PhaseSetOptionParser stop;
    public final PhaseSetOptionParser log;
    public final VersionOptionParser version;
    public final HelpOptionParser help;
    public final UnknownOptionParser unknown_options;
    public final ScalaFileArgumentParser files;
    public final UnknownArgumentParser unknown_arguments;

    public CompilerCommand(String product, String version,
        Reporter reporter, PhaseRepository phases)
    {
        this(product, version, "<source files>", reporter, phases);
    }

    protected CompilerCommand(String product, String version, String syntax,
        Reporter reporter, PhaseRepository phases)
    {
        super(product, version, syntax, reporter);
        this.phases = phases;

        ArgumentParser[] parsers = new ArgumentParser[] {

        this.nowarn = new BooleanOptionParser(this,
            "nowarn", "Generate no warnings",
            false),

        this.verbose = new BooleanOptionParser(this,
            "verbose", "Output messages about what the compiler is doing",
            false),

        this.debug = new BooleanOptionParser(this,
            "debug", "Output debugging messages",
            false),

        this.explaintypes = new BooleanOptionParser(this,
            "explaintypes", "Explain type errors in more detail",
            false),

        this.uniqid = new BooleanOptionParser(this,
            "uniqid", "Print identifiers with unique names (debugging option)",
            false),

        this.types = new BooleanOptionParser(this,
            "types", "Print tree types (debugging option)",
            false),

        this.prompt = new BooleanOptionParser(this,
            "prompt", "Display a prompt after each error (debugging option)",
            false),

        //this.optimize = new OptimizeOptionParser(this,
        //    "optimize", "optimize bytecode (-optimize:help for option list)",
	//    null /* todo: uncomment: phases.OPTIMIZE */),

        this.classpath = new StringOptionParser(this,
            "classpath", "Specify where to find user class files",
            "path", ClassPath.CLASS_PATH),

        this.sourcepath = new StringOptionParser(this,
            "sourcepath", "Specify where to find input source files",
            "path", ClassPath.SOURCE_PATH),

        this.bootclasspath = new StringOptionParser(this,
            "bootclasspath", "Override location of bootstrap class files",
            "path", ClassPath.BOOT_PATH),

        this.extdirs = new StringOptionParser(this,
            "extdirs", "Override location of installed extensions",
            "dirs", ClassPath.EXTENSION_PATH),

        this.outpath = new StringOptionParser(this,
            "d", "Specify where to place generated class files",
            "directory", "."),

        this.target = new ChoiceOptionParser(this,
            "target", "Specify which bakend to use (jvm, jvm-bcel, msil)",
            "target", Global.TARGETS, Global.TARGET_JVM),

        this.noimports = new BooleanOptionParser(this,
            "noimports", "Compile without any implicit imports",
            false),

        this.nopredefs = new BooleanOptionParser(this,
            "nopredefs", "Compile without any implicit predefined values",
            false),

        this.skip = new PhaseSetOptionParser(this,
            "skip", "Skip <phases> (see below)",
            phases.phases, PhaseDescriptor.SKIP),

        this.check = new PhaseSetOptionParser(this,
            "check", "Check the tree after <phases> (see below)",
            phases.phases, PhaseDescriptor.CHECK),

        this.print = new PrintOptionParser(this,
            "print", "Print out program after <phases> (see below)",
            phases.phases, PhaseDescriptor.PRINT),

        this.printer = new ChoiceOptionParser(this,
            "printer", "Printer to use",
            "printer", Global.PRINTERS, Global.PRINTER_TEXT),

        this.printfile = new StringOptionParser(this,
            "print-file", "Specify file in which to print trees",
            "file", "-"),

        this.graph = new PhaseSetOptionParser(this,
            "graph", "Graph the program after <phases> (see below)",
            phases.phases, PhaseDescriptor.GRAPH),

        this.stop = new PhaseSetOptionParser(this,
            "stop", "Stop after first phase in <phases> (see below)",
            phases.phases, PhaseDescriptor.STOP),

        this.log = new PhaseSetOptionParser(this,
            "log", "Log operations in <phases> (see below)",
            phases.phases, PhaseDescriptor.LOG),

        this.version = new VersionOptionParser(this,
            "version", "Print product version and exit",
            product() + " " + version() + " -- (c) 2002 LAMP/EPFL"),

        this.help = new HelpOptionParser(this,
            "help", "Print a synopsis of standard options"),

        this.unknown_options = new UnknownOptionParser(this),

        this.files = new ScalaFileArgumentParser(this),

        this.unknown_arguments = new UnknownArgumentParser(this),

        };

        for (int i = 0; i < parsers.length; i++) add(parsers[i]);
    }

    public boolean parse(String[] args) {
        boolean result = super.parse(args);
        reporter().nowarn = nowarn.value;
        reporter().verbose = verbose.value;
        reporter().prompt = prompt.value;
        return result;
    }

    public String getHelpMessage() {
        StringBuffer buffer = new StringBuffer(super.getHelpMessage());
        buffer.append(Strings.EOL);
        buffer.append("and possible compilation phases include:");
        buffer.append(Strings.EOL);
        List lines = new ArrayList(phases.phases.length);
        for (int i = 0; i < phases.phases.length; i++) {
            PhaseDescriptor phase = phases.phases[i];
            lines.add("  " + phase.name() + "\t  " + phase.description());
        }
        lines.add("  " + "all" + "\t  " + "matches all phases");
        buffer.append(Strings.format(lines));
        return buffer.toString();
    }

    public ClassPath classpath() {
        return new ClassPath(classpath.value, sourcepath.value,
            bootclasspath.value, extdirs.value);
    }

    public String outpath() {
        return outpath.value + (outpath.value.endsWith("/") ? "" : "/");
    }

}
