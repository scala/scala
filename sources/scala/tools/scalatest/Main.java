/*     ___ ____ ___   __   ___  _____
**    / _// __// _ | / /  / _ |/_  _/     Scala test
**  __\ \/ /__/ __ |/ /__/ __ | / /       (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_//_/
**
**  $Id$
*/

package scala.tools.scalatest;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;


/**
 *
 */
public class Main {

    //########################################################################
    // Private Constants

    private final static String PRODUCT   = "scalatest";
    private final static String VERSION   = "1.02";
    private final static String COPYRIGHT = "(c) 2002-03 LAMP/EPFL";
    private final static String AUTHORS   =
        "Philippe Altherr & Stephane Micheloud";

    private static String SCALA_RUNTIME  =
        System.getProperty("scala.runtime");
    private static String SCALA_BINPATH  =
        System.getProperty("scala.binpath");
    private static String SCALA_TESTPATH =
        System.getProperty("scala.testpath");

    private final static String NAME_DTDFILE     = "dtd";

    private final static String SUFFIX_OBJDIR    = "-obj";
    private final static String SUFFIX_CHECKFILE = ".check";
    private final static String SUFFIX_DTDFILE   = ".dtd";
    private final static String SUFFIX_LOGFILE   = ".log";
    private final static String SUFFIX_SCALAFILE = ".scala";
    private final static String SUFFIX_XMLFILE   = ".xml";

    private final static String HEADER_TESTING = "testing: ";
    private final static String HEADER_VERBOSE = "debug: ";

    /**
     * Color modes
     */
    private final int NONE = 0;
    private final int SOME = 1;
    private final int MANY = 2;

    /**
     * Test types
     */
    private final int AUTO = 0;
    private final int RUN  = 1;
    private final int INT  = 2;
    private final int JVM  = 3;
    private final int XML  = 4;
    private final int POS  = 5;
    private final int NEG  = 6;

    //########################################################################
    // Private Variables

    private boolean noRun    = false;
    private boolean showLog  = false;
    private boolean showDiff = false;
    private boolean failed   = false;
    private int verbose      = 0;
    private int errors       = 0;
//  private int successCount = 0;

    private boolean testAll  = true;
    private int testType     = AUTO;

    /**
     * Test files grouped by type
     */
    private List/*String*/ filesRUN = new ArrayList();
    private List/*String*/ filesINT = new ArrayList();
    private List/*String*/ filesJVM = new ArrayList();
    private List/*String*/ filesXML = new ArrayList();
    private List/*String*/ filesPOS = new ArrayList();
    private List/*String*/ filesNEG = new ArrayList();

    /**
     * Used to format output
     */
    private Console console = new Console();
    private StringBuffer blanks = new StringBuffer("    ");

    private String colorOutline;
    private String colorSuccess;
    private String colorFailure;
    private String colorWarning;
    private String colorNormal;

    private String statusSuccess = "  OK  ";
    private String statusFailed  = "FAILED";

    /**
     * Test paths
     */
    private String objDir = getTempDirectory(SCALA_TESTPATH);

    private final String filesDir = "files" + FileUtils.FILE_SEP;
    private final String runDir   = filesDir + "run" + FileUtils.FILE_SEP;
    private final String jvmDir   = filesDir + "jvm" + FileUtils.FILE_SEP;
    private final String xmlDir   = filesDir + "xml" + FileUtils.FILE_SEP;
    private final String posDir   = "pos" + FileUtils.FILE_SEP;
    private final String coursDir = "cours" + FileUtils.FILE_SEP;
    private final String negDir   = "neg" + FileUtils.FILE_SEP;

    /**
     * Scala tools
     */
    private String scalac    = getExecutable("scalac");
    private String scalarun  = getExecutable("scalarun");
    private String dtd2scala = getExecutable("dtd2scala");
//  private String scalainfo = getExecutable("scala-info");

    private String scalapath = SCALA_RUNTIME;

    /**
     * Command lines executed in the tests
     */
    private MessageFormat scalacCmdLine;
    private MessageFormat scalarunCmdLine;
    private MessageFormat dtd2scalaCmdLine;
    private MessageFormat javaCmdLine;

    /**
     * Test options
     */
    private String flags = "";
    private int color = MANY;

    //########################################################################
    // Private Static Functions

    private static String getExecutable(String name) {
        return SCALA_BINPATH + FileUtils.FILE_SEP + name;
    }

    private static String getTempDirectory(String defaultDir) {
        String tempDir = defaultDir;
        File dir = FileUtils.getTempDir();
        if (dir != null) {
            dir = new File(dir, PRODUCT);
            try {
                dir.mkdir();
                tempDir = dir.getCanonicalPath() + FileUtils.FILE_SEP;
            } catch (Exception e) {}
        }
        return tempDir;
    }

    //########################################################################
    // Private Functions

    private void abort(String message) {
        console.println(message);
        System.exit(1);
    }

    private void printUsage() {
        console.println("Usage: " + PRODUCT + " [OPTION]...");
    }

    private void printHelp() {
        printUsage();
        console.println();
        console.println("--auto          use filenames to select the test to run");
        console.println("--run           next files test the interpreter and all backends");
        console.println("--int           next files test the interpreter");
        console.println("--jvm           next files test the jvm backend");
        console.println("--xml           next files test the dtd2scala tool");
        console.println("--pos           next files test a compilation success");
        console.println("--neg           next files test a compilation failure");
        console.println("--no-run        run no test, use results of last run");
        console.println("--show-log      show output of failed tests");
        console.println("--show-diff     show differences between actual and expected output");
        console.println("--failed        test only files that failed last time");
        console.println("--verbose=LEVEL display debug information (LEVEL=0|1|2)");
        console.println("--errors=<int>  specify the number of expected errors");
        console.println("--socos=<path>  specify the socos command");
        console.println("--surus=<path>  specify the surus command");
        console.println("--scala=<path>  specify the scala runtime class path");
        console.println("--flags=<flags> specify flags to pass on to the executable");
        console.println("--color=USAGE   control the color usage (USAGE=none|some|many)");
        console.println("--objdir=<dir>  specify where to place generated files");
        console.println("--help, -?      display this help and exit");
        console.println("--version       output version information and exit");
    }

    private void printVersion() {
        console.println(PRODUCT + " " + VERSION + ", " + COPYRIGHT);
        console.println("Written by " + AUTHORS);
    }

    private void printOutline(String s) {
        console.print(colorOutline + s + colorNormal);
    }

    private void printSuccess(String s) {
        console.print(colorSuccess + s + colorNormal);
    }

    private void printFailure(String s) {
        console.print(colorFailure + s + colorNormal);
    }

    private void printStatus(String message, boolean success) {
        int n = 70 - message.length();
        while (blanks.length() < n) blanks.append(" ");
        if (n > 0)
            console.print(blanks.substring(blanks.length() - n));
        console.println((success) ? statusSuccess : statusFailed);
    }

    private void printLog(File log) {
        try {
           BufferedReader rd = new BufferedReader(new FileReader(log));
           String s;
           while ((s = rd.readLine()) != null)
               console.println(s);
        } catch (Exception e) {
           // FileNotFoundException or IOException
        }
    }

    private String getDiff(File logFile, String name) {
        String diff = "";
        File checkFile =
            new File(SCALA_TESTPATH, name + SUFFIX_CHECKFILE);
        if (checkFile.isFile()) {
            diff = FileUtils.compareFiles(logFile, checkFile);
        }
        return diff;
    }

    private abstract class TestCommand extends Command {
        private String message;
        protected boolean success;
        protected String name;
        protected File logFile;
        protected File outDir;

        TestCommand(Console console) { super(console); }

        protected void pre(String arg, boolean create) {
            int inx = arg.lastIndexOf('.');
            name = (inx < 0) ? arg : arg.substring(0, inx);
            if (testAll)
                name = SCALA_TESTPATH + FileUtils.FILE_SEP + name;
            if (verbose > 1)
                console.println(HEADER_VERBOSE + "diff "
                    + objDir + name + SUFFIX_LOGFILE + " "
                    + name + SUFFIX_CHECKFILE);
            message = HEADER_TESTING + arg;
            outDir = new File(objDir, name + SUFFIX_OBJDIR);
            FileUtils.createDir(outDir);
            logFile = new File(objDir, name + SUFFIX_LOGFILE);
            success = false;
        }

        protected void printMessage() {
            assert message != null;
            printOutline(message);
        }

        protected void post() {
            assert message != null;
            String diff = "";
            if (success) {
                diff = getDiff(logFile, name);
                success = diff.length() == 0;
            }
            printStatus(message, success);
            if (! success) {
                if (showLog)
                    printLog(logFile);
                if (showDiff)
                    console.println(diff);
            }
            if (outDir != null)
                FileUtils.deleteDir(outDir);
        }
    }

    private String makeCmdLine(MessageFormat f, Object[] objs) {
        String cmdLine = f.format(objs);
        if (verbose > 0)
            console.println(HEADER_VERBOSE + cmdLine);
        return cmdLine;
    }

    private class InterpretationCommand extends TestCommand {
        InterpretationCommand(Console console) { super(console); }

        public boolean run(String arg) {
            pre(arg, false);

            String cmdLine =
                makeCmdLine(scalarunCmdLine, new Object[]{ flags, arg });

            printMessage();
            try {
                success = execute(cmdLine, new FileOutputStream(logFile)) >= 0;
            } catch (FileNotFoundException e) {}

            post();
            return success;
        }
    }

    private class InterpretationTest extends Test {
        InterpretationTest(List[] groups) {
            super("Testing interpreter", groups);
        }
        public int run() {
            return filesCount - run(new InterpretationCommand(console));
        }
    }

    private class CompilationCommand extends TestCommand {
        CompilationCommand(Console console) { super(console); }

        public boolean run(String arg) {
            pre(arg, true);

            String outpath = objDir + name + SUFFIX_OBJDIR;
            String cmdLine1 = makeCmdLine(
                scalacCmdLine,
                new Object[]{ outpath , flags, "", name + SUFFIX_SCALAFILE });
            String cmdLine2 = makeCmdLine(
                javaCmdLine,
                new Object[]{ outpath, "" });

            printMessage();
            try {
                success = execute(cmdLine1) >= 0 &&
                    execute(cmdLine2, new FileOutputStream(logFile)) >= 0;
            } catch (FileNotFoundException e) {}

            post();
            return success;
        }
    }

    private class CompilationTest extends Test {
        CompilationTest(List[] groups) {
            super("Testing jvm backend", groups);
        }
        public int run() {
            return filesCount - run(new CompilationCommand(console));
        }
    }

    private class XMLCommand extends TestCommand {
        XMLCommand(Console console) { super(console); }

        public boolean run(String arg) {
            pre(arg, true);

            String outpath  = objDir + name + SUFFIX_OBJDIR;
            String dtdFile  = name + SUFFIX_DTDFILE;
            String objFile  = outpath + FileUtils.FILE_SEP
                + NAME_DTDFILE + SUFFIX_SCALAFILE;
            String xmlFile  = SCALA_TESTPATH + FileUtils.FILE_SEP
                + name + SUFFIX_XMLFILE;
            String cmdLine1 = makeCmdLine(
                dtd2scalaCmdLine,
                new Object[]{ outpath, dtdFile });
            String cmdLine2 = makeCmdLine(
                scalacCmdLine,
                new Object[]{ outpath, flags, objFile, name + SUFFIX_SCALAFILE });
            String cmdLine3 = makeCmdLine(
                javaCmdLine,
                new Object[]{ outpath, xmlFile });

            printMessage();
            try {
                success = execute(cmdLine1) >= 0
                    && execute(cmdLine2) >= 0
                    && execute(cmdLine3, new FileOutputStream(logFile)) >= 0;
            } catch (FileNotFoundException e) {}

            post();
            return success;
        }
    }

    private class XMLTest extends Test {
        XMLTest(List/*String*/ files) {
            super("Testing dtd2scala", new List[]{ files });
        }
        public int run() {
            return filesCount - run(new XMLCommand(console));
        }
    }

    private class SuccessCommand extends TestCommand {
        SuccessCommand(Console console) { super(console); }

        public boolean run(String arg) {
            pre(arg, true);

            String outpath  = objDir + name + SUFFIX_OBJDIR;
            String cmdLine = makeCmdLine(
                scalacCmdLine,
                new Object[]{ outpath, flags, "", name + SUFFIX_SCALAFILE });

            printMessage();
            try {
                success = execute(cmdLine, new FileOutputStream(logFile)) >= 0;
            } catch (FileNotFoundException e) {}

            post();
            return success;
        }
    }

    private class SuccessTest extends Test {
        SuccessTest(List/*String*/ files) {
            super("Testing compiler "
                + "(on files whose compilation should succeed)",
                new List[]{ files });
        }
        public int run() {
            return filesCount - run(new SuccessCommand(console));
        }
    }

    private class FailureCommand extends TestCommand {
        FailureCommand(Console console) { super(console); }

        public boolean run(String arg) {
            pre(arg, true);

            String outpath  = objDir + name + SUFFIX_OBJDIR;
            String cmdLine = makeCmdLine(
                scalacCmdLine,
                new Object[]{ outpath, flags, "", name + SUFFIX_SCALAFILE });

            printMessage();
            try {
                success = execute(cmdLine, new FileOutputStream(logFile)) >= 0;
            } catch (FileNotFoundException e) {}

            post();
            return success;
        }
    }

    private class FailureTest extends Test {
        FailureTest(List/*String*/ files) {
            super("Testing compiler "
                + "(on files whose compilation should fail)",
                new List[]{ files });
        }
        public int run() {
            return filesCount - run(new FailureCommand(console));
        }
    }

    private void testAll() {
        console.println("Test configuration");
        console.println("socos executable: " + scalac);
        console.println("surus executable: " + scalarun);
        console.println("scala runtime   : " + scalapath);

        Test.setConsole(console);

        int failure = 0;
        failure += new InterpretationTest(new List[]{ filesRUN, filesINT}).run();
        failure += new CompilationTest(new List[]{ filesRUN, filesJVM}).run();
        failure += new XMLTest(filesXML).run();
        failure += new SuccessTest(filesPOS).run();
        failure += new FailureTest(filesNEG).run();

        console.println();
        if (failure == 0)
            printSuccess("All tests were successful");
        else if (failure == 1)
            printFailure("There was 1 test that failed");
        else
            printFailure("There were " + failure + " tests that failed");
        console.println();
    }

    private void addFile(String name) {
       testAll = false;
       if (! new File(name).isFile())
           abort("File \"" + name + "\" doesn't exist");

       switch (testType) {
           case AUTO: break;
           case RUN: filesRUN.add(name); break;
           case INT: filesINT.add(name); break;
           case JVM: filesJVM.add(name); break;
           case XML: filesXML.add(name); break;
           case POS: filesPOS.add(name); break;
           case NEG: filesNEG.add(name); break;
           default: abort("unknown test type " + testType);
       }
    }

    private void addFiles(List/*String*/ fileList, String subDir) {
        File f = new File(SCALA_TESTPATH, subDir);
        if (f.isDirectory()) {
            String[] files = f.list(new FilenameFilter() {
                public boolean accept(File dir, String name) {
                    return name.endsWith(SUFFIX_SCALAFILE);
                }
            });
            for (int i = 0; i < files.length; i++)
                fileList.add(subDir + files[i]);
        }
    }

    private void addFiles(List/*String*/ fileList, String[] subDirs) {
        for (int i = 0; i < subDirs.length; i++)
            addFiles(fileList, subDirs[i]);
    }

    private String getOptionValue(String option, String regex) {
        int inx = option.indexOf('=');
        assert inx > 0;
        String name = option.substring(0, inx);
        String value = option.substring(inx + 1);
        if (value.length() == 0)
            abort("illegal empty value for option --" + name);
        else if (! (regex == null || value.matches(regex)))
            abort("unknown value for option --" + name + ": " + value);
        return value;
    }

    private int getColorOption(String option, String regex) {
        String value = getOptionValue(option, regex);
        return "nsm".indexOf(value.charAt(0)); // hack !
    }

    private String getPathOption(String option, String regex) {
        String value = getOptionValue(option, regex);
        File dir = new File(value);
        if (! dir.isDirectory())
            abort("Could not access directory '" + value + "'");
        return value;
    }

    private void initializeColors() {
        switch (color) {
        case MANY:
            colorOutline = Console.COLOR39;
            colorSuccess = Console.GREEN;
            colorFailure = Console.RED;
            colorWarning = Console.YELLOW;
            colorNormal  = Console.RESET;
            break;
        case SOME:
            colorOutline = Console.BOLD;
            colorSuccess = Console.RESET;
            colorFailure = Console.BOLD;
            colorWarning = Console.BOLD;
            colorNormal  = Console.RESET;
            break;
        default:
            colorOutline = "";
            colorSuccess = "";
            colorFailure = "";
            colorWarning = "";
            colorNormal  = "";
        }
        statusSuccess = "[" + colorSuccess + statusSuccess + colorNormal + "]";
        statusFailed  = "[" + colorFailure + statusFailed  + colorNormal + "]";
    }

    public Main(String[] args) {
        console.setOut(System.out);

        for (int i = 0; i < args.length; i++) {
            String opt = null, arg = null;
            if (args[i].matches("--[a-z]+(-[a-z]*)?(=.*)?"))
                opt = args[i].substring(2);
            else if (args[i].equals("-?"))
                opt = "help";
            else if (! args[i].startsWith("-"))
                arg = args[i];
            else
                abort("illegal argument " + args[i]);

            if (opt == null) addFile(arg);
            else if (opt.equals("auto")) testType = AUTO;
            else if (opt.equals("run"))  testType = RUN;
            else if (opt.equals("int"))  testType = INT;
            else if (opt.equals("jvm"))  testType = JVM;
            else if (opt.equals("xml"))  testType = XML;
            else if (opt.equals("pos"))  testType = POS;
            else if (opt.equals("neg"))  testType = NEG;

            else if (opt.equals("no-run"))    noRun = true;
            else if (opt.equals("show-log"))  showLog = true;
            else if (opt.equals("show-diff")) showDiff = true;
            else if (opt.equals("failed"))    failed = true;

            else if (opt.startsWith("verbose="))
                verbose = Integer.parseInt(getOptionValue(opt, "(0|1|2)"));
            else if (opt.startsWith("errors="))
                errors = Integer.parseInt(getOptionValue(opt, "[0-9]*"));
            else if (opt.startsWith("socos="))
                scalac = getOptionValue(opt, null);
            else if (opt.startsWith("surus="))
                scalarun = getOptionValue(opt, null);
            else if (opt.startsWith("scala="))
                scalapath = getOptionValue(opt, null);
            else if (opt.startsWith("flags="))
                flags = getOptionValue(opt, null);
            else if (opt.startsWith("color="))
                color = getColorOption(opt, "(none|some|many)");
            else if (opt.startsWith("objdir="))
                objDir = getPathOption(opt, null);
            else if (opt.equals("help")) {
                printHelp();
                System.exit(0);
            }
            else if (opt.equals("version")) {
                printVersion();
                System.exit(0);
            }
            else
                abort("unknown option " + opt);
        }

        initializeColors();

        scalacCmdLine = /* 0:outpath, 1:flags, 2:objfile, 3:arg */
            new MessageFormat(scalac + " -Xshortname -d {0} {1} {2} {3}");

        scalarunCmdLine = /* 0:flags, 1:arg */
            new MessageFormat(scalarun + " {0} "
                + SCALA_TESTPATH + FileUtils.FILE_SEP + "{1} -- Test");

        dtd2scalaCmdLine = /* 0:outpath, 1:arg */
            new MessageFormat(dtd2scala + " -d {0} {1} " + NAME_DTDFILE);

        javaCmdLine = /* 0:outpath, 1:arg */
            new MessageFormat("java -cp "
                + scalapath + FileUtils.PATH_SEP + "{0} Test {1}");

        if (testAll) {
            if (testType == RUN)
                addFiles(filesRUN, runDir);
            if (testType == AUTO || testType == INT)
                addFiles(filesINT, runDir);
            if (testType == AUTO || testType == JVM)
                addFiles(filesJVM, new String[]{ runDir, jvmDir });
            if (testType == AUTO || testType == XML)
                addFiles(filesXML, xmlDir);
            if (testType == AUTO || testType == POS)
                addFiles(filesPOS, new String[]{ posDir, coursDir });
            if (testType == AUTO || testType == NEG)
                addFiles(filesNEG, negDir);
        }
    }

    public static void main(String[] args) {
        new Main(args).testAll();
    }

}
