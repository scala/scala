/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Interpreter.java,v 1.63 2002/09/13 01:50:30 paltherr Exp $
// $Id$

package scalai;

import java.io.StringReader;
import java.io.PushbackReader;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

import scalac.Global;
import scalac.ast.parser.Sourcefile;
import scalac.util.PrefixMatcher;
import scalac.util.Strings;
import scalac.util.Debug;

public class Interpreter {

    //########################################################################
    // Public Constants

    public static final String PRODUCT   = Main.PRODUCT;
    public static final String VERSION   = Main.VERSION;
    public static final String COPYRIGHT = "(c) 2002, LAMP/EPFL";

    //########################################################################
    // Private Constants

    private static final int QUIT    = 0;
    private static final int LOAD    = 1;
    private static final int USE     = 2;
    private static final int GC      = 3;
    private static final int MEMORY  = 4;
    private static final int SHOWVER = 5;
    private static final int HELP    = 6;

    //########################################################################
    // Private Fields

    private final Global global;
    private final boolean interactive;
    private final boolean emacs;
    private final BufferedReader reader;
    private final PrintWriter writer;
    private final PrefixMatcher matcher;
    private final Evaluator evaluator;
    private final Compiler compiler;

    private String[] lfiles;
    private String[] ufiles;

    //########################################################################
    // Protected Methods

    protected PrefixMatcher matcher() {
        PrefixMatcher matcher = new PrefixMatcher();
        matcher.insert(":quit", new Integer(QUIT),
            "Quit interpreter");
        matcher.insert(":load", new Integer(LOAD), "<files>",
            "Load files (or repeat last use if no files are given)");
        matcher.insert(":use", new Integer(USE), "<files>",
            "Use files (or repeat last use if no files are given)");
        matcher.insert(":gc", new Integer(GC),
            "Run garbage collector");
        matcher.insert(":memory", new Integer(MEMORY),
            "Print out memory usage");
        matcher.insert(":help", new Integer(HELP),
            "Print out this command summary");
        matcher.insert(":version", new Integer(SHOWVER),
            "Print out product version");
        matcher.insert(":?", new Integer(HELP),
            "Same as :help");
        return matcher;
    }

    //########################################################################
    // Public Constructors

    public Interpreter(InterpreterCommand command) {
        this(command, new BufferedReader(new InputStreamReader(System.in)),
            new PrintWriter(System.out, true));
    }

    public Interpreter(InterpreterCommand command, BufferedReader reader,
        PrintWriter writer)
    {
        this.global = new Global(command, true);
        this.interactive = command.interactive.value;
        this.emacs = command.emacs.value;
        this.reader = reader;
        this.writer = writer;
        this.matcher = matcher();
        this.evaluator = new Evaluator();
        this.compiler = new Compiler(global,evaluator,new SymbolWriter(writer)); // !!!
        this.lfiles = new String[0];
        this.ufiles = new String[0];
    }

    //########################################################################
    // Public Methods

    public void main(String[] program, String main, String[] args) {
        if (interactive) showBanner();
        if (program.length > 0) load(lfiles = program);
        if (global.reporter.errors() == 0 && main != null) call(main, args);
        // Compute something to start compiler and force loading of Predef
        if (interactive && program.length == 0 && main == null) eval("null");
        if (interactive) while (handle(read()));
        global.stop("total");
        if (!interactive) global.reporter.printSummary();
    }

    public String read() {
        writer.print("> ");
        writer.flush();
        StringBuffer buffer = new StringBuffer();
        for (boolean stop = false; !stop; ) {
            String line;
            try {
                line = reader.readLine();
            } catch (IOException exception) {
                writer.println();
                error(exception.toString());
                System.exit(1);
                throw Debug.abort(exception);
            }

            if (line == null) { writer.println(); break; }
            if (emacs && line.equals("emacs:end")) break;

            if (!emacs) {
                line = Strings.trimTrailing(line);
                int lastChar = Strings.lastChar(line);
                stop = ",;:({=|\\".indexOf(lastChar) < 0;
                if (lastChar == '\\') line = line.substring(0,line.length()-1);
            }

            buffer.append(line).append('\n');

            if (!emacs && !stop) {
                writer.print("| ");
                writer.flush();
            }
        }
        return buffer.length() == 0 ? null : buffer.toString();
    }

    public boolean handle(String input) {
        if (input == null) return exec(QUIT);
        String trimed = input.trim();
        return trimed.startsWith(":") ? exec(trimed) : eval(input);
    }

    public boolean exec(String input) {
        int length = input.indexOf(' ');
        length = length < 0 ? input.length() : length;
        String command = input.substring(0, length);
        PrefixMatcher.Entry[] entries = matcher.lookup(command);
        if (entries.length != 1) {
            error(matcher.getErrorMessage(command,entries,"command"));
            return true;
        }
        try {
            String[] arguments = readArguments(input.substring(length));
            return exec(((Integer)entries[0].value).intValue(), arguments);
        } catch (IOException exception) {
            error(exception.getMessage());
            return true;
        }
    }

    public boolean exec(int command) {
        return exec(command, new String[0]);
    }

    public boolean exec(int command, String[] args) {
        switch (command) {
        case QUIT:
            writer.println("[Leaving " + PRODUCT + "]");
            return false;

        case LOAD:
            if (args.length != 0) lfiles = args;
            return load(lfiles);

        case USE:
            if (args.length != 0) ufiles = args;
            return eval(ufiles);

        case GC:
            System.gc();
            return true;

        case MEMORY:
            Runtime rt = Runtime.getRuntime();
            writer.println("total memory: " + rt.totalMemory());
            writer.println("free memory : " + rt.freeMemory());
            return true;

        case SHOWVER:
            writer.println(PRODUCT + " " + VERSION + " -- " + COPYRIGHT);
            return true;

        case HELP:
            writer.println("interpreter commands:");
            writer.println(Strings.format(matcher.getHelpStrings(" ","\t  ")));
            return true;

        default:
            throw Debug.abort("unknown command " + command);
        }
    }

    public boolean eval(String input) {
        if (input.trim().length() == 0) return true;
        global.compile(input, true);
        return interpret(true);
    }

    public boolean eval(String[] files) {
        if (files.length == 0) return true;
        global.compile(files, true);
        return interpret(true);
    }

    public boolean load(String[] files) {
        if (files.length == 0) return true;
        global.compile(files, false);
        return interpret(false);
    }

    public boolean call(String main, String[] args) {
        new EntryPointCompiler(PRODUCT, global).compile(main, args);
        return interpret(false);
    }

    public boolean interpret(boolean interactive) {
        if (global.reporter.errors() == 0) {
            CodeContainer code = compiler.compile(global.units, interactive);
            EvaluatorResult result;
            try {
                result = EvaluatorResult.Value(evaluator.evaluate(code));
            } catch (EvaluatorException exception) {
                result = EvaluatorResult.Error(exception);
            }
            show(result, interactive);
        }
        Sourcefile.flushSources();
        return true;
    }

    public void show(EvaluatorResult result, boolean interactive) {
        switch (result) {
        case Void:
            return;
        case Value(Object value):
            // !!! if (value != null)
            if (interactive && value != null) writer.println(value);
            return;
        case Error(EvaluatorException exception):
            writer.println(exception.mkString(global));
            return;
        }
    }

    public void showBanner() {
        writer.println("  __ __  _      _");
        writer.println(" (_ /   /_||   /_|   INTERPRETER");
        writer.println("___)\\__/  ||__/  |   " + COPYRIGHT);
        writer.println(" ");
        writer.println(" version: " + VERSION);
        writer.println(" type :? to get a list of all interpreter commands");
        writer.println(" ");
    }

    //########################################################################
    // Private Methods - argument parsing

    private String[] readArguments(String command) throws IOException {
        return readArguments(new PushbackReader(new StringReader(command)));
    }

    private String[] readArguments(PushbackReader reader) throws IOException {
        List arguments = new ArrayList();
        while (true) {
            int c = reader.read();
            if (c == -1) return (String[])arguments.toArray(new String[0]);
            if (!Character.isWhitespace((char)c)) {
                reader.unread(c);
                arguments.add(readArgument(reader));
            }
        }
    }

    private String readArgument(PushbackReader reader) throws IOException {
        StringBuffer value = new StringBuffer();
        StringBuffer token = new StringBuffer();
        while (true) {
            int c = read(reader, token);
            switch (c) {
            case '\"': value.append(readString(reader, token)); break;
            case '\\': value.append(readBackslashed(reader, token)); break;
            default  :
                if (!Character.isWhitespace((char)c)) {
                    value.append((char)c);
                    break;
                } else {
                    reader.unread(c);
                }
            case -1  : return value.toString();
            }
            token.delete(0, token.length());
        }
    }

    private String readBackslashed(PushbackReader reader, StringBuffer input)
        throws IOException
    {
        int c = read(reader, input);
        switch (c) {
        case ' ' : return " ";
        case '\"': return "\"";
        case '\\': return "\\";
        default  : return input.toString();
        }
    }

    private String readString(PushbackReader reader, StringBuffer input)
        throws IOException
    {
        StringBuffer value = new StringBuffer();
        StringBuffer token = new StringBuffer();
        while (true) {
            int c = read(reader, token, "unterminated string '" + input + "'");
            switch (c) {
            case '\"': input.append((char)c); return value.toString();
            case '\\': value.append(readEscape(reader, token)); break;
            default  : value.append((char)c); break;
            }
            input.append(token.toString());
            token.delete(0, token.length());
        }
    }

    private char readEscape(PushbackReader reader, StringBuffer input)
        throws IOException
    {
        int c = read(reader, input,
            "unterminated escape sequence '" + input + "'");
        switch (c) {
        case 'b' : return '\b';
        case 't' : return '\t';
        case 'n' : return '\n';
        case 'f' : return '\f';
        case 'r' : return '\r';
        case '\"': return '\"';
        case '\'': return '\'';
        case '\\': return '\\';
        case 'u' : return readUnicode(reader, input);
        case '0' :
        case '1' :
        case '2' :
        case '3' :
        case '4' :
        case '5' :
        case '6' :
        case '7' : return readOctal(reader, input, c);
        default  : throw new IOException(
            "illegal escape sequence '" + input + "'");
        }
    }

    private char readUnicode(PushbackReader reader, StringBuffer input)
        throws IOException
    {
        int value = 0;
        boolean illegal = false;
        for (int i = 0; i < 4; i++) {
            int c = read(reader, input,
                "unterminated Unicode escape sequence '" + input + "'");
            switch (c) {
            case '0': case '1': case '2': case '3': case '4': case '5':
            case '6': case '7': case '8': case '9':
                value = (value << 4) + (c - '0'); break;
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
                value = (value << 4) + (c - 'A' + 0x0A); break;
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
                value = (value << 4) + (c - 'a' + 0x0a); break;
            default:
                illegal = true; break;
            }
        }
        if (illegal) throw new IOException(
            "illegal Unicode escape sequence '" + input + "'");
        return (char)value;
    }

    private char readOctal(PushbackReader reader, StringBuffer input,int first)
        throws IOException
    {
        int value = first - '0';
        while (value < 32) {
            int c = read(reader, input);
            if (c < '0' || '7' < c) { reader.unread(c); break; }
            value = (value << 3) + (c - '0');
        }
        return (char)value;
    }

    private int read(PushbackReader reader, StringBuffer input, String error)
        throws IOException
    {
        int c = read(reader, input);
        if (c == -1) throw new IOException(error);
        return (char)c;
    }

    private int read(PushbackReader reader, StringBuffer input)
        throws IOException
    {
        int c = reader.read();
        if (c != -1) input.append((char)c);
        return c;
    }

    //########################################################################
    // Private Methods - error messages

    private void error(String message) {
        writer.println("Error: " + message);
    }

    //########################################################################
}
