/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scalai;

import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.PushbackReader;
import java.io.StringReader;
import java.io.PrintWriter;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

import scala.runtime.InterpreterSupport;

import scalac.util.Debug;
import scalac.util.Strings;
import scalac.util.PrefixMatcher;

import scalac.Global;

public class InterpreterShell {

    //########################################################################
    // Public Constants

    public static final String PRODUCT   = Main.PRODUCT;
    public static final String VERSION   = Main.VERSION;
    public static final String COPYRIGHT = "(c) 2002-04, LAMP/EPFL";

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

    private final boolean interactive;
    private final boolean nologo;
    private final boolean emacs;
    private final BufferedReader reader;
    private final PrintWriter writer;
    private final PrefixMatcher matcher;
    private final Global global;
    private final Interpreter interpreter;
    private final InterpreterPrinter printer;

    private String[] lfiles;
    private String[] ufiles;

    //########################################################################
    // Public Constructors

    public InterpreterShell(InterpreterCommand command) {
        this(command, new BufferedReader(new InputStreamReader(System.in)),
            new PrintWriter(System.out, true));
    }

    public InterpreterShell(InterpreterCommand command, BufferedReader reader,
        PrintWriter writer)
    {
        this.interactive = command.interactive.value;
        this.nologo = command.nologo.value;
        this.emacs = command.emacs.value;
        this.reader = reader;
        this.writer = writer;
        this.matcher = matcher();
        this.global = new scala.tools.scalac.Global$class(command, true);
        this.interpreter = new Interpreter(global);
        this.printer = new InterpreterPrinter(interpreter, writer);
        this.lfiles = new String[0];
        this.ufiles = new String[0];
    }

    //########################################################################
    // Public Methods - shell

    public void main(String[] files, String script, String main, String[]args){
        if (interactive && !nologo) showBanner();
        if (files.length > 0) load(lfiles = files);
        if (global.reporter.errors() == 0 && script != null) eval(script);
        if (global.reporter.errors() == 0 && main != null) call(main, args);
        if (interactive) loop();
    }

    public void loop() {
        InterpreterSupport.setDefinitionPrinter(printer);
        while (handle(read()));
        InterpreterSupport.setDefinitionPrinter(null);
    }

    public boolean handle(String input) {
        if (input == null) return exec(QUIT);
        String trimed = input.trim();
        if (trimed.startsWith(":")) return exec(trimed);
        eval(input);
        return true;
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
    // Public Methods - command execution

    public boolean exec(String input) {
        int length = input.indexOf(' ');
        length = length < 0 ? input.length() : length;
        String command = input.substring(0, length);
        PrefixMatcher.Entry[] entries = matcher.lookup(command);
        if (entries.length != 1) {
            error(matcher.getErrorMessage(command, entries, "command"));
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
            load(lfiles);
            return true;

        case USE:
            if (args.length != 0) ufiles = args;
            eval(ufiles);
            return true;

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

    //########################################################################
    // Public Methods - interpretation

    public void call(String main, String[] args) {
        show(interpreter.invoke(main, args), false);
    }

    public void load(String input) {
        show(interpreter.interpret(input, false), false);
    }

    public void load(String[] files) {
        show(interpreter.interpret(files, false), false);
    }

    public void eval(String input) {
        show(interpreter.interpret(input, true), true);
    }

    public void eval(String[] files) {
        show(interpreter.interpret(files, true), true);
    }

    public void show(EvaluatorResult result, boolean interactive) {
        printer.showResult(result, interactive);
    }

    //########################################################################
    // Private Methods - matcher initialization

    private PrefixMatcher matcher() {
        PrefixMatcher matcher = new PrefixMatcher();
        matcher.insert(":quit", new Integer(QUIT),
            "Quit interpreter");
        matcher.insert(":load", new Integer(LOAD), "<files>",
            "Load files (or repeat last load if no files are given)");
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
    // Private Methods - error messages

    private void error(String message) {
        writer.println("error: " + message);
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
}
