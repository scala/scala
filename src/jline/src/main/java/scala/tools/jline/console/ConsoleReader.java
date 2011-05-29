/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */

package scala.tools.jline.console;

import scala.tools.jline.Terminal;
import scala.tools.jline.TerminalFactory;
import scala.tools.jline.console.completer.CandidateListCompletionHandler;
import scala.tools.jline.console.completer.Completer;
import scala.tools.jline.console.completer.CompletionHandler;
import scala.tools.jline.console.history.History;
import scala.tools.jline.console.history.MemoryHistory;
import scala.tools.jline.internal.Configuration;
import scala.tools.jline.internal.Log;
import org.fusesource.jansi.AnsiOutputStream;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.*;

/**
 * A reader for console applications. It supports custom tab-completion,
 * saveable command history, and command line editing. On some platforms,
 * platform-specific commands will need to be issued before the reader will
 * function properly. See {@link jline.Terminal#init} for convenience
 * methods for issuing platform-specific setup commands.
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 */
public class ConsoleReader
{
    public static final String JLINE_NOBELL = "jline.nobell";

    public static final String JLINE_EXPANDEVENTS = "jline.expandevents";

    public static final char BACKSPACE = '\b';

    public static final char RESET_LINE = '\r';

    public static final char KEYBOARD_BELL = '\07';

    public static final char NULL_MASK = 0;

    public static final int TAB_WIDTH = 4;

    private static final ResourceBundle
        resources = ResourceBundle.getBundle(CandidateListCompletionHandler.class.getName());

    private final Terminal terminal;

    private InputStream in;

    private final Writer out;

    private final CursorBuffer buf = new CursorBuffer();

    private String prompt;

    private boolean bellEnabled = true;

    private boolean expandEvents = false;

    private Character mask;

    private Character echoCharacter;

    private StringBuffer searchTerm = null;

    private String previousSearchTerm = "";

    private int searchIndex = -1;

    public ConsoleReader(final InputStream in, final OutputStream out, final InputStream bindings, final Terminal term) throws
        IOException
    {
        this.in = in;
        this.terminal = term != null ? term : TerminalFactory.get();
        this.out = new PrintWriter(getTerminal().wrapOutIfNeeded(out));
        this.keyBindings = loadKeyBindings(bindings);

        setBellEnabled(!Configuration.getBoolean(JLINE_NOBELL, false));
        setExpandEvents(Configuration.getBoolean(JLINE_EXPANDEVENTS, false));
    }

    /**
     * @deprecated use {@link #ConsoleReader(InputStream, OutputStream, InputStream, Terminal)}
     * to let the terminal wrap the output stream if needed.
     */
    public ConsoleReader(final InputStream in, final Writer out, final InputStream bindings, final Terminal term) throws
        IOException
    {
        this.in = in;
        this.out = out;
        this.terminal = term != null ? term : TerminalFactory.get();
        this.keyBindings = loadKeyBindings(bindings);

        setBellEnabled(!Configuration.getBoolean(JLINE_NOBELL, false));
    }

    /**
     * @deprecated use {@link #ConsoleReader(InputStream, OutputStream, InputStream, Terminal)}
     * to let the terminal wrap the output stream if needed.
     */
    public ConsoleReader(final InputStream in, final Writer out, final Terminal term) throws IOException {
        this(in, out, null, term);
    }

    /**
     * @deprecated use {@link #ConsoleReader(InputStream, OutputStream, InputStream, Terminal)}
     * to let the terminal wrap the output stream if needed.
     */
    public ConsoleReader(final InputStream in, final Writer out) throws IOException
    {
        this(in, out, null, null);
    }

    /**
     * Create a new reader using {@link FileDescriptor#in} for input and
     * {@link System#out} for output.
     * <p/>
     * {@link FileDescriptor#in} is used because it has a better chance of not being buffered.
     */
    public ConsoleReader() throws IOException {
        this(new FileInputStream(FileDescriptor.in), System.out, null, null );
    }

    // FIXME: Only used for tests

    void setInput(final InputStream in) {
        this.in = in;
    }

    public InputStream getInput() {
        return in;
    }

    public Writer getOutput() {
        return out;
    }

    public Terminal getTerminal() {
        return terminal;
    }

    public CursorBuffer getCursorBuffer() {
        return buf;
    }

    public void setBellEnabled(final boolean enabled) {
        this.bellEnabled = enabled;
    }

    public boolean isBellEnabled() {
        return bellEnabled;
    }

    public void setExpandEvents(final boolean expand) {
        this.expandEvents = expand;
    }

    public boolean getExpandEvents() {
        return expandEvents;
    }

    public void setPrompt(final String prompt) {
        this.prompt = prompt;
    }

    public String getPrompt() {
        return prompt;
    }

    /**
     * Set the echo character. For example, to have "*" entered when a password is typed:
     * <p/>
     * <pre>
     * myConsoleReader.setEchoCharacter(new Character('*'));
     * </pre>
     * <p/>
     * Setting the character to
     * <p/>
     * <pre>
     * null
     * </pre>
     * <p/>
     * will restore normal character echoing. Setting the character to
     * <p/>
     * <pre>
     * new Character(0)
     * </pre>
     * <p/>
     * will cause nothing to be echoed.
     *
     * @param c the character to echo to the console in place of the typed character.
     */
    public void setEchoCharacter(final Character c) {
        this.echoCharacter = c;
    }

    /**
     * Returns the echo character.
     */
    public Character getEchoCharacter() {
        return echoCharacter;
    }

    /**
     * Erase the current line.
     *
     * @return false if we failed (e.g., the buffer was empty)
     */
    protected final boolean resetLine() throws IOException {
        if (buf.cursor == 0) {
            return false;
        }

        backspaceAll();

        return true;
    }

    int getCursorPosition() {
        // FIXME: does not handle anything but a line with a prompt absolute position
        String prompt = getPrompt();
        return ((prompt == null) ? 0 : stripAnsi(lastLine(prompt)).length()) + buf.cursor;
    }

    /**
     * Returns the text after the last '\n'.
     * prompt is returned if no '\n' characters are present.
     * null is returned if prompt is null.
     */
    private String lastLine(String str) {
        if (str == null) return "";
        int last = str.lastIndexOf("\n");

        if (last >= 0) {
            return str.substring(last + 1, str.length());
        }

        return str;
    }

    private String stripAnsi(String str) {
        if (str == null) return "";
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            AnsiOutputStream aos = new AnsiOutputStream(baos);
            aos.write(str.getBytes());
            aos.flush();
            return baos.toString();
        } catch (IOException e) {
            return str;
        }
    }

    /**
     * Move the cursor position to the specified absolute index.
     */
    public final boolean setCursorPosition(final int position) throws IOException {
        return moveCursor(position - buf.cursor) != 0;
    }

    /**
     * Set the current buffer's content to the specified {@link String}. The
     * visual console will be modified to show the current buffer.
     *
     * @param buffer the new contents of the buffer.
     */
    private void setBuffer(final String buffer) throws IOException {
        // don't bother modifying it if it is unchanged
        if (buffer.equals(buf.buffer.toString())) {
            return;
        }

        // obtain the difference between the current buffer and the new one
        int sameIndex = 0;

        for (int i = 0, l1 = buffer.length(), l2 = buf.buffer.length(); (i < l1)
            && (i < l2); i++) {
            if (buffer.charAt(i) == buf.buffer.charAt(i)) {
                sameIndex++;
            }
            else {
                break;
            }
        }

        int diff = buf.cursor - sameIndex;
        if (diff < 0) { // we can't backspace here so try from the end of the buffer
            moveToEnd();
            diff = buf.buffer.length() - sameIndex;
        }

        backspace(diff); // go back for the differences
        killLine(); // clear to the end of the line
        buf.buffer.setLength(sameIndex); // the new length
        putString(buffer.substring(sameIndex)); // append the differences
    }

    private void setBuffer(final CharSequence buffer) throws IOException {
        setBuffer(String.valueOf(buffer));
    }

    /**
     * Output put the prompt + the current buffer
     */
    public final void drawLine() throws IOException {
        String prompt = getPrompt();
        if (prompt != null) {
            print(prompt);
        }

        print(buf.buffer.toString());

        if (buf.length() != buf.cursor) { // not at end of line
            back(buf.length() - buf.cursor - 1);
        }
        // force drawBuffer to check for weird wrap (after clear screen)
        drawBuffer();
    }

    /**
     * Clear the line and redraw it.
     */
    public final void redrawLine() throws IOException {
        print(RESET_LINE);
//        flush();
        drawLine();
    }

    /**
     * Clear the buffer and add its contents to the history.
     *
     * @return the former contents of the buffer.
     */
    final String finishBuffer() throws IOException { // FIXME: Package protected because used by tests
        String str = buf.buffer.toString();

        if (expandEvents) {
            str = expandEvents(str);
        }

        // we only add it to the history if the buffer is not empty
        // and if mask is null, since having a mask typically means
        // the string was a password. We clear the mask after this call
        if (str.length() > 0) {
            if (mask == null && isHistoryEnabled()) {
                history.add(str);
            }
            else {
                mask = null;
            }
        }

        history.moveToEnd();

        buf.buffer.setLength(0);
        buf.cursor = 0;

        return str;
    }

    /**
     * Expand event designator such as !!, !#, !3, etc...
     * See http://www.gnu.org/software/bash/manual/html_node/Event-Designators.html
     *
     * @param str
     * @return
     */
    protected String expandEvents(String str) throws IOException {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            switch (c) {
                case '!':
                    if (i + 1 < str.length()) {
                        c = str.charAt(++i);
                        boolean neg = false;
                        String rep = null;
                        int i1, idx;
                        switch (c) {
                            case '!':
                                if (history.size() == 0) {
                                    throw new IllegalArgumentException("!!: event not found");
                                }
                                rep = history.get(history.index() - 1).toString();
                                break;
                            case '#':
                                sb.append(sb.toString());
                                break;
                            case '?':
                                i1 = str.indexOf('?', i + 1);
                                if (i1 < 0) {
                                    i1 = str.length();
                                }
                                String sc = str.substring(i + 1, i1);
                                i = i1;
                                idx = searchBackwards(sc);
                                if (idx < 0) {
                                    throw new IllegalArgumentException("!?" + sc + ": event not found");
                                } else {
                                    rep = history.get(idx).toString();
                                }
                                break;
                            case ' ':
                            case '\t':
                                sb.append('!');
                                sb.append(c);
                                break;
                            case '-':
                                neg = true;
                                i++;
                                // fall through
                            case '0':
                            case '1':
                            case '2':
                            case '3':
                            case '4':
                            case '5':
                            case '6':
                            case '7':
                            case '8':
                            case '9':
                                i1 = i;
                                for (; i < str.length(); i++) {
                                    c = str.charAt(i);
                                    if (c < '0' || c > '9') {
                                        break;
                                    }
                                }
                                idx = 0;
                                try {
                                    idx = Integer.parseInt(str.substring(i1, i));
                                } catch (NumberFormatException e) {
                                    throw new IllegalArgumentException((neg ? "!-" : "!") + str.substring(i1, i) + ": event not found");
                                }
                                if (neg) {
                                    if (idx < history.size()) {
                                        rep = (history.get(history.index() - idx)).toString();
                                    } else {
                                        throw new IllegalArgumentException((neg ? "!-" : "!") + str.substring(i1, i) + ": event not found");
                                    }
                                } else {
                                    if (idx >= history.index() - history.size() && idx < history.index()) {
                                        rep = (history.get(idx)).toString();
                                    } else {
                                        throw new IllegalArgumentException((neg ? "!-" : "!") + str.substring(i1, i) + ": event not found");
                                    }
                                }
                                break;
                            default:
                                String ss = str.substring(i);
                                i = str.length();
                                idx = searchBackwards(ss, history.index(), true);
                                if (idx < 0) {
                                    throw new IllegalArgumentException("!" + ss + ": event not found");
                                } else {
                                    rep = history.get(idx).toString();
                                }
                                break;
                        }
                        if (rep != null) {
                            sb.append(rep);
                        }
                    } else {
                        sb.append(c);
                    }
                    break;
                case '^':
                    if (i == 0) {
                        int i1 = str.indexOf('^', i + 1);
                        int i2 = str.indexOf('^', i1 + 1);
                        if (i2 < 0) {
                            i2 = str.length();
                        }
                        if (i1 > 0 && i2 > 0) {
                            String s1 = str.substring(i + 1, i1);
                            String s2 = str.substring(i1 + 1, i2);
                            String s = history.get(history.index() - 1).toString().replace(s1, s2);
                            sb.append(s);
                            i = i2 + 1;
                            break;
                        }
                    }
                    sb.append(c);
                    break;
                default:
                    sb.append(c);
                    break;
            }
        }
        String result = sb.toString();
        if (!str.equals(result)) {
            print(result);
            println();
            flush();
        }
        return result;

    }

    /**
     * Write out the specified string to the buffer and the output stream.
     */
    public final void putString(final CharSequence str) throws IOException {
        buf.write(str);
        print(str);
        drawBuffer();
    }

    /**
     * Output the specified character, both to the buffer and the output stream.
     */
    private void putChar(final int c, final boolean print) throws IOException {
        buf.write((char) c);

        if (print) {
            if (mask == null) {
                // no masking
                print(c);
            }
            else if (mask == NULL_MASK) {
                // Don't print anything
            }
            else {
                print(mask);
            }

            drawBuffer();
        }
    }

    /**
     * Redraw the rest of the buffer from the cursor onwards. This is necessary
     * for inserting text into the buffer.
     *
     * @param clear the number of characters to clear after the end of the buffer
     */
    private void drawBuffer(final int clear) throws IOException {
        // debug ("drawBuffer: " + clear);
        if (buf.cursor == buf.length() && clear == 0) {
        } else {
            char[] chars = buf.buffer.substring(buf.cursor).toCharArray();
            if (mask != null) {
                Arrays.fill(chars, mask);
            }
            if (getTerminal().hasWeirdWrap()) {
                // need to determine if wrapping will occur:
                int width = getTerminal().getWidth();
                int pos = getCursorPosition();
                for (int i = 0; i < chars.length; i++) {
                    print(chars[i]);
                    if ((pos + i + 1) % width == 0) {
                        print(32); // move cursor to next line by printing dummy space
                        print(13); // CR / not newline.
                    }
                }
            } else {
                print(chars);
            }
            clearAhead(clear, chars.length);
            if (getTerminal().isAnsiSupported()) {
                if (chars.length > 0) {
                    back(chars.length);
                }
            } else {
                back(chars.length);
            }
        }
        if (getTerminal().hasWeirdWrap()) {
            int width = getTerminal().getWidth();
            // best guess on whether the cursor is in that weird location...
            // Need to do this without calling ansi cursor location methods
            // otherwise it breaks paste of wrapped lines in xterm.
            if (getCursorPosition() > 0 && (getCursorPosition() % width == 0)
                    && buf.cursor == buf.length() && clear == 0) {
                // the following workaround is reverse-engineered from looking
                // at what bash sent to the terminal in the same situation
                print(32); // move cursor to next line by printing dummy space
                print(13); // CR / not newline.
            }
        }
    }

    /**
     * Redraw the rest of the buffer from the cursor onwards. This is necessary
     * for inserting text into the buffer.
     */
    private void drawBuffer() throws IOException {
        drawBuffer(0);
    }

    /**
     * Clear ahead the specified number of characters without moving the cursor.
     *
     * @param num the number of characters to clear
     * @param delta the difference between the internal cursor and the screen
     * cursor - if > 0, assume some stuff was printed and weird wrap has to be
     * checked
     */
    private void clearAhead(final int num, int delta) throws IOException {
        if (num == 0) {
            return;
        }

        if (getTerminal().isAnsiSupported()) {
            int width = getTerminal().getWidth();
            int screenCursorCol = getCursorPosition() + delta;
            // clear current line
            printAnsiSequence("K");
            // if cursor+num wraps, then we need to clear the line(s) below too
            int curCol = screenCursorCol % width;
            int endCol = (screenCursorCol + num - 1) % width;
            int lines = num / width;
            if (endCol < curCol) lines++;
            for (int i = 0; i < lines; i++) {
                printAnsiSequence("B");
                printAnsiSequence("2K");
            }
            for (int i = 0; i < lines; i++) {
                printAnsiSequence("A");
            }
            return;
        }

        // print blank extra characters
        print(' ', num);

        // we need to flush here so a "clever" console doesn't just ignore the redundancy
        // of a space followed by a backspace.
//        flush();

        // reset the visual cursor
        back(num);

//        flush();
    }

    /**
     * Move the visual cursor backwards without modifying the buffer cursor.
     */
    protected void back(final int num) throws IOException {
        if (num == 0) return;
        if (getTerminal().isAnsiSupported()) {
            int width = getTerminal().getWidth();
            int cursor = getCursorPosition();
            int realCursor = cursor + num;
            int realCol  = realCursor % width;
            int newCol = cursor % width;
            int moveup = num / width;
            int delta = realCol - newCol;
            if (delta < 0) moveup++;
            if (moveup > 0) {
                printAnsiSequence(moveup + "A");
            }
            printAnsiSequence((1 + newCol) + "G");
            return;
        }
        print(BACKSPACE, num);
//        flush();
    }

    /**
     * Flush the console output stream. This is important for printout out single characters (like a backspace or
     * keyboard) that we want the console to handle immediately.
     */
    public void flush() throws IOException {
        out.flush();
    }

    private int backspaceAll() throws IOException {
        return backspace(Integer.MAX_VALUE);
    }

    /**
     * Issue <em>num</em> backspaces.
     *
     * @return the number of characters backed up
     */
    private int backspace(final int num) throws IOException {
        if (buf.cursor == 0) {
            return 0;
        }

        int count = 0;

        int termwidth = getTerminal().getWidth();
        int lines = getCursorPosition() / termwidth;
        count = moveCursor(-1 * num) * -1;
        buf.buffer.delete(buf.cursor, buf.cursor + count);
        if (getCursorPosition() / termwidth != lines) {
            if (getTerminal().isAnsiSupported()) {
                // debug("doing backspace redraw: " + getCursorPosition() + " on " + termwidth + ": " + lines);
                printAnsiSequence("K");
                // if cursor+num wraps, then we need to clear the line(s) below too
                // last char printed is one pos less than cursor so we subtract
                // one
/*
                // TODO: fixme (does not work - test with reverse search with wrapping line and CTRL-E)
                int endCol = (getCursorPosition() + num - 1) % termwidth;
                int curCol = getCursorPosition() % termwidth;
                if (endCol < curCol) lines++;
                for (int i = 1; i < lines; i++) {
                    printAnsiSequence("B");
                    printAnsiSequence("2K");
                }
                for (int i = 1; i < lines; i++) {
                    printAnsiSequence("A");
                }
                return count;
*/
            }
        }
        drawBuffer(count);

        return count;
    }

    /**
     * Issue a backspace.
     *
     * @return true if successful
     */
    public boolean backspace() throws IOException {
        return backspace(1) == 1;
    }

    protected boolean moveToEnd() throws IOException {
        return moveCursor(buf.length() - buf.cursor) > 0;
    }

    /**
     * Delete the character at the current position and redraw the remainder of the buffer.
     */
    private boolean deleteCurrentCharacter() throws IOException {
        if (buf.length() == 0 || buf.cursor == buf.length()) {
            return false;
        }

        buf.buffer.deleteCharAt(buf.cursor);
        drawBuffer(1);
        return true;
    }

    private boolean previousWord() throws IOException {
        while (isDelimiter(buf.charLeftOfCursor()) && (moveCursor(-1) != 0)) {
            // nothing
        }

        while (!isDelimiter(buf.charLeftOfCursor()) && (moveCursor(-1) != 0)) {
            // nothing
        }

        return true;
    }

    private boolean nextWord() throws IOException {
        while (isDelimiter(buf.charAtCursor()) && (moveCursor(1) != 0)) {
            // nothing
        }

        while (!isDelimiter(buf.charAtCursor()) && (moveCursor(1) != 0)) {
            // nothing
        }

        return true;
    }

    private boolean deletePreviousWord() throws IOException {
        while (isDelimiter(buf.charLeftOfCursor()) && backspace()) {
            // nothing
        }

        while (!isDelimiter(buf.charLeftOfCursor()) && backspace()) {
            // nothing
        }

        return true;
    }

    private boolean deleteNextWord() throws IOException {
        while (isDelimiter(buf.charAtCursor()) && deleteCurrentCharacter()) {
            // nothing
        }

        while (!isDelimiter(buf.charAtCursor()) && deleteCurrentCharacter()) {
            // nothing
        }

        return true;
    }

    /**
     * Move the cursor <i>where</i> characters.
     *
     * @param num   If less than 0, move abs(<i>where</i>) to the left, otherwise move <i>where</i> to the right.
     * @return      The number of spaces we moved
     */
    public int moveCursor(final int num) throws IOException {
        int where = num;

        if ((buf.cursor == 0) && (where <= 0)) {
            return 0;
        }

        if ((buf.cursor == buf.buffer.length()) && (where >= 0)) {
            return 0;
        }

        if ((buf.cursor + where) < 0) {
            where = -buf.cursor;
        }
        else if ((buf.cursor + where) > buf.buffer.length()) {
            where = buf.buffer.length() - buf.cursor;
        }

        moveInternal(where);

        return where;
    }

    /**
     * Move the cursor <i>where</i> characters, without checking the current buffer.
     *
     * @param where the number of characters to move to the right or left.
     */
    private void moveInternal(final int where) throws IOException {
        // debug ("move cursor " + where + " ("
        // + buf.cursor + " => " + (buf.cursor + where) + ")");
        buf.cursor += where;

        if (getTerminal().isAnsiSupported()) {
            if (where < 0) {
                back(Math.abs(where));
            } else {
                int width = getTerminal().getWidth();
                int cursor = getCursorPosition();
                int oldLine = (cursor - where) / width;
                int newLine = cursor / width;
                if (newLine > oldLine) {
                    if (getTerminal().hasWeirdWrap()) {
                        // scroll up if at bottom
                        // note:
                        //   on rxvt cywgin getTerminal().getHeight() is incorrect
                        //   MacOs xterm does not seem to support scrolling
                        if (getCurrentAnsiRow() == getTerminal().getHeight()) {
                            printAnsiSequence((newLine - oldLine) + "S");
                        }
                    }
                    printAnsiSequence((newLine - oldLine) + "B");
                }
                printAnsiSequence(1 +(cursor % width) + "G");
            }
//            flush();
            return;
        }

        char c;

        if (where < 0) {
            int len = 0;
            for (int i = buf.cursor; i < buf.cursor - where; i++) {
                if (buf.buffer.charAt(i) == '\t') {
                    len += TAB_WIDTH;
                }
                else {
                    len++;
                }
            }

            char chars[] = new char[len];
            Arrays.fill(chars, BACKSPACE);
            out.write(chars);

            return;
        }
        else if (buf.cursor == 0) {
            return;
        }
        else if (mask != null) {
            c = mask;
        }
        else {
            print(buf.buffer.substring(buf.cursor - where, buf.cursor).toCharArray());
            return;
        }

        // null character mask: don't output anything
        if (mask == NULL_MASK) {
            return;
        }

        print(c, Math.abs(where));
    }

    // FIXME: replace() is not used

    public final boolean replace(final int num, final String replacement) {
        buf.buffer.replace(buf.cursor - num, buf.cursor, replacement);
        try {
            moveCursor(-num);
            drawBuffer(Math.max(0, num - replacement.length()));
            moveCursor(replacement.length());
        }
        catch (IOException e) {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    //
    // Key reading
    //

    /**
     * Read a character from the console.
     *
     * @return the character, or -1 if an EOF is received.
     */
    public final int readVirtualKey() throws IOException {
        int c = getTerminal().readVirtualKey(in);

        Log.trace("Keystroke: ", c);

        // clear any echo characters
        clearEcho(c);

        return c;
    }

    /**
     * Clear the echoed characters for the specified character code.
     */
    private int clearEcho(final int c) throws IOException {
        // if the terminal is not echoing, then ignore
        if (!getTerminal().isEchoEnabled()) {
            return 0;
        }

        // otherwise, clear
        int num = countEchoCharacters((char) c);
        back(num);
        drawBuffer(num);

        return num;
    }

    private int countEchoCharacters(final char c) {
        // tabs as special: we need to determine the number of spaces
        // to cancel based on what out current cursor position is
        if (c == 9) {
            int tabStop = 8; // will this ever be different?
            int position = getCursorPosition();

            return tabStop - (position % tabStop);
        }

        return getPrintableCharacters(c).length();
    }

    /**
     * Return the number of characters that will be printed when the specified
     * character is echoed to the screen
     *
     * Adapted from cat by Torbjorn Granlund, as repeated in stty by David MacKenzie.
     */
    private StringBuilder getPrintableCharacters(final char ch) {
        StringBuilder sbuff = new StringBuilder();

        if (ch >= 32) {
            if (ch < 127) {
                sbuff.append(ch);
            }
            else if (ch == 127) {
                sbuff.append('^');
                sbuff.append('?');
            }
            else {
                sbuff.append('M');
                sbuff.append('-');

                if (ch >= (128 + 32)) {
                    if (ch < (128 + 127)) {
                        sbuff.append((char) (ch - 128));
                    }
                    else {
                        sbuff.append('^');
                        sbuff.append('?');
                    }
                }
                else {
                    sbuff.append('^');
                    sbuff.append((char) (ch - 128 + 64));
                }
            }
        }
        else {
            sbuff.append('^');
            sbuff.append((char) (ch + 64));
        }

        return sbuff;
    }

    public final int readCharacter(final char... allowed) throws IOException {
        // if we restrict to a limited set and the current character is not in the set, then try again.
        char c;

        Arrays.sort(allowed); // always need to sort before binarySearch

        while (Arrays.binarySearch(allowed, c = (char) readVirtualKey()) < 0) {
            // nothing
        }

        return c;
    }

    //
    // Key Bindings
    //

    public static final String JLINE_COMPLETION_THRESHOLD = "jline.completion.threshold";

    public static final String JLINE_KEYBINDINGS = "jline.keybindings";

    public static final String JLINEBINDINGS_PROPERTIES = ".jlinebindings.properties";

    /**
     * The map for logical operations.
     */
    private final short[] keyBindings;

    private short[] loadKeyBindings(InputStream input) throws IOException {
        if (input == null) {
            try {
                File file = new File(Configuration.getUserHome(), JLINEBINDINGS_PROPERTIES);

                String path = Configuration.getString(JLINE_KEYBINDINGS);
                if (path != null) {
                    file = new File(path);
                }

                if (file.isFile()) {
                    Log.debug("Loading user bindings from: ", file);
                    input = new FileInputStream(file);
                }
            }
            catch (Exception e) {
                Log.error("Failed to load user bindings", e);
            }
        }

        if (input == null) {
            Log.debug("Using default bindings");
            input = getTerminal().getDefaultBindings();
        }

        short[] keyBindings = new short[Character.MAX_VALUE * 2];

        Arrays.fill(keyBindings, Operation.UNKNOWN.code);

        // Loads the key bindings. Bindings file is in the format:
        //
        // keycode: operation name

        if (input != null) {
            input = new BufferedInputStream(input);
            Properties p = new Properties();
            p.load(input);
            input.close();

            for (Object key : p.keySet()) {
                String val = (String) key;

                try {
                    short code = Short.parseShort(val);
                    String name = p.getProperty(val);
                    Operation op = Operation.valueOf(name);
                    keyBindings[code] = op.code;
                }
                catch (NumberFormatException e) {
                    Log.error("Failed to convert binding code: ", val, e);
                }
            }

            // hardwired arrow key bindings
            // keybindings[VK_UP] = PREV_HISTORY;
            // keybindings[VK_DOWN] = NEXT_HISTORY;
            // keybindings[VK_LEFT] = PREV_CHAR;
            // keybindings[VK_RIGHT] = NEXT_CHAR;
        }

        return keyBindings;
    }

    int getKeyForAction(final short logicalAction) {
        for (int i = 0; i < keyBindings.length; i++) {
            if (keyBindings[i] == logicalAction) {
                return i;
            }
        }

        return -1;
    }

    int getKeyForAction(final Operation op) {
        assert op != null;
        return getKeyForAction(op.code);
    }

    public void printBindings() {
        System.out.println("printBindings(): keyBindings.length = " + keyBindings.length);
        for (int i = 0; i < keyBindings.length; i++) {
            if (keyBindings[i] != Operation.UNKNOWN.code) {
                System.out.println("keyBindings[" + i + "] = " + keyBindings[i]);
            }
        }
    }

    /**
     * Reads the console input and returns an array of the form [raw, key binding].
     */
    private int[] readBinding() throws IOException {
        int c = readVirtualKey();

        if (c == -1) {
            return null;
        }

        // extract the appropriate key binding
        short code = keyBindings[c];

        Log.trace("Translated: ", c, " -> ", code);

        return new int[]{c, code};
    }

    //
    // Line Reading
    //

    /**
     * Read the next line and return the contents of the buffer.
     */
    public String readLine() throws IOException {
        return readLine((String) null);
    }

    /**
     * Read the next line with the specified character mask. If null, then
     * characters will be echoed. If 0, then no characters will be echoed.
     */
    public String readLine(final Character mask) throws IOException {
        return readLine(null, mask);
    }

    public String readLine(final String prompt) throws IOException {
        return readLine(prompt, null);
    }

    /**
     * Read a line from the <i>in</i> {@link InputStream}, and return the line
     * (without any trailing newlines).
     *
     * @param prompt    The prompt to issue to the console, may be null.
     * @return          A line that is read from the terminal, or null if there was null input (e.g., <i>CTRL-D</i>
 *                      was pressed).
     */
    public String readLine(String prompt, final Character mask) throws IOException {
        // prompt may be null
        // mask may be null

        // FIXME: This blows, each call to readLine will reset the console's state which doesn't seem very nice.
        this.mask = mask;
        if (prompt != null) {
            setPrompt(prompt);
        }
        else {
            prompt = getPrompt();
        }

        try {
            if (!getTerminal().isSupported()) {
                beforeReadLine(prompt, mask);
            }

            if (prompt != null && prompt.length() > 0) {
                out.write(prompt);
                out.flush();
            }

            // if the terminal is unsupported, just use plain-java reading
            if (!getTerminal().isSupported()) {
                return readLine(in);
            }

            String originalPrompt = this.prompt;

            final int NORMAL = 1;
            final int SEARCH = 2;
            int state = NORMAL;

            boolean success = true;

            while (true) {
                int[] next = readBinding();

                if (next == null) {
                    return null;
                }

                int c = next[0];
                // int code = next[1];
                Operation code = Operation.valueOf(next[1]);

                if (c == -1) {
                    return null;
                }

                // Search mode.
                //
                // Note that we have to do this first, because if there is a command
                // not linked to a search command, we leave the search mode and fall
                // through to the normal state.
                if (state == SEARCH) {
                    int cursorDest = -1;

                    switch (code) {
                        // This doesn't work right now, it seems CTRL-G is not passed
                        // down correctly. :(
                        case ABORT:
                            state = NORMAL;
                            break;

                        case SEARCH_PREV:
                            if (searchTerm.length() == 0) {
                                searchTerm.append(previousSearchTerm);
                            }

                            if (searchIndex == -1) {
                                searchIndex = searchBackwards(searchTerm.toString());
                            } else {
                                searchIndex = searchBackwards(searchTerm.toString(), searchIndex);
                            }
                            break;

                        case DELETE_PREV_CHAR:
                            if (searchTerm.length() > 0) {
                                searchTerm.deleteCharAt(searchTerm.length() - 1);
                                searchIndex = searchBackwards(searchTerm.toString());
                            }
                            break;

                        case UNKNOWN:
                            searchTerm.appendCodePoint(c);
                            searchIndex = searchBackwards(searchTerm.toString());
                            break;

                        default:
                            // Set buffer and cursor position to the found string.
                            if (searchIndex != -1) {
                                history.moveTo(searchIndex);
                                // set cursor position to the found string
                                cursorDest = history.current().toString().indexOf(searchTerm.toString());
                            }
                            state = NORMAL;
                            break;
                    }

                    // if we're still in search mode, print the search status
                    if (state == SEARCH) {
                        if (searchTerm.length() == 0) {
                            printSearchStatus("", "");
                            searchIndex = -1;
                        } else {
                            if (searchIndex == -1) {
                                beep();
                            } else {
                                printSearchStatus(searchTerm.toString(), history.get(searchIndex).toString());
                            }
                        }
                    }
                    // otherwise, restore the line
                    else {
                        restoreLine(originalPrompt, cursorDest);
                    }
                }

                if (state == NORMAL) {
                    switch (code) {
                        case EXIT: // ctrl-d
                            if (buf.buffer.length() == 0) {
                                return null;
                            } else {
                                success = deleteCurrentCharacter();
                            }
                            break;

                        case COMPLETE: // tab
                            success = complete();
                            break;

                        case MOVE_TO_BEG:
                            success = setCursorPosition(0);
                            break;

                        case KILL_LINE: // CTRL-K
                            success = killLine();
                            break;

                        case CLEAR_SCREEN: // CTRL-L
                            success = clearScreen();
                            break;

                        case KILL_LINE_PREV: // CTRL-U
                            success = resetLine();
                            break;

                        case NEWLINE: // enter
                            moveToEnd();
                            println(); // output newline
                            flush();
                            return finishBuffer();

                        case DELETE_PREV_CHAR: // backspace
                            success = backspace();
                            break;

                        case DELETE_NEXT_CHAR: // delete
                            success = deleteCurrentCharacter();
                            break;

                        case MOVE_TO_END:
                            success = moveToEnd();
                            break;

                        case PREV_CHAR:
                            success = moveCursor(-1) != 0;
                            break;

                        case NEXT_CHAR:
                            success = moveCursor(1) != 0;
                            break;

                        case NEXT_HISTORY:
                            success = moveHistory(true);
                            break;

                        case PREV_HISTORY:
                            success = moveHistory(false);
                            break;

                        case ABORT:
                        case REDISPLAY:
                            break;

                        case PASTE:
                            success = paste();
                            break;

                        case DELETE_PREV_WORD:
                            success = deletePreviousWord();
                            break;

                        case DELETE_NEXT_WORD:
                            success = deleteNextWord();
                            break;

                        case PREV_WORD:
                            success = previousWord();
                            break;

                        case NEXT_WORD:
                            success = nextWord();
                            break;

                        case START_OF_HISTORY:
                            success = history.moveToFirst();
                            if (success) {
                                setBuffer(history.current());
                            }
                            break;

                        case END_OF_HISTORY:
                            success = history.moveToLast();
                            if (success) {
                                setBuffer(history.current());
                            }
                            break;

                        case CLEAR_LINE:
                            moveInternal(-(buf.cursor));
                            killLine();
                            break;

                        case INSERT:
                            buf.setOverTyping(!buf.isOverTyping());
                            break;

                        case SEARCH_PREV: // CTRL-R
                            if (searchTerm != null) {
                                previousSearchTerm = searchTerm.toString();
                            }
                            searchTerm = new StringBuffer(buf.buffer);
                            state = SEARCH;
                            if (searchTerm.length() > 0) {
                                searchIndex = searchBackwards(searchTerm.toString());
                                if (searchIndex == -1) {
                                    beep();
                                }
                                printSearchStatus(searchTerm.toString(),
                                        searchIndex > -1 ? history.get(searchIndex).toString() : "");
                            } else {
                                searchIndex = -1;
                                printSearchStatus("", "");
                            }
                            break;

                        case UNKNOWN:
                        default:
                            if (c != 0) { // ignore null chars
                                ActionListener action = triggeredActions.get((char) c);
                                if (action != null) {
                                    action.actionPerformed(null);
                                }
                                else {
                                    putChar(c, true);
                                }
                            }
                            else {
                                success = false;
                            }
                    }

                    if (!success) {
                        beep();
                    }

                    flush();
                }
            }
        }
        finally {
            if (!getTerminal().isSupported()) {
                afterReadLine();
            }
        }
    }

    /**
     * Read a line for unsupported terminals.
     */
    private String readLine(final InputStream in) throws IOException {
        StringBuilder buff = new StringBuilder();

        while (true) {
            int i = in.read();

            if (i == -1 || i == '\n' || i == '\r') {
                return buff.toString();
            }

            buff.append((char) i);
        }

        // return new BufferedReader (new InputStreamReader (in)).readLine ();
    }

    //
    // Completion
    //

    private final List<Completer> completers = new LinkedList<Completer>();

    private CompletionHandler completionHandler = new CandidateListCompletionHandler();

    /**
     * Add the specified {@link jline.console.completer.Completer} to the list of handlers for tab-completion.
     *
     * @param completer the {@link jline.console.completer.Completer} to add
     * @return true if it was successfully added
     */
    public boolean addCompleter(final Completer completer) {
        return completers.add(completer);
    }

    /**
     * Remove the specified {@link jline.console.completer.Completer} from the list of handlers for tab-completion.
     *
     * @param completer     The {@link Completer} to remove
     * @return              True if it was successfully removed
     */
    public boolean removeCompleter(final Completer completer) {
        return completers.remove(completer);
    }

    /**
     * Returns an unmodifiable list of all the completers.
     */
    public Collection<Completer> getCompleters() {
        return Collections.unmodifiableList(completers);
    }

    public void setCompletionHandler(final CompletionHandler handler) {
        assert handler != null;
        this.completionHandler = handler;
    }

    public CompletionHandler getCompletionHandler() {
        return this.completionHandler;
    }

    /**
     * Use the completers to modify the buffer with the appropriate completions.
     *
     * @return true if successful
     */
    protected boolean complete() throws IOException {
        // debug ("tab for (" + buf + ")");
        if (completers.size() == 0) {
            return false;
        }

        List<CharSequence> candidates = new LinkedList<CharSequence>();
        String bufstr = buf.buffer.toString();
        int cursor = buf.cursor;

        int position = -1;

        for (Completer comp : completers) {
            if ((position = comp.complete(bufstr, cursor, candidates)) != -1) {
                break;
            }
        }

        return candidates.size() != 0 && getCompletionHandler().complete(this, candidates, position);
    }

    /**
     * The number of tab-completion candidates above which a warning will be
     * prompted before showing all the candidates.
     */
    private int autoprintThreshold = Integer.getInteger(JLINE_COMPLETION_THRESHOLD, 100); // same default as bash

    /**
     * @param threshold the number of candidates to print without issuing a warning.
     */
    public void setAutoprintThreshold(final int threshold) {
        this.autoprintThreshold = threshold;
    }

    /**
     * @return the number of candidates to print without issuing a warning.
     */
    public int getAutoprintThreshold() {
        return autoprintThreshold;
    }

    private boolean paginationEnabled;

    /**
     * Whether to use pagination when the number of rows of candidates exceeds the height of the terminal.
     */
    public void setPaginationEnabled(final boolean enabled) {
        this.paginationEnabled = enabled;
    }

    /**
     * Whether to use pagination when the number of rows of candidates exceeds the height of the terminal.
     */
    public boolean isPaginationEnabled() {
        return paginationEnabled;
    }

    //
    // History
    //

    private History history = new MemoryHistory();

    public void setHistory(final History history) {
        this.history = history;
    }

    public History getHistory() {
        return history;
    }

    private boolean historyEnabled = true;

    /**
     * Whether or not to add new commands to the history buffer.
     */
    public void setHistoryEnabled(final boolean enabled) {
        this.historyEnabled = enabled;
    }

    /**
     * Whether or not to add new commands to the history buffer.
     */
    public boolean isHistoryEnabled() {
        return historyEnabled;
    }

    /**
     * Move up or down the history tree.
     */
    private boolean moveHistory(final boolean next) throws IOException {
        if (next && !history.next()) {
            return false;
        }
        else if (!next && !history.previous()) {
            return false;
        }

        setBuffer(history.current());

        return true;
    }

    //
    // Printing
    //

    public static final String CR = System.getProperty("line.separator");

    /**
     * Output the specified character to the output stream without manipulating the current buffer.
     */
    private void print(final int c) throws IOException {
        if (c == '\t') {
            char chars[] = new char[TAB_WIDTH];
            Arrays.fill(chars, ' ');
            out.write(chars);
            return;
        }

        out.write(c);
    }

    /**
     * Output the specified characters to the output stream without manipulating the current buffer.
     */
    private void print(final char... buff) throws IOException {
        int len = 0;
        for (char c : buff) {
            if (c == '\t') {
                len += TAB_WIDTH;
            }
            else {
                len++;
            }
        }

        char chars[];
        if (len == buff.length) {
            chars = buff;
        }
        else {
            chars = new char[len];
            int pos = 0;
            for (char c : buff) {
                if (c == '\t') {
                    Arrays.fill(chars, pos, pos + TAB_WIDTH, ' ');
                    pos += TAB_WIDTH;
                }
                else {
                    chars[pos] = c;
                    pos++;
                }
            }
        }

        out.write(chars);
    }

    private void print(final char c, final int num) throws IOException {
        if (num == 1) {
            print(c);
        }
        else {
            char[] chars = new char[num];
            Arrays.fill(chars, c);
            print(chars);
        }
    }

    /**
     * Output the specified string to the output stream (but not the buffer).
     */
    public final void print(final CharSequence s) throws IOException {
        assert s != null;
        print(s.toString().toCharArray());
    }

    public final void println(final CharSequence s) throws IOException {
        assert s != null;
        print(s.toString().toCharArray());
        println();
    }

    /**
     * Output a platform-dependant newline.
     */
    public final void println() throws IOException {
        print(CR);
//        flush();
    }

    //
    // Actions
    //

    /**
     * Issue a delete.
     *
     * @return true if successful
     */
    public final boolean delete() throws IOException {
        return delete(1) == 1;
    }

    // FIXME: delete(int) only used by above + the return is always 1 and num is ignored

    /**
     * Issue <em>num</em> deletes.
     *
     * @return the number of characters backed up
     */
    private int delete(final int num) throws IOException {
        // TODO: Try to use jansi for this

        /* Commented out because of DWA-2949:
        if (buf.cursor == 0) {
            return 0;
        }
        */

        buf.buffer.delete(buf.cursor, buf.cursor + 1);
        drawBuffer(1);

        return 1;
    }

    /**
     * Kill the buffer ahead of the current cursor position.
     *
     * @return true if successful
     */
    public boolean killLine() throws IOException {
        int cp = buf.cursor;
        int len = buf.buffer.length();

        if (cp >= len) {
            return false;
        }

        int num = buf.buffer.length() - cp;
        clearAhead(num, 0);

        for (int i = 0; i < num; i++) {
            buf.buffer.deleteCharAt(len - i - 1);
        }

        return true;
    }

    /**
     * Clear the screen by issuing the ANSI "clear screen" code.
     */
    public boolean clearScreen() throws IOException {
        if (!getTerminal().isAnsiSupported()) {
            return false;
        }

        // send the ANSI code to clear the screen
        printAnsiSequence("2J");

        // then send the ANSI code to go to position 1,1
        printAnsiSequence("1;1H");

        redrawLine();

        return true;
    }

    /**
     * Issue an audible keyboard bell, if {@link #isBellEnabled} return true.
     */
    public void beep() throws IOException {
        if (isBellEnabled()) {
            print(KEYBOARD_BELL);
            // need to flush so the console actually beeps
            flush();
        }
    }

    /**
     * Paste the contents of the clipboard into the console buffer
     *
     * @return true if clipboard contents pasted
     */
    public boolean paste() throws IOException {
        Clipboard clipboard;
        try { // May throw ugly exception on system without X
            clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        }
        catch (Exception e) {
            return false;
        }

        if (clipboard == null) {
            return false;
        }

        Transferable transferable = clipboard.getContents(null);

        if (transferable == null) {
            return false;
        }

        try {
            Object content = transferable.getTransferData(DataFlavor.plainTextFlavor);

            // This fix was suggested in bug #1060649 at
            // http://sourceforge.net/tracker/index.php?func=detail&aid=1060649&group_id=64033&atid=506056
            // to get around the deprecated DataFlavor.plainTextFlavor, but it
            // raises a UnsupportedFlavorException on Mac OS X

            if (content == null) {
                try {
                    content = new DataFlavor().getReaderForText(transferable);
                }
                catch (Exception e) {
                    // ignore
                }
            }

            if (content == null) {
                return false;
            }

            String value;

            if (content instanceof Reader) {
                // TODO: we might want instead connect to the input stream
                // so we can interpret individual lines
                value = "";
                String line;

                BufferedReader read = new BufferedReader((Reader) content);
                while ((line = read.readLine()) != null) {
                    if (value.length() > 0) {
                        value += "\n";
                    }

                    value += line;
                }
            }
            else {
                value = content.toString();
            }

            if (value == null) {
                return true;
            }

            putString(value);

            return true;
        }
        catch (UnsupportedFlavorException e) {
            Log.error("Paste failed: ", e);

            return false;
        }
    }

    //
    // Triggered Actions
    //

    private final Map<Character, ActionListener> triggeredActions = new HashMap<Character, ActionListener>();

    /**
     * Adding a triggered Action allows to give another curse of action if a character passed the pre-processing.
     * <p/>
     * Say you want to close the application if the user enter q.
     * addTriggerAction('q', new ActionListener(){ System.exit(0); }); would do the trick.
     */
    public void addTriggeredAction(final char c, final ActionListener listener) {
        triggeredActions.put(c, listener);
    }

    //
    // Formatted Output
    //

    /**
     * Output the specified {@link Collection} in proper columns.
     */
    public void printColumns(final Collection<? extends CharSequence> items) throws IOException {
        if (items == null || items.isEmpty()) {
            return;
        }

        int width = getTerminal().getWidth();
        int height = getTerminal().getHeight();

        int maxWidth = 0;
        for (CharSequence item : items) {
            maxWidth = Math.max(maxWidth, item.length());
        }
        Log.debug("Max width: ", maxWidth);

        int showLines;
        if (isPaginationEnabled()) {
            showLines = height - 1; // page limit
        }
        else {
            showLines = Integer.MAX_VALUE;
        }

        StringBuilder buff = new StringBuilder();
        for (CharSequence item : items) {
            if ((buff.length() + maxWidth) > width) {
                println(buff);
                buff.setLength(0);

                if (--showLines == 0) {
                    // Overflow
                    print(resources.getString("display-more"));
                    flush();
                    int c = readVirtualKey();
                    if (c == '\r' || c == '\n') {
                        // one step forward
                        showLines = 1;
                    }
                    else if (c != 'q') {
                        // page forward
                        showLines = height - 1;
                    }

                    back(resources.getString("display-more").length());
                    if (c == 'q') {
                        // cancel
                        break;
                    }
                }
            }

            // NOTE: toString() is important here due to AnsiString being retarded
            buff.append(item.toString());
            for (int i = 0; i < (maxWidth + 3 - item.length()); i++) {
                buff.append(' ');
            }
        }

        if (buff.length() > 0) {
            println(buff);
        }
    }

    //
    // Non-supported Terminal Support
    //

    private Thread maskThread;

    private void beforeReadLine(final String prompt, final Character mask) {
        if (mask != null && maskThread == null) {
            final String fullPrompt = "\r" + prompt
                + "                 "
                + "                 "
                + "                 "
                + "\r" + prompt;

            maskThread = new Thread()
            {
                public void run() {
                    while (!interrupted()) {
                        try {
                            Writer out = getOutput();
                            out.write(fullPrompt);
                            out.flush();
                            sleep(3);
                        }
                        catch (IOException e) {
                            return;
                        }
                        catch (InterruptedException e) {
                            return;
                        }
                    }
                }
            };

            maskThread.setPriority(Thread.MAX_PRIORITY);
            maskThread.setDaemon(true);
            maskThread.start();
        }
    }

    private void afterReadLine() {
        if (maskThread != null && maskThread.isAlive()) {
            maskThread.interrupt();
        }

        maskThread = null;
    }

    /**
     * Erases the current line with the existing prompt, then redraws the line
     * with the provided prompt and buffer
     * @param prompt
     *            the new prompt
     * @param buffer
     *            the buffer to be drawn
     * @param cursorDest
     *            where you want the cursor set when the line has been drawn.
     *            -1 for end of line.
     * */
    public void resetPromptLine(String prompt, String buffer, int cursorDest) throws IOException {
        // move cursor to end of line
        moveToEnd();

        // backspace all text, including prompt
        buf.buffer.append(this.prompt);
        buf.cursor += this.prompt.length();
        this.prompt = "";
        backspaceAll();

        this.prompt = prompt;
        redrawLine();
        setBuffer(buffer);

        // move cursor to destination (-1 will move to end of line)
        if (cursorDest < 0) cursorDest = buffer.length();
        setCursorPosition(cursorDest);

        flush();
    }

    public void printSearchStatus(String searchTerm, String match) throws IOException {
        String prompt = "(reverse-i-search)`" + searchTerm + "': ";
        String buffer = match;
        int cursorDest = match.indexOf(searchTerm);
        resetPromptLine(prompt, buffer, cursorDest);
    }

    public void restoreLine(String originalPrompt, int cursorDest) throws IOException {
        // TODO move cursor to matched string
        String prompt = lastLine(originalPrompt);
        String buffer = buf.buffer.toString();
        resetPromptLine(prompt, buffer, cursorDest);
    }

    //
    // History search
    //
    /**
     * Search backward in history from a given position.
     *
     * @param searchTerm substring to search for.
     * @param startIndex the index from which on to search
     * @return index where this substring has been found, or -1 else.
     */
    public int searchBackwards(String searchTerm, int startIndex) {
        return searchBackwards(searchTerm, startIndex, false);
    }

    /**
     * Search backwards in history from the current position.
     *
     * @param searchTerm substring to search for.
     * @return index where the substring has been found, or -1 else.
     */
    public int searchBackwards(String searchTerm) {
        return searchBackwards(searchTerm, history.index());
    }


    public int searchBackwards(String searchTerm, int startIndex, boolean startsWith) {
        ListIterator<History.Entry> it = history.entries(startIndex);
        while (it.hasPrevious()) {
            History.Entry e = it.previous();
            if (startsWith) {
                if (e.value().toString().startsWith(searchTerm)) {
                    return e.index();
                }
            } else {
                if (e.value().toString().contains(searchTerm)) {
                    return e.index();
                }
            }
        }
        return -1;
    }

    //
    // Helpers
    //

    /**
     * Checks to see if the specified character is a delimiter. We consider a
     * character a delimiter if it is anything but a letter or digit.
     *
     * @param c     The character to test
     * @return      True if it is a delimiter
     */
    private boolean isDelimiter(final char c) {
        return !Character.isLetterOrDigit(c);
    }

    private void printAnsiSequence(String sequence) throws IOException {
        print(27);
        print('[');
        print(sequence);
        flush(); // helps with step debugging
    }

    // return column position, reported by the terminal
    private int getCurrentPosition() {
        // check for ByteArrayInputStream to disable for unit tests
        if (getTerminal().isAnsiSupported() && !(in instanceof ByteArrayInputStream)) {
            try {
                printAnsiSequence("6n");
                flush();
                StringBuffer b = new StringBuffer(8);
                // position is sent as <ESC>[{ROW};{COLUMN}R
                int r;
                while((r = in.read()) > -1 && r != 'R') {
                    if (r != 27 && r != '[') {
                        b.append((char) r);
                    }
                }
                String[] pos = b.toString().split(";");
                return Integer.parseInt(pos[1]);
            } catch (Exception x) {
                // no luck
            }
        }

        return -1; // TODO: throw exception instead?
    }

    // return row position, reported by the terminal
    // needed to know whether to scroll up on cursor move in last col for weird
    // wrapping terminals - not tested for anything else
    private int getCurrentAnsiRow() {
        // check for ByteArrayInputStream to disable for unit tests
        if (getTerminal().isAnsiSupported() && !(in instanceof ByteArrayInputStream)) {
            try {
                printAnsiSequence("6n");
                flush();
                StringBuffer b = new StringBuffer(8);
                // position is sent as <ESC>[{ROW};{COLUMN}R
                int r;
                while((r = in.read()) > -1 && r != 'R') {
                    if (r != 27 && r != '[') {
                        b.append((char) r);
                    }
                }
                String[] pos = b.toString().split(";");
                return Integer.parseInt(pos[0]);
            } catch (Exception x) {
                // no luck
            }
        }

        return -1; // TODO: throw exception instead?
    }
}
