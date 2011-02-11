/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */

package scala.tools.jline;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import scala.tools.jline.internal.Configuration;
import org.fusesource.jansi.internal.WindowsSupport;

import scala.tools.jline.internal.Log;
import scala.tools.jline.internal.ReplayPrefixOneCharInputStream;

import static scala.tools.jline.WindowsTerminal.ConsoleMode.*;
import static scala.tools.jline.WindowsTerminal.WindowsKey.*;
import static scala.tools.jline.console.Key.*;

/**
 * Terminal implementation for Microsoft Windows. Terminal initialization in
 * {@link #init} is accomplished by extracting the
 * <em>jline_<i>version</i>.dll</em>, saving it to the system temporary
 * directoy (determined by the setting of the <em>java.io.tmpdir</em> System
 * property), loading the library, and then calling the Win32 APIs <a
 * href="http://msdn.microsoft.com/library/default.asp?
 * url=/library/en-us/dllproc/base/setconsolemode.asp">SetConsoleMode</a> and
 * <a href="http://msdn.microsoft.com/library/default.asp?
 * url=/library/en-us/dllproc/base/getconsolemode.asp">GetConsoleMode</a> to
 * disable character echoing.
 * <p/>
 * <p>
 * By default, the {@link #readCharacter} method will attempt to test to see if
 * the specified {@link InputStream} is {@link System#in} or a wrapper around
 * {@link FileDescriptor#in}, and if so, will bypass the character reading to
 * directly invoke the readc() method in the JNI library. This is so the class
 * can read special keys (like arrow keys) which are otherwise inaccessible via
 * the {@link System#in} stream. Using JNI reading can be bypassed by setting
 * the <code>jline.WindowsTerminal.directConsole</code> system property
 * to <code>false</code>.
 * </p>
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 * @since 2.0
 */
public class WindowsTerminal
    extends TerminalSupport
{
    public static final String JLINE_WINDOWS_TERMINAL_INPUT_ENCODING = "jline.WindowsTerminal.input.encoding";

    public static final String JLINE_WINDOWS_TERMINAL_OUTPUT_ENCODING = "jline.WindowsTerminal.output.encoding";

    public static final String JLINE_WINDOWS_TERMINAL_DIRECT_CONSOLE = "jline.WindowsTerminal.directConsole";

    public static final String WINDOWSBINDINGS_PROPERTIES = "windowsbindings.properties";

    public static final String ANSI = WindowsTerminal.class.getName() + ".ansi";

    private boolean directConsole;

    private int originalMode;

    private final ReplayPrefixOneCharInputStream replayStream;

    private final InputStreamReader replayReader;

    public WindowsTerminal() throws Exception {
        super(true);

        this.replayStream =
            new ReplayPrefixOneCharInputStream(Configuration.getString(JLINE_WINDOWS_TERMINAL_INPUT_ENCODING, Configuration.getFileEncoding()));
        this.replayReader = new InputStreamReader(replayStream, replayStream.getEncoding());
    }

    @Override
    public void init() throws Exception {
        super.init();

        setAnsiSupported(Boolean.getBoolean(ANSI));

        //
        // FIXME: Need a way to disable direct console and sysin detection muck
        //

        setDirectConsole(Boolean.getBoolean(JLINE_WINDOWS_TERMINAL_DIRECT_CONSOLE));

        this.originalMode = getConsoleMode();
        setConsoleMode(originalMode & ~ENABLE_ECHO_INPUT.code);
        setEchoEnabled(false);
    }

    /**
     * Restore the original terminal configuration, which can be used when
     * shutting down the console reader. The ConsoleReader cannot be
     * used after calling this method.
     */
    @Override
    public void restore() throws Exception {
        // restore the old console mode
        setConsoleMode(originalMode);
        super.restore();
    }

    @Override
    public int getWidth() {
        int w = getWindowsTerminalWidth();
        return w < 1 ? DEFAULT_WIDTH : w;
    }

    @Override
    public int getHeight() {
        int h = getWindowsTerminalHeight();
        return h < 1 ? DEFAULT_HEIGHT : h;
    }

    @Override
    public void setEchoEnabled(final boolean enabled) {
        // Must set these four modes at the same time to make it work fine.
        if (enabled) {
            setConsoleMode(getConsoleMode() |
                ENABLE_ECHO_INPUT.code |
                ENABLE_LINE_INPUT.code |
                ENABLE_PROCESSED_INPUT.code |
                ENABLE_WINDOW_INPUT.code);
        }
        else {
            setConsoleMode(getConsoleMode() &
                ~(ENABLE_LINE_INPUT.code |
                    ENABLE_ECHO_INPUT.code |
                    ENABLE_PROCESSED_INPUT.code |
                    ENABLE_WINDOW_INPUT.code));
        }
        super.setEchoEnabled(enabled);
    }

    /**
     * Whether or not to allow the use of the JNI console interaction.
     */
    public void setDirectConsole(final boolean flag) {
        this.directConsole = flag;
        Log.debug("Direct console: ", flag);
    }

    /**
     * Whether or not to allow the use of the JNI console interaction.
     */
    public Boolean getDirectConsole() {
        return directConsole;
    }


    @Override
    public int readCharacter(final InputStream in) throws IOException {
        // if we can detect that we are directly wrapping the system
        // input, then bypass the input stream and read directly (which
        // allows us to access otherwise unreadable strokes, such as
        // the arrow keys)

        if (directConsole || isSystemIn(in)) {
            return readByte();
        }
        else {
            return super.readCharacter(in);
        }
    }

    private boolean isSystemIn(final InputStream in) throws IOException {
        assert in != null;

        if (in == System.in) {
            return true;
        }
        else if (in instanceof FileInputStream && ((FileInputStream) in).getFD() == FileDescriptor.in) {
            return true;
        }

        return false;
    }

    @Override
    public int readVirtualKey(final InputStream in) throws IOException {
        int indicator = readCharacter(in);

        // in Windows terminals, arrow keys are represented by
        // a sequence of 2 characters. E.g., the up arrow
        // key yields 224, 72
        if (indicator == SPECIAL_KEY_INDICATOR.code || indicator == NUMPAD_KEY_INDICATOR.code) {
            int c = readCharacter(in);
            WindowsKey key = WindowsKey.valueOf(c);
            if (key == null)
                return 0;

            switch (key) {
                case UP_ARROW_KEY:
                    return CTRL_P.code; // translate UP -> CTRL-P

                case LEFT_ARROW_KEY:
                    return CTRL_B.code; // translate LEFT -> CTRL-B

                case RIGHT_ARROW_KEY:
                    return CTRL_F.code; // translate RIGHT -> CTRL-F

                case DOWN_ARROW_KEY:
                    return CTRL_N.code; // translate DOWN -> CTRL-N

                case DELETE_KEY:
                    return CTRL_QM.code; // translate DELETE -> CTRL-?

                case HOME_KEY:
                    return CTRL_A.code;

                case END_KEY:
                    return CTRL_E.code;

                case PAGE_UP_KEY:
                    return CTRL_K.code;

                case PAGE_DOWN_KEY:
                    return CTRL_L.code;

                case ESCAPE_KEY:
                    return CTRL_OB.code; // translate ESCAPE -> CTRL-[

                case INSERT_KEY:
                    return CTRL_C.code;

                default:
                    return 0;
            }
        }
        else if (indicator > 128) {
            // handle unicode characters longer than 2 bytes,
            // thanks to Marc.Herbert@continuent.com
            replayStream.setInput(indicator, in);
            // replayReader = new InputStreamReader(replayStream, encoding);
            indicator = replayReader.read();

        }

        return indicator;
    }

    @Override
    public InputStream getDefaultBindings() {
        return WindowsTerminal.class.getResourceAsStream(WINDOWSBINDINGS_PROPERTIES);
    }

    //
    // Native Bits
    //
    private int getConsoleMode() {
        return WindowsSupport.getConsoleMode();
    }

    private void setConsoleMode(int mode) {
        WindowsSupport.setConsoleMode(mode);
    }

    private int readByte() {
        return WindowsSupport.readByte();
    }

    private int getWindowsTerminalWidth() {
        return WindowsSupport.getWindowsTerminalWidth();
    }

    private int getWindowsTerminalHeight() {
        return WindowsSupport.getWindowsTerminalHeight();
    }

    /**
     * Console mode
     * <p/>
     * Constants copied <tt>wincon.h</tt>.
     */
    public static enum ConsoleMode
    {
        /**
         * The ReadFile or ReadConsole function returns only when a carriage return
         * character is read. If this mode is disable, the functions return when one
         * or more characters are available.
         */
        ENABLE_LINE_INPUT(2),

        /**
         * Characters read by the ReadFile or ReadConsole function are written to
         * the active screen buffer as they are read. This mode can be used only if
         * the ENABLE_LINE_INPUT mode is also enabled.
         */
        ENABLE_ECHO_INPUT(4),

        /**
         * CTRL+C is processed by the system and is not placed in the input buffer.
         * If the input buffer is being read by ReadFile or ReadConsole, other
         * control keys are processed by the system and are not returned in the
         * ReadFile or ReadConsole buffer. If the ENABLE_LINE_INPUT mode is also
         * enabled, backspace, carriage return, and linefeed characters are handled
         * by the system.
         */
        ENABLE_PROCESSED_INPUT(1),

        /**
         * User interactions that change the size of the console screen buffer are
         * reported in the console's input buffee. Information about these events
         * can be read from the input buffer by applications using
         * theReadConsoleInput function, but not by those using ReadFile
         * orReadConsole.
         */
        ENABLE_WINDOW_INPUT(8),

        /**
         * If the mouse pointer is within the borders of the console window and the
         * window has the keyboard focus, mouse events generated by mouse movement
         * and button presses are placed in the input buffer. These events are
         * discarded by ReadFile or ReadConsole, even when this mode is enabled.
         */
        ENABLE_MOUSE_INPUT(16),

        /**
         * When enabled, text entered in a console window will be inserted at the
         * current cursor location and all text following that location will not be
         * overwritten. When disabled, all following text will be overwritten. An OR
         * operation must be performed with this flag and the ENABLE_EXTENDED_FLAGS
         * flag to enable this functionality.
         */
        ENABLE_PROCESSED_OUTPUT(1),

        /**
         * This flag enables the user to use the mouse to select and edit text. To
         * enable this option, use the OR to combine this flag with
         * ENABLE_EXTENDED_FLAGS.
         */
        ENABLE_WRAP_AT_EOL_OUTPUT(2),;

        public final int code;

        ConsoleMode(final int code) {
            this.code = code;
        }
    }

    /**
     * Windows keys.
     * <p/>
     * Constants copied <tt>wincon.h</tt>.
     */
    public static enum WindowsKey
    {
        /**
         * On windows terminals, this character indicates that a 'special' key has
         * been pressed. This means that a key such as an arrow key, or delete, or
         * home, etc. will be indicated by the next character.
         */
        SPECIAL_KEY_INDICATOR(224),

        /**
         * On windows terminals, this character indicates that a special key on the
         * number pad has been pressed.
         */
        NUMPAD_KEY_INDICATOR(0),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR,
         * this character indicates an left arrow key press.
         */
        LEFT_ARROW_KEY(75),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR
         * this character indicates an
         * right arrow key press.
         */
        RIGHT_ARROW_KEY(77),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR
         * this character indicates an up
         * arrow key press.
         */
        UP_ARROW_KEY(72),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR
         * this character indicates an
         * down arrow key press.
         */
        DOWN_ARROW_KEY(80),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR
         * this character indicates that
         * the delete key was pressed.
         */
        DELETE_KEY(83),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR
         * this character indicates that
         * the home key was pressed.
         */
        HOME_KEY(71),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR
         * this character indicates that
         * the end key was pressed.
         */
        END_KEY(79),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR
         * this character indicates that
         * the page up key was pressed.
         */
        PAGE_UP_KEY(73),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR
         * this character indicates that
         * the page down key was pressed.
         */
        PAGE_DOWN_KEY(81),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR
         * this character indicates that
         * the insert key was pressed.
         */
        INSERT_KEY(82),

        /**
         * When following the SPECIAL_KEY_INDICATOR or NUMPAD_KEY_INDICATOR,
         * this character indicates that the escape key was pressed.
         */
        ESCAPE_KEY(0),;

        public final int code;

        WindowsKey(final int code) {
            this.code = code;
        }

        private static final Map<Integer, WindowsKey> codes;

        static {
            Map<Integer, WindowsKey> map = new HashMap<Integer, WindowsKey>();

            for (WindowsKey key : WindowsKey.values()) {
                map.put(key.code, key);
            }

            codes = map;
        }

        public static WindowsKey valueOf(final int code) {
            return codes.get(code);
        }
    }
}
