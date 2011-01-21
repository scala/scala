package scala.tools.jline.console;

import scala.tools.jline.TerminalFactory;
import scala.tools.jline.WindowsTerminal;
import scala.tools.jline.console.history.History;
import scala.tools.jline.console.history.MemoryHistory;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;

import static scala.tools.jline.WindowsTerminal.WindowsKey.DELETE_KEY;
import static scala.tools.jline.WindowsTerminal.WindowsKey.END_KEY;
import static scala.tools.jline.WindowsTerminal.WindowsKey.ESCAPE_KEY;
import static scala.tools.jline.WindowsTerminal.WindowsKey.HOME_KEY;
import static scala.tools.jline.WindowsTerminal.WindowsKey.INSERT_KEY;
import static scala.tools.jline.WindowsTerminal.WindowsKey.LEFT_ARROW_KEY;
import static scala.tools.jline.WindowsTerminal.WindowsKey.NUMPAD_KEY_INDICATOR;
import static scala.tools.jline.WindowsTerminal.WindowsKey.PAGE_DOWN_KEY;
import static scala.tools.jline.WindowsTerminal.WindowsKey.PAGE_UP_KEY;
import static scala.tools.jline.WindowsTerminal.WindowsKey.SPECIAL_KEY_INDICATOR;
import static scala.tools.jline.console.Operation.DELETE_NEXT_CHAR;
import static scala.tools.jline.console.Operation.DELETE_PREV_CHAR;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * Tests for the {@link ConsoleReader}.
 */
public class ConsoleReaderTest
{
    @Before
    public void setUp() throws Exception {
        System.setProperty(WindowsTerminal.JLINE_WINDOWS_TERMINAL_DIRECT_CONSOLE, "false");
    }

    private void assertWindowsKeyBehavior(String expected, char[] input) throws Exception {
        StringBuilder buffer = new StringBuilder();
        buffer.append(input);
        ConsoleReader reader = createConsole(buffer.toString().getBytes());
        assertNotNull(reader);
        String line = reader.readLine();
        assertEquals(expected, line);
    }

    private ConsoleReader createConsole(byte[] bytes) throws Exception {
        InputStream in = new ByteArrayInputStream(bytes);
        Writer writer = new StringWriter();
        ConsoleReader reader = new ConsoleReader(in, writer);
        reader.setHistory(createSeededHistory());
        return reader;
    }

    private History createSeededHistory() {
        History history = new MemoryHistory();
        history.add("dir");
        history.add("cd c:\\");
        history.add("mkdir monkey");
        return history;
    }

    @Test
    public void testDeleteAndBackspaceKeymappings() throws Exception {
        // test only works on Windows
        if (!(TerminalFactory.get() instanceof WindowsTerminal)) {
            return;
        }

        ConsoleReader consoleReader = new ConsoleReader();
        assertNotNull(consoleReader);
        assertEquals(127, consoleReader.getKeyForAction(DELETE_NEXT_CHAR));
        assertEquals(8, consoleReader.getKeyForAction(DELETE_PREV_CHAR));
    }

    @Test
    public void testReadline() throws Exception {
        ConsoleReader consoleReader = createConsole("Sample String\r\n".getBytes());
        assertNotNull(consoleReader);
        String line = consoleReader.readLine();
        assertEquals("Sample String", line);
    }

    @Test
    public void testDeleteOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(TerminalFactory.get() instanceof WindowsTerminal)) {
            return;
        }

        char[] characters = new char[]{
            'S', 's',
            (char) SPECIAL_KEY_INDICATOR.code,
            (char) LEFT_ARROW_KEY.code,
            (char) SPECIAL_KEY_INDICATOR.code,
            (char) DELETE_KEY.code, '\r', 'n'
        };
        assertWindowsKeyBehavior("S", characters);
    }

    @Test
    public void testNumpadDeleteOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(TerminalFactory.get() instanceof WindowsTerminal)) {
            return;
        }

        char[] characters = new char[]{
            'S', 's',
            (char) NUMPAD_KEY_INDICATOR.code,
            (char) LEFT_ARROW_KEY.code,
            (char) NUMPAD_KEY_INDICATOR.code,
            (char) DELETE_KEY.code, '\r', 'n'
        };
        assertWindowsKeyBehavior("S", characters);
    }

    @Test
    public void testHomeKeyOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(TerminalFactory.get() instanceof WindowsTerminal)) {
            return;
        }

        char[] characters = new char[]{
            'S', 's',
            (char) SPECIAL_KEY_INDICATOR.code,
            (char) HOME_KEY.code, 'x', '\r', '\n'
        };
        assertWindowsKeyBehavior("xSs", characters);

    }

    @Test
    public void testEndKeyOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(TerminalFactory.get() instanceof WindowsTerminal)) {
            return;
        }

        char[] characters = new char[]{
            'S', 's',
            (char) SPECIAL_KEY_INDICATOR.code,
            (char) HOME_KEY.code, 'x',
            (char) SPECIAL_KEY_INDICATOR.code, (char) END_KEY.code,
            'j', '\r', '\n'
        };
        assertWindowsKeyBehavior("xSsj", characters);
    }

    @Test
    public void testPageUpOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(TerminalFactory.get() instanceof WindowsTerminal)) {
            return;
        }

        char[] characters = new char[]{
            (char) SPECIAL_KEY_INDICATOR.code,
            (char) PAGE_UP_KEY.code, '\r', '\n'
        };
        assertWindowsKeyBehavior("dir", characters);
    }

    @Test
    public void testPageDownOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(TerminalFactory.get() instanceof WindowsTerminal)) {
            return;
        }

        char[] characters = new char[]{
            (char) SPECIAL_KEY_INDICATOR.code,
            (char) PAGE_DOWN_KEY.code, '\r', '\n'
        };
        assertWindowsKeyBehavior("mkdir monkey", characters);
    }

    @Test
    public void testEscapeOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(TerminalFactory.get() instanceof WindowsTerminal)) {
            return;
        }

        char[] characters = new char[]{
            's', 's', 's',
            (char) SPECIAL_KEY_INDICATOR.code,
            (char) ESCAPE_KEY.code, '\r', '\n'
        };
        assertWindowsKeyBehavior("", characters);
    }

    @Test
    public void testInsertOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(TerminalFactory.get() instanceof WindowsTerminal)) {
            return;
        }

        char[] characters = new char[]{
            'o', 'p', 's',
            (char) SPECIAL_KEY_INDICATOR.code,
            (char) HOME_KEY.code,
            (char) SPECIAL_KEY_INDICATOR.code,
            (char) INSERT_KEY.code, 'o', 'o', 'p', 's', '\r', '\n'
        };
        assertWindowsKeyBehavior("oops", characters);
    }

    @Test
    public void testExpansion() throws Exception {
        ConsoleReader reader = new ConsoleReader();
        MemoryHistory history = new MemoryHistory();
        history.setMaxSize(3);
        history.add("foo");
        history.add("dir");
        history.add("cd c:\\");
        history.add("mkdir monkey");
        reader.setHistory(history);

        assertEquals("echo a!", reader.expandEvents("echo a!"));
        assertEquals("mkdir monkey ; echo a!", reader.expandEvents("!! ; echo a!"));
        assertEquals("echo ! a", reader.expandEvents("echo ! a"));
        assertEquals("echo !\ta", reader.expandEvents("echo !\ta"));

        assertEquals("mkdir barey", reader.expandEvents("^monk^bar^"));
        assertEquals("mkdir barey", reader.expandEvents("^monk^bar"));
        assertEquals("a^monk^bar", reader.expandEvents("a^monk^bar"));

        assertEquals("mkdir monkey", reader.expandEvents("!!"));
        assertEquals("echo echo a", reader.expandEvents("echo !#a"));

        assertEquals("mkdir monkey", reader.expandEvents("!mk"));
        try {
            reader.expandEvents("!mz");
        } catch (IllegalArgumentException e) {
            assertEquals("!mz: event not found", e.getMessage());
        }

        assertEquals("mkdir monkey", reader.expandEvents("!?mo"));
        assertEquals("mkdir monkey", reader.expandEvents("!?mo?"));

        assertEquals("mkdir monkey", reader.expandEvents("!-1"));
        assertEquals("cd c:\\", reader.expandEvents("!-2"));
        assertEquals("cd c:\\", reader.expandEvents("!2"));
        assertEquals("mkdir monkey", reader.expandEvents("!3"));
        try {
            reader.expandEvents("!20");
        } catch (IllegalArgumentException e) {
            assertEquals("!20: event not found", e.getMessage());
        }
        try {
            reader.expandEvents("!-20");
        } catch (IllegalArgumentException e) {
            assertEquals("!-20: event not found", e.getMessage());
        }
    }
}
