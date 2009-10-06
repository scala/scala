package jline;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;

import junit.framework.TestCase;

public class ConsoleReaderTest extends TestCase {

    public ConsoleReaderTest(String name) {
        super(name);
    }

    protected void setUp() throws Exception {
        System.setProperty("jline.WindowsTerminal.directConsole", "false");
    }

    public void testDeleteAndBackspaceKeymappings() throws Exception {
        // test only works on Windows
        if (!(Terminal.getTerminal() instanceof WindowsTerminal))
            return;

        ConsoleReader consoleReader = new ConsoleReader();
        assertNotNull(consoleReader);
        assertEquals(127, consoleReader
                .getKeyForAction(ConsoleReader.DELETE_NEXT_CHAR));
        assertEquals(8, consoleReader
                .getKeyForAction(ConsoleReader.DELETE_PREV_CHAR));
    }

    public void testReadline() throws Exception {
        ConsoleReader consoleReader = createConsole("Sample String\r\n"
                .getBytes());
        assertNotNull(consoleReader);
        String line = consoleReader.readLine();
        assertEquals("Sample String", line);

    }

    public void testDeleteOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(Terminal.getTerminal() instanceof WindowsTerminal))
            return;

        char[] characters = new char[] { 'S', 's',
                WindowsTerminal.SPECIAL_KEY_INDICATOR,
                WindowsTerminal.LEFT_ARROW_KEY,
                WindowsTerminal.SPECIAL_KEY_INDICATOR,
                WindowsTerminal.DELETE_KEY, '\r', 'n' };
        assertWindowsKeyBehavior("S", characters);
    }

    public void testNumpadDeleteOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(Terminal.getTerminal() instanceof WindowsTerminal))
            return;

        char[] characters = new char[] { 'S', 's',
                WindowsTerminal.NUMPAD_KEY_INDICATOR,
                WindowsTerminal.LEFT_ARROW_KEY,
                WindowsTerminal.NUMPAD_KEY_INDICATOR,
                WindowsTerminal.DELETE_KEY, '\r', 'n' };
        assertWindowsKeyBehavior("S", characters);
    }

    public void testHomeKeyOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(Terminal.getTerminal() instanceof WindowsTerminal))
            return;

        char[] characters = new char[] { 'S', 's',
                WindowsTerminal.SPECIAL_KEY_INDICATOR,
                WindowsTerminal.HOME_KEY, 'x', '\r', '\n' };
        assertWindowsKeyBehavior("xSs", characters);

    }

    public void testEndKeyOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(Terminal.getTerminal() instanceof WindowsTerminal))
            return;

        char[] characters = new char[] { 'S', 's',
                WindowsTerminal.SPECIAL_KEY_INDICATOR,
                WindowsTerminal.HOME_KEY, 'x',
                WindowsTerminal.SPECIAL_KEY_INDICATOR, WindowsTerminal.END_KEY,
                'j', '\r', '\n' };
        assertWindowsKeyBehavior("xSsj", characters);
    }

    public void testPageUpOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(Terminal.getTerminal() instanceof WindowsTerminal))
            return;

        char[] characters = new char[] { WindowsTerminal.SPECIAL_KEY_INDICATOR,
                WindowsTerminal.PAGE_UP_KEY, '\r', '\n' };
        assertWindowsKeyBehavior("dir", characters);
    }

    public void testPageDownOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(Terminal.getTerminal() instanceof WindowsTerminal))
            return;

        char[] characters = new char[] { WindowsTerminal.SPECIAL_KEY_INDICATOR,
                WindowsTerminal.PAGE_DOWN_KEY, '\r', '\n' };
        assertWindowsKeyBehavior("mkdir monkey", characters);
    }

    public void testEscapeOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(Terminal.getTerminal() instanceof WindowsTerminal))
            return;

        char[] characters = new char[] { 's', 's', 's',
                WindowsTerminal.SPECIAL_KEY_INDICATOR,
                WindowsTerminal.ESCAPE_KEY, '\r', '\n' };
        assertWindowsKeyBehavior("", characters);
    }

    public void testInsertOnWindowsTerminal() throws Exception {
        // test only works on Windows
        if (!(Terminal.getTerminal() instanceof WindowsTerminal))
            return;

        char[] characters = new char[] { 'o', 'p', 's',
                WindowsTerminal.SPECIAL_KEY_INDICATOR,
                WindowsTerminal.HOME_KEY,
                WindowsTerminal.SPECIAL_KEY_INDICATOR,
                WindowsTerminal.INSERT_KEY, 'o', 'o', 'p', 's', '\r', '\n' };
        assertWindowsKeyBehavior("oops", characters);
    }

    private void assertWindowsKeyBehavior(String expected, char[] input)
            throws Exception {
        StringBuffer buffer = new StringBuffer();
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
        History history = new History();
        history.addToHistory("dir");
        history.addToHistory("cd c:\\");
        history.addToHistory("mkdir monkey");
        return history;
    }
}
