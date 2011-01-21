package scala.tools.jline;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * Tests for the {@link TerminalFactory}.
 */
public class TerminalFactoryTest
{
    @Before
    public void setUp() throws Exception {
        TerminalFactory.reset();
    }

    @Test
    public void testConfigureNone() {
        TerminalFactory.configure(TerminalFactory.NONE);
        Terminal t = TerminalFactory.get();
        assertNotNull(t);
        assertEquals(UnsupportedTerminal.class.getName(), t.getClass().getName());
    }

    @Test
    public void testConfigureUnsupportedTerminal() {
        TerminalFactory.configure(UnsupportedTerminal.class.getName());
        Terminal t = TerminalFactory.get();
        assertNotNull(t);
        assertEquals(UnsupportedTerminal.class.getName(), t.getClass().getName());
    }
}