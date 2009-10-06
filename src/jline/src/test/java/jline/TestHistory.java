/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;


/**
 *  Tests command history.
 *
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class TestHistory extends JLineTestCase {
    public TestHistory(String test) {
        super(test);
    }

    public void testSingleHistory() throws Exception {
        Buffer b = new Buffer().
            append("test line 1").op(ConsoleReader.NEWLINE).
            append("test line 2").op(ConsoleReader.NEWLINE).
            append("test line 3").op(ConsoleReader.NEWLINE).
            append("test line 4").op(ConsoleReader.NEWLINE).
            append("test line 5").op(ConsoleReader.NEWLINE).
            append("");

        assertBuffer("", b);

        assertBuffer("test line 5", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 4", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 5", b = b.op(ConsoleReader.NEXT_HISTORY));
        assertBuffer("test line 4", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 3", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 2", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 1", b = b.op(ConsoleReader.PREV_HISTORY));

        // beginning of history
        assertBuffer("test line 1", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 1", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 1", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 1", b = b.op(ConsoleReader.PREV_HISTORY));

        assertBuffer("test line 2", b = b.op(ConsoleReader.NEXT_HISTORY));
        assertBuffer("test line 3", b = b.op(ConsoleReader.NEXT_HISTORY));
        assertBuffer("test line 4", b = b.op(ConsoleReader.NEXT_HISTORY));
        assertBuffer("test line 5", b = b.op(ConsoleReader.NEXT_HISTORY));

        // end of history
        assertBuffer("", b = b.op(ConsoleReader.NEXT_HISTORY));
        assertBuffer("", b = b.op(ConsoleReader.NEXT_HISTORY));
        assertBuffer("", b = b.op(ConsoleReader.NEXT_HISTORY));

        assertBuffer("test line 5", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 4", b = b.op(ConsoleReader.PREV_HISTORY));
        b = b.op(ConsoleReader.MOVE_TO_BEG).append("XXX")
             .op(ConsoleReader.NEWLINE);
        assertBuffer("XXXtest line 4", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 5", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 4", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("test line 5", b = b.op(ConsoleReader.NEXT_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(ConsoleReader.NEXT_HISTORY));
        assertBuffer("", b = b.op(ConsoleReader.NEXT_HISTORY));

        assertBuffer("XXXtest line 4", b = b.op(ConsoleReader.PREV_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(ConsoleReader.NEWLINE).
            op(ConsoleReader.PREV_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(ConsoleReader.NEWLINE).
            op(ConsoleReader.PREV_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(ConsoleReader.NEWLINE).
            op(ConsoleReader.PREV_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(ConsoleReader.NEWLINE).
            op(ConsoleReader.PREV_HISTORY));
    }
}
