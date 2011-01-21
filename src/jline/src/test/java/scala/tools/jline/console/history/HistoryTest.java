/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package scala.tools.jline.console.history;

import scala.tools.jline.console.ConsoleReaderTestSupport;
import org.junit.Test;

import static scala.tools.jline.console.Operation.MOVE_TO_BEG;
import static scala.tools.jline.console.Operation.NEWLINE;
import static scala.tools.jline.console.Operation.NEXT_HISTORY;
import static scala.tools.jline.console.Operation.PREV_HISTORY;
import static scala.tools.jline.console.Operation.PREV_CHAR;

/**
 * Tests command history.
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class HistoryTest
    extends ConsoleReaderTestSupport
{
    @Test
    public void testSingleHistory() throws Exception {
        Buffer b = new Buffer().
            append("test line 1").op(NEWLINE).
            append("test line 2").op(NEWLINE).
            append("test line 3").op(NEWLINE).
            append("test line 4").op(NEWLINE).
            append("test line 5").op(NEWLINE).
            append("");

        assertBuffer("", b);

        assertBuffer("test line 5", b = b.op(PREV_HISTORY));
        assertBuffer("test line 5", b = b.op(PREV_CHAR));
        assertBuffer("test line 4", b = b.op(PREV_HISTORY));
        assertBuffer("test line 5", b = b.op(NEXT_HISTORY));
        assertBuffer("test line 4", b = b.op(PREV_HISTORY));
        assertBuffer("test line 3", b = b.op(PREV_HISTORY));
        assertBuffer("test line 2", b = b.op(PREV_HISTORY));
        assertBuffer("test line 1", b = b.op(PREV_HISTORY));

        // beginning of history
        assertBuffer("test line 1", b = b.op(PREV_HISTORY));
        assertBuffer("test line 1", b = b.op(PREV_HISTORY));
        assertBuffer("test line 1", b = b.op(PREV_HISTORY));
        assertBuffer("test line 1", b = b.op(PREV_HISTORY));

        assertBuffer("test line 2", b = b.op(NEXT_HISTORY));
        assertBuffer("test line 3", b = b.op(NEXT_HISTORY));
        assertBuffer("test line 4", b = b.op(NEXT_HISTORY));
        assertBuffer("test line 5", b = b.op(NEXT_HISTORY));

        // end of history
        assertBuffer("", b = b.op(NEXT_HISTORY));
        assertBuffer("", b = b.op(NEXT_HISTORY));
        assertBuffer("", b = b.op(NEXT_HISTORY));

        assertBuffer("test line 5", b = b.op(PREV_HISTORY));
        assertBuffer("test line 4", b = b.op(PREV_HISTORY));
        b = b.op(MOVE_TO_BEG).append("XXX").op(NEWLINE);
        assertBuffer("XXXtest line 4", b = b.op(PREV_HISTORY));
        assertBuffer("test line 5", b = b.op(PREV_HISTORY));
        assertBuffer("test line 4", b = b.op(PREV_HISTORY));
        assertBuffer("test line 5", b = b.op(NEXT_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(NEXT_HISTORY));
        assertBuffer("", b = b.op(NEXT_HISTORY));

        assertBuffer("XXXtest line 4", b = b.op(PREV_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(NEWLINE).op(PREV_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(NEWLINE).op(PREV_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(NEWLINE).op(PREV_HISTORY));
        assertBuffer("XXXtest line 4", b = b.op(NEWLINE).op(PREV_HISTORY));
    }
}
