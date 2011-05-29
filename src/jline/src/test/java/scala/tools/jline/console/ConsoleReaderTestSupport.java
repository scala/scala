/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package scala.tools.jline.console;

import scala.tools.jline.UnixTerminal;
import org.junit.Before;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import static scala.tools.jline.UnixTerminal.UnixKey.ARROW_DOWN;
import static scala.tools.jline.UnixTerminal.UnixKey.ARROW_LEFT;
import static scala.tools.jline.UnixTerminal.UnixKey.ARROW_PREFIX;
import static scala.tools.jline.UnixTerminal.UnixKey.ARROW_RIGHT;
import static scala.tools.jline.UnixTerminal.UnixKey.ARROW_START;
import static scala.tools.jline.UnixTerminal.UnixKey.ARROW_UP;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

/**
 * Provides support for console reader tests.
 */
public abstract class ConsoleReaderTestSupport
{
    protected ConsoleReader console;

    @Before
    public void setUp() throws Exception {
        console = new ConsoleReader(null, new PrintWriter(new OutputStreamWriter(new ByteArrayOutputStream())), new UnixTerminal());
    }

    protected void assertBuffer(final String expected, final Buffer buffer) throws IOException {
        assertBuffer(expected, buffer, true);
    }

    protected void assertBuffer(final String expected, final Buffer buffer, final boolean clear) throws IOException {
        // clear current buffer, if any
        if (clear) {
            console.finishBuffer();
            console.getHistory().clear();
        }

        console.setInput(new ByteArrayInputStream(buffer.getBytes()));

        // run it through the reader
        while (console.readLine((String) null) != null) {
            // ignore
        }

        assertEquals(expected, console.getCursorBuffer().toString());
    }

    private int getKeyForAction(final Operation key) {
        return getKeyForAction(key.code);
    }

    private int getKeyForAction(final short logicalAction) {
        int action = console.getKeyForAction(logicalAction);

        if (action == -1) {
            console.printBindings();
            fail("Keystroke for logical action " + logicalAction + " was not bound in the console");
        }

        return action;
    }

    protected class Buffer
    {
        private final ByteArrayOutputStream out = new ByteArrayOutputStream();

        public Buffer() {
            // nothing
        }

        public Buffer(final String str) {
            append(str);
        }

        public byte[] getBytes() {
            return out.toByteArray();
        }

        public Buffer op(final short operation) {
            return append(getKeyForAction(operation));
        }

        public Buffer op(final Operation op) {
            return op(op.code);
        }

        public Buffer ctrlA() {
            return append(getKeyForAction(Operation.MOVE_TO_BEG));
        }

        public Buffer ctrlU() {
            return append(getKeyForAction(Operation.KILL_LINE_PREV));
        }

        public Buffer tab() {
            return append(getKeyForAction(Operation.COMPLETE));
        }

        public Buffer back() {
            return append(getKeyForAction(Operation.DELETE_PREV_CHAR));
        }

        public Buffer left() {
            return append(ARROW_START.code).append(ARROW_PREFIX.code).append(ARROW_LEFT.code);
        }

        public Buffer right() {
            return append(ARROW_START.code).append(ARROW_PREFIX.code).append(ARROW_RIGHT.code);
        }

        public Buffer up() {
            return append(ARROW_START.code).append(ARROW_PREFIX.code).append(ARROW_UP.code);
        }

        public Buffer down() {
            return append(ARROW_START.code).append(ARROW_PREFIX.code).append(ARROW_DOWN.code);
        }

        public Buffer append(final String str) {
            for (byte b : str.getBytes()) {
                append(b);
            }
            return this;
        }

        public Buffer append(final int i) {
            out.write((byte) i);
            return this;
        }
    }
}
