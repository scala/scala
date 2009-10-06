/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;

import junit.framework.*;

import java.io.*;

public abstract class JLineTestCase extends TestCase {
    ConsoleReader console;

    public JLineTestCase(String test) {
        super(test);
    }

    public void setUp() throws Exception {
        super.setUp();
        console = new ConsoleReader(null, new PrintWriter(
                new OutputStreamWriter(new ByteArrayOutputStream())), null,
                new UnixTerminal());
    }

    public void assertBuffer(String expected, Buffer buffer) throws IOException {
        assertBuffer(expected, buffer, true);
    }

    public void assertBuffer(String expected, Buffer buffer, boolean clear)
            throws IOException {
        // clear current buffer, if any
        if (clear) {
            console.finishBuffer();
            console.getHistory().clear();
        }

        console.setInput(new ByteArrayInputStream(buffer.getBytes()));

        // run it through the reader
        while (console.readLine((String) null) != null) {
            ;
        }

        assertEquals(expected, console.getCursorBuffer().toString());
    }

    private int getKeyForAction(short logicalAction) {
        int action = console.getKeyForAction(logicalAction);

        if (action == -1) {
            fail("Keystroke for logical action " + logicalAction
                    + " was not bound in the console");
        }

        return action;
    }

    /**
     * TODO: Fix this so tests don't break on windows machines.
     *
     * @author Ryan
     *
     */
    class Buffer {
        private final ByteArrayOutputStream bout = new ByteArrayOutputStream();

        public Buffer() {
        }

        public Buffer(String str) {
            append(str);
        }

        public byte[] getBytes() {
            return bout.toByteArray();
        }

        public Buffer op(short operation) {
            return append(getKeyForAction(operation));
        }

        public Buffer ctrlA() {
            return append(getKeyForAction(ConsoleReader.MOVE_TO_BEG));
        }

        public Buffer ctrlU() {
            return append(getKeyForAction(ConsoleReader.KILL_LINE_PREV));
        }

        public Buffer tab() {
            return append(getKeyForAction(ConsoleReader.COMPLETE));
        }

        public Buffer back() {
            return append(getKeyForAction(ConsoleReader.DELETE_PREV_CHAR));
        }

        public Buffer left() {
            return append(UnixTerminal.ARROW_START).append(
                    UnixTerminal.ARROW_PREFIX).append(UnixTerminal.ARROW_LEFT);
        }

        public Buffer right() {
            return append(UnixTerminal.ARROW_START).append(
                    UnixTerminal.ARROW_PREFIX).append(UnixTerminal.ARROW_RIGHT);
        }

        public Buffer up() {
            return append(UnixTerminal.ARROW_START).append(
                    UnixTerminal.ARROW_PREFIX).append(UnixTerminal.ARROW_UP);
        }

        public Buffer down() {
            return append(UnixTerminal.ARROW_START).append(
                    UnixTerminal.ARROW_PREFIX).append(UnixTerminal.ARROW_DOWN);
        }

        public Buffer append(String str) {
            byte[] bytes = str.getBytes();

            for (int i = 0; i < bytes.length; i++) {
                append(bytes[i]);
            }

            return this;
        }

        public Buffer append(int i) {
            return append((byte) i);
        }

        public Buffer append(byte b) {
            bout.write(b);

            return this;
        }
    }
}
