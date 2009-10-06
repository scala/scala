/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;


/**
 *  Tests various features of editing lines.
 *
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class TestEditLine extends JLineTestCase {
    public TestEditLine(String test) {
        super(test);
    }

    public void testDeletePreviousWord() throws Exception {
        Buffer b = new Buffer("This is a test");

        assertBuffer("This is a ", b = b.op(ConsoleReader.DELETE_PREV_WORD));
        assertBuffer("This is ", b = b.op(ConsoleReader.DELETE_PREV_WORD));
        assertBuffer("This ", b = b.op(ConsoleReader.DELETE_PREV_WORD));
        assertBuffer("", b = b.op(ConsoleReader.DELETE_PREV_WORD));
        assertBuffer("", b = b.op(ConsoleReader.DELETE_PREV_WORD));
        assertBuffer("", b = b.op(ConsoleReader.DELETE_PREV_WORD));
    }

    public void testMoveToEnd() throws Exception {
        Buffer b = new Buffer("This is a test");

        assertBuffer("This is a XtestX",
                     new Buffer("This is a test").op(ConsoleReader.PREV_WORD)
                                                 .append('X')
                                                 .op(ConsoleReader.MOVE_TO_END)
                                                 .append('X'));

        assertBuffer("This is Xa testX",
                     new Buffer("This is a test").op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .append('X')
                                                 .op(ConsoleReader.MOVE_TO_END)
                                                 .append('X'));

        assertBuffer("This Xis a testX",
                     new Buffer("This is a test").op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .append('X')
                                                 .op(ConsoleReader.MOVE_TO_END)
                                                 .append('X'));
    }

    public void testPreviousWord() throws Exception {
        assertBuffer("This is a Xtest",
                     new Buffer("This is a test").op(ConsoleReader.PREV_WORD)
                                                 .append('X'));
        assertBuffer("This is Xa test",
                     new Buffer("This is a test").op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .append('X'));
        assertBuffer("This Xis a test",
                     new Buffer("This is a test").op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .append('X'));
        assertBuffer("XThis is a test",
                     new Buffer("This is a test").op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .append('X'));
        assertBuffer("XThis is a test",
                     new Buffer("This is a test").op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .append('X'));
        assertBuffer("XThis is a test",
                     new Buffer("This is a test").op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .op(ConsoleReader.PREV_WORD)
                                                 .append('X'));
    }

    public void testLineStart() throws Exception {
        assertBuffer("XThis is a test",
                     new Buffer("This is a test").ctrlA().append('X'));
        assertBuffer("TXhis is a test",
                     new Buffer("This is a test").ctrlA().right().append('X'));
    }

    public void testClearLine() throws Exception {
        assertBuffer("", new Buffer("This is a test").ctrlU());
        assertBuffer("t", new Buffer("This is a test").left().ctrlU());
        assertBuffer("st", new Buffer("This is a test").left().left().ctrlU());
    }

    public void testRight() throws Exception {
        Buffer b = new Buffer("This is a test");
        b = b.left().right().back();
        assertBuffer("This is a tes", b);
        b = b.left().left().left().right().left().back();
        assertBuffer("This is ates", b);
        b.append('X');
        assertBuffer("This is aXtes", b);
    }

    public void testLeft() throws Exception {
        Buffer b = new Buffer("This is a test");
        b = b.left().left().left();
        assertBuffer("This is a est", b = b.back());
        assertBuffer("This is aest", b = b.back());
        assertBuffer("This is est", b = b.back());
        assertBuffer("This isest", b = b.back());
        assertBuffer("This iest", b = b.back());
        assertBuffer("This est", b = b.back());
        assertBuffer("Thisest", b = b.back());
        assertBuffer("Thiest", b = b.back());
        assertBuffer("Thest", b = b.back());
        assertBuffer("Test", b = b.back());
        assertBuffer("est", b = b.back());
        assertBuffer("est", b = b.back());
        assertBuffer("est", b = b.back());
        assertBuffer("est", b = b.back());
        assertBuffer("est", b = b.back());
    }

    public void testBackspace() throws Exception {
        Buffer b = new Buffer("This is a test");
        assertBuffer("This is a tes", b = b.back());
        assertBuffer("This is a te", b = b.back());
        assertBuffer("This is a t", b = b.back());
        assertBuffer("This is a ", b = b.back());
        assertBuffer("This is a", b = b.back());
        assertBuffer("This is ", b = b.back());
        assertBuffer("This is", b = b.back());
        assertBuffer("This i", b = b.back());
        assertBuffer("This ", b = b.back());
        assertBuffer("This", b = b.back());
        assertBuffer("Thi", b = b.back());
        assertBuffer("Th", b = b.back());
        assertBuffer("T", b = b.back());
        assertBuffer("", b = b.back());
        assertBuffer("", b = b.back());
        assertBuffer("", b = b.back());
        assertBuffer("", b = b.back());
        assertBuffer("", b = b.back());
    }

    public void testBuffer() throws Exception {
        assertBuffer("This is a test", new Buffer("This is a test"));
    }
}
