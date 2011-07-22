/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */

package scala.tools.jline.console;

import java.util.HashMap;
import java.util.Map;

/**
 * Map for console operation to virtual key bindings.
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 * @see java.awt.event.KeyEvent
 * @since 2.0
 */
public enum Operation
{
    /**
     * Unknown operation.
     */
    UNKNOWN(-99),

    /**
     * Operation that moves to the beginning of the buffer.
     */
    MOVE_TO_BEG(-1),

    /**
     * Operation that moves to the end of the buffer.
     */
    MOVE_TO_END(-3),

    /**
     * Operation that moved to the previous character in the buffer.
     */
    PREV_CHAR(-4),

    /**
     * Operation that issues a newline.
     */
    NEWLINE(-6),

    /**
     * Operation that deletes the buffer from the current character to the end.
     */
    KILL_LINE(-7),

    /**
     * Operation that clears the screen.
     */
    CLEAR_SCREEN(-8),

    /**
     * Operation that sets the buffer to the next history item.
     */
    NEXT_HISTORY(-9),

    /**
     * Operation that sets the buffer to the previous history item.
     */
    PREV_HISTORY(-11),

    /**
     * Operation that redisplays the current buffer.
     */
    REDISPLAY(-13),

    /**
     * Operation that deletes the buffer from the cursor to the beginning.
     */
    KILL_LINE_PREV(-15),

    /**
     * Operation that deletes the previous word in the buffer.
     */
    DELETE_PREV_WORD(-16),

    /**
     * Operation that moves to the next character in the buffer.
     */
    NEXT_CHAR(-19),

    /**
     * Operation that moves to the previous character in the buffer.
     */
    REPEAT_PREV_CHAR(-20),

    /**
     * Operation that searches backwards in the command history.
     */
    SEARCH_PREV(-21),

    /**
     * Operation that repeats the character.
     */
    REPEAT_NEXT_CHAR(-24),

    /**
     * Operation that searches forward in the command history.
     */
    SEARCH_NEXT(-25),

    /**
     * Operation that moved to the previous whitespace.
     */
    PREV_SPACE_WORD(-27),

    /**
     * Operation that moved to the end of the current word.
     */
    TO_END_WORD(-29),

    /**
     * Operation that
     */
    REPEAT_SEARCH_PREV(-34),

    /**
     * Operation that
     */
    PASTE_PREV(-36),

    /**
     * Operation that
     */
    REPLACE_MODE(-37),

    /**
     * Operation that
     */
    SUBSTITUTE_LINE(-38),

    /**
     * Operation that
     */
    TO_PREV_CHAR(-39),

    /**
     * Operation that
     */
    NEXT_SPACE_WORD(-40),

    /**
     * Operation that
     */
    DELETE_PREV_CHAR(-41),

    /**
     * Operation that
     */
    ADD(-42),

    /**
     * Operation that
     */
    PREV_WORD(-43),

    /**
     * Operation that
     */
    CHANGE_META(-44),

    /**
     * Operation that
     */
    DELETE_META(-45),

    /**
     * Operation that
     */
    END_WORD(-46),

    /**
     * Operation that toggles insert/overtype
     */
    INSERT(-48),

    /**
     * Operation that
     */
    REPEAT_SEARCH_NEXT(-49),

    /**
     * Operation that
     */
    PASTE_NEXT(-50),

    /**
     * Operation that
     */
    REPLACE_CHAR(-51),

    /**
     * Operation that
     */
    SUBSTITUTE_CHAR(-52),

    /**
     * Operation that
     */
    TO_NEXT_CHAR(-53),

    /**
     * Operation that undoes the previous operation.
     */
    UNDO(-54),

    /**
     * Operation that moved to the next word.
     */
    NEXT_WORD(-55),

    /**
     * Operation that deletes the previous character.
     */
    DELETE_NEXT_CHAR(-56),

    /**
     * Operation that toggles between uppercase and lowercase.
     */
    CHANGE_CASE(-57),

    /**
     * Operation that performs completion operation on the current word.
     */
    COMPLETE(-58),

    /**
     * Operation that exits the command prompt.
     */
    EXIT(-59),

    /**
     * Operation that pastes the contents of the clipboard into the line
     */
    PASTE(-60),

    /**
     * Operation that moves the current History to the beginning.
     */
    START_OF_HISTORY(-61),

    /**
     * Operation that moves the current History to the end.
     */
    END_OF_HISTORY(-62),

    /**
     * Operation that clears whatever text is on the current line.
     */
    CLEAR_LINE(-63),

    /**
     * Cancel search
     */
    ABORT(-64),

    /**
     * Delete next word
     */
    DELETE_NEXT_WORD(-65),

    ;

    public final short code;

    Operation(final int code) {
        this.code = (short) code;
    }

    private static final Map<Short, Operation> codes;

    static {
        Map<Short, Operation> map = new HashMap<Short, Operation>();

        for (Operation op : Operation.values()) {
            map.put(op.code, op);
        }

        codes = map;
    }

    public static Operation valueOf(final int code) {
        return codes.get((short) code);
    }
}