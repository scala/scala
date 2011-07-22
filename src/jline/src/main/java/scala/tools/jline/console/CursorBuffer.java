/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */

package scala.tools.jline.console;

/**
 * A holder for a {@link StringBuilder} that also contains the current cursor position.
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 * @since 2.0
 */
public class CursorBuffer
{
    private boolean overTyping = false;

    public int cursor = 0;

    public final StringBuilder buffer = new StringBuilder();

    public boolean isOverTyping() {
        return overTyping;
    }

    public void setOverTyping(final boolean b) {
        overTyping = b;
    }

    public int length() {
        return buffer.length();
    }

    /**
     * Gets the character to the left of the cursor.
     */
    public char charLeftOfCursor() {
        if (cursor <= 0) {
            return 0;
        }

        return buffer.charAt(cursor - 1);
    }

    /**
     * Gets the character at the cursor.
     */
    public char charAtCursor() {
        if (cursor < 0 || cursor >= buffer.length()) {
            return 0;
        }
        return buffer.charAt(cursor);
    }

    /**
     * Write the specific character into the buffer, setting the cursor position
     * ahead one. The text may overwrite or insert based on the current setting
     * of {@link #isOverTyping}.
     *
     * @param c the character to insert
     */
    public void write(final char c) {
        buffer.insert(cursor++, c);
        if (isOverTyping() && cursor < buffer.length()) {
            buffer.deleteCharAt(cursor);
        }
    }

    /**
     * Insert the specified chars into the buffer, setting the cursor to the end of the insertion point.
     */
    public void write(final CharSequence str) {
        assert str != null;

        if (buffer.length() == 0) {
            buffer.append(str);
        }
        else {
            buffer.insert(cursor, str);
        }

        cursor += str.length();

        if (isOverTyping() && cursor < buffer.length()) {
            buffer.delete(cursor, (cursor + str.length()));
        }
    }

    public boolean clear() {
        if (buffer.length() == 0) {
            return false;
        }

        buffer.delete(0, buffer.length());
        cursor = 0;
        return true;
    }

    @Override
    public String toString() {
        return buffer.toString();
    }
}
