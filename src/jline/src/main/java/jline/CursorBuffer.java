/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;

/**
 * A CursorBuffer is a holder for a {@link StringBuffer} that also contains the
 * current cursor position.
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class CursorBuffer {
    public int cursor = 0;

    StringBuffer buffer = new StringBuffer();

    private boolean overtyping = false;

    public int length() {
        return buffer.length();
    }

    public char current() {
        if (cursor <= 0) {
            return 0;
        }

        return buffer.charAt(cursor - 1);
    }

    public boolean clearBuffer() {
        if (buffer.length() == 0) {
            return false;
        }

        buffer.delete(0, buffer.length());
        cursor = 0;
        return true;
    }

    /**
     * Write the specific character into the buffer, setting the cursor position
     * ahead one. The text may overwrite or insert based on the current setting
     * of isOvertyping().
     *
     * @param c
     *            the character to insert
     */
    public void write(final char c) {
        buffer.insert(cursor++, c);
        if (isOvertyping() && cursor < buffer.length()) {
            buffer.deleteCharAt(cursor);
        }
    }

    /**
     * Insert the specified {@link String} into the buffer, setting the cursor
     * to the end of the insertion point.
     *
     * @param str
     *            the String to insert. Must not be null.
     */
    public void write(final String str) {
        if (buffer.length() == 0) {
            buffer.append(str);
        } else {
            buffer.insert(cursor, str);
        }

        cursor += str.length();

        if (isOvertyping() && cursor < buffer.length()) {
            buffer.delete(cursor, (cursor + str.length()));
        }
    }

    public String toString() {
        return buffer.toString();
    }

    public boolean isOvertyping() {
        return overtyping;
    }

    public void setOvertyping(boolean b) {
        overtyping = b;
    }

	public StringBuffer getBuffer() {
		return buffer;
	}

	public void setBuffer(StringBuffer buffer) {
		buffer.setLength(0);
		buffer.append(this.buffer.toString());

		this.buffer = buffer;
	}


}
