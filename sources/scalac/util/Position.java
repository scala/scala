/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.util;


public final class Position {

/** source file positions are integers in the format:
 *     line-number << LINESHIFT + column-number
 *  NOPOS represents an undefined position.
 */
    public static final int LINESHIFT    = 10;
    public static final int FILESHIFT    = 26;
    public static final int COLUMNMASK   = 1023;
    public static final int LINEMASK     = 0xffff;

/** predefined positions
 */
    public static final int NOPOS        = 0;

/** first position in a source file
 */
    public static final int FIRSTPOS     = (1 << LINESHIFT) + 1;

/** encode a line and column number into a single int
 */
    public static int encode(int line, int col, int file) {
        return (file << FILESHIFT) | (line << LINESHIFT) | col;
    }

/** get the file id of an encoded position
 */
    public static int file(int pos) {
        return pos >>> FILESHIFT;
    }

/** get the line number out of an encoded position
 */
    public static int line(int pos) {
        return (pos >>> LINESHIFT) & LINEMASK;
    }

/** return the column number of an encoded position
 */
    public static int column(int pos) {
        return pos & COLUMNMASK;
    }
}
