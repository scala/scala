/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

/**
 * This class represents a position in a source file. Such a position
 * is defined by a source file (mandatory), a line number (optional)
 * and a column number (optional, may be specified only if the line
 * number is defined).
 *
 * Line (Column) numbers range from 0 to Integer.MAX_VALUE. A value of
 * 0 indicates that the line (column) is undefined, 1 represents the
 * first line (column). Negative values are prohibited.
 *
 * The class provides also functions to encode a line number and a
 * column number into one single integer. The encode line (column)
 * numbers range from 0 to LINE_MASK (COLUMN_MASK), where 0 indicates
 * that the line (column) is the undefined and 1 represents the first
 * line (column). Line (Column) numbers greater than LINE_MASK
 * (COLUMN_MASK) are replaced by LINE_MASK (COLUMN_MASK). Furthermore,
 * if the encoded line number is LINE_MASK, the column number is
 * always set to 0.
 *
 * The following properties hold:
 * - the undefined position is 0: encode(0,0) == 0
 * - encodings are non-negative : encode(line,column) >= 0
 * - position order is preserved:
 *   (line1 < line2) || (line1 == line2 && column1 < column2)
 * implies
 *   encode(line1,column1) <= encode(line2,column2)
 */
public class Position {

    //########################################################################
    // Public Constants

    /** Number of bits used to encode the line number */
    public static final int LINE_BITS   = 20;
    /** Number of bits used to encode the column number */
    public static final int COLUMN_BITS = 31 - LINE_BITS; // no negatives => 31

    /** Mask to decode the line number */
    public static final int LINE_MASK   = (1 << LINE_BITS) - 1;
    /** Mask to decode the column number */
    public static final int COLUMN_MASK = (1 << COLUMN_BITS) - 1;

    /** The undefined position */
    public static final int NOPOS       = 0;

    /** The first position in a source file */
    public static final int FIRSTPOS    = encode(1, 1);

    //########################################################################
    // Public Functions

    /** Encodes a position into a single integer. */
    public static int encode(int line, int column) {
        assert line >= 0 : line;
        assert line == 0 ? column == 0 : column >= 0 : line + "," + column;
        if (line >= LINE_MASK) { line = LINE_MASK; column = 0; }
        if (column > COLUMN_MASK) column = COLUMN_MASK;
        return (line << COLUMN_BITS) | column;
    }

    /** Returns the line number of the encoded position. */
    public static int line(int position) {
        return (position >> COLUMN_BITS) & LINE_MASK;
    }

    /** Returns the column number of the encoded position. */
    public static int column(int position) {
        return position & COLUMN_MASK;
    }

    /** Returns a string representation of the encoded position. */
    public static String toString(int position) {
        return line(position) + ":" + column(position);
    }

    //########################################################################
    // Private Fields

    /** The position's file */
    private final AbstractSourceFile file;

    /** The position's line number */
    private final int line;

    /** The position's column number */
    private final int column;

    //########################################################################
    // Public Constructors

    /** Initializes a new instance. */
    public Position(String source) {
        this(new SourceFile(source, new byte[0]));
    }

    /** Initializes a new instance. */
    public Position(AbstractSourceFile file) {
        this(file, 0, 0);
    }

    /** Initializes a new instance. */
    public Position(AbstractSourceFile file, int position) {
        this(file, line(position), column(position));
    }

    /** Initializes a new instance. */
    public Position(AbstractSourceFile file, int line, int column) {
        this.file = file;
        this.line = line;
        this.column = column;
        assert file != null;
        assert line >= 0 : line;
        assert line == 0 ? column == 0 : column >= 0 : line + "," + column;
    }

    //########################################################################
    // Public Methods

    /** Returns the file of this position. */
    public AbstractSourceFile file() {
        return file;
    }

    /** Returns the line number of this position. */
    public int line() {
        return line;
    }

    /** Returns the column number of this position. */
    public int column() {
        return column;
    }

    /** Returns an int encoding the line and column of this position. */
    public int encodedLineColumn() {
        return encode(line, column);
    }

    /** Returns a string representation of this position. */
    public String toString() {
        StringBuffer buffer = new StringBuffer(file.name());
        if (line > 0) buffer.append(":").append(line);
        if (line > 0 && column > 0) buffer.append(":").append(column);
        return buffer.toString();
    }

    /** Returns the hash code of this position. */
    public int hashCode() {
        return file.hashCode() ^ encodedLineColumn();
    }

    /** Returns true iff the given object represents the same position. */
    public boolean equals(Object object) {
        if (!(object instanceof Position)) return false;
        Position that = (Position)object;
        return file == that.file && line == that.line && column == that.column;
    }

    //########################################################################
}
