/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

public class Position {

    //########################################################################
    // Public Constants

    /** Number of bits used to encode the file */
    public static final int FILE_BITS   = 6;
    /** Number of bits used to encode the line number */
    public static final int LINE_BITS   = 16;
    /** Number of bits used to encode the column number */
    public static final int COLUMN_BITS = 32 - LINE_BITS - FILE_BITS;

    /** Mask to decode the file */
    public static final int FILE_MASK   = (1 << FILE_BITS) - 1;
    /** Mask to decode the line number */
    public static final int LINE_MASK   = (1 << LINE_BITS) - 1;
    /** Mask to decode the column number */
    public static final int COLUMN_MASK = (1 << COLUMN_BITS) - 1;

    /** The undefined position */
    public static final int NOPOS       = 0;

    /** The first position in a source file */
    public static final int FIRSTPOS    = encode(null, 1, 1);

    //########################################################################
    // Public Functions

    /** Encodes a position into a single int. */
    public static int encode(SourceFile file, int line, int column) {
        int fileId = file == null ? 0 : file.id() & FILE_MASK;
        if (fileId < 0 || FILE_MASK < fileId) fileId = 0;
        if (line < 0  || LINE_MASK < line) line = 0;
        if (column < 0 || COLUMN_MASK < column) column = 0;
        return (((fileId << LINE_BITS) | line) << COLUMN_BITS) | column;
    }

    /** Returns the file of the encoded position. */
    public static SourceFile file(int position) {
        int fileId = (position >> (COLUMN_BITS + LINE_BITS)) & FILE_MASK;
        return SourceFile.fromId(fileId);
    }

    /** Returns the line number of the encoded position. */
    public static int line(int position) {
        return (position >> COLUMN_BITS) & LINE_MASK;
    }

    /** Returns the column number of the encoded position. */
    public static int column(int position) {
        return position & COLUMN_MASK;
    }

    //########################################################################
    // Private Fields

    /** The position's file */
    private final SourceFile file;

    /** The position's line number */
    private final int line;

    /** The position's column number */
    private final int column;

    //########################################################################
    // Public Constructors

    /** Initializes a new instance. */
    public Position(int position) {
        this(file(position), line(position), column(position));
    }

    /** Initializes a new instance. */
    public Position(String source) {
        this(new SourceFile(source, new byte[0]));
    }

    /** Initializes a new instance. */
    public Position(SourceFile file) {
        this(file, 0, 0);
    }

    /** Initializes a new instance. */
    public Position(SourceFile file, int line, int column) {
        this.file = file;
        this.line = line;
        this.column = column;
    }

    //########################################################################
    // Public Methods

    /** Returns an int encoding this position. */
    public int encode() {
        return encode(file, line, column);
    }

    /** Returns the file of this position. */
    public SourceFile file() {
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

    /** Returns a string representation of this position. */
    public String toString() {
        return file.name() + ":" + line + ":" + column;
    }

    /** Returns the hash code of this position. */
    public int hashCode() {
        return encode();
    }

    /** Returns true iff the given object represents the same position. */
    public boolean equals(Object object) {
        if (!(object instanceof Position)) return false;
        Position that = (Position)object;
        return file == that.file && line == that.line && column == that.column;
    }

    //########################################################################
}
