/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;


/** This class represents a single source file. */
public class SourceFile {

    //########################################################################
    // Public Constants

    /** Constants used for source parsing */
    public static final char LF = '\u000A';
    public static final char FF = '\u000C';
    public static final char CR = '\u000D';
    public static final char SU = '\u001A';

    //########################################################################
    // Private Fields

    /** The underlying file */
    private final AbstractFile file;

    /** The content of this source file */
    private final char[] content;

    /** The position of the last line returned by getLine */
    private int lineNumber = 0;
    private int lineStart  = 0;
    private int lineLength = 0;
    private int nextIndex  = 0;

    //########################################################################
    // Public Constructors

    /** Initializes this instance with given name and content. */
    public SourceFile(String sourcename, char[] content) {
        this(new CharArrayFile(sourcename, content), content);
    }

    /** Initializes this instance with given file and content. */
    public SourceFile(AbstractFile file, char[] content) {
        this.file = file;
        this.content = normalize(content);
    }

    //########################################################################
    // Public Methods

    /** Returns the underlying file. */
    public AbstractFile getFile() {
        return file;
    }

    /** Returns the content of this source file. */
    public char[] getContent() {
        return content;
    }

    /**
     * Returns an instance of Position representing the given line and
     * column of this source file.
     */
    public Position getPosition(int line, int column) {
        return new Position(this, line, column);
    }

    /** Returns the specified line. */
    public String getLine(int line) {
        int index = lineNumber <= line ? nextIndex : (lineNumber = 0);
        for (; index < content.length && lineNumber < line; lineNumber++) {
            lineStart = index;
            for (; index < content.length; index++) {
                if (content[index] == CR) break;
                if (content[index] == LF) break;
                if (content[index] == FF) break;
            }
            lineLength = index - lineStart;
            if (index < content.length)
                index++;
            if (index < content.length)
                if (content[index - 1] == CR && content[index] == LF) index++;
        }
        nextIndex = index;
        return new String(content, lineStart, lineLength);
    }

    /** Returns the path of the underlying file. */
    public String toString() {
        return file.getPath();
    }

    //########################################################################
    // Private Functions

    /** Ensures that the last char of the array is SU. */
    private static char[] normalize(char[] input) {
        if (input.length > 0 && input[input.length - 1] == SU)
                return input;
        char[] content = new char[input.length + 1];
        System.arraycopy(input, 0, content, 0, input.length);
        content[input.length] = SU;
        return content;
    }

    //########################################################################
}
