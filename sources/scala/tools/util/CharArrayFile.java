/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

/**
 * This class implements an abstract regular file backed by a
 * character array.
 */
public class CharArrayFile extends VirtualFile {

    //########################################################################
    // Private Fields

    /** The character array */
    private final char[] chars;

    //########################################################################
    // Public Constructors

    /**
     * Initializes this instance with the specified name, an identical
     * path and the specified character array.
     */
    public CharArrayFile(String name, char[] chars) {
        this(name, name, chars);
    }

    /**
     * Initializes this instance with the specified name, path and
     * character array.
     */
    public CharArrayFile(String name, String path, char[] chars) {
        super(name, path);
        this.chars = chars;
    }

    //########################################################################
    // Public Methods

    /** Reads the content of this abstract file into a byte array. */
    public byte[] read() {
        assert false: "!!! not yet implemented";
        return new String(chars).getBytes(); // !!!
    }

    //########################################################################
}
