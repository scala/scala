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
public class ByteArrayFile extends VirtualFile {

    //########################################################################
    // Private Fields

    /** The character array */
    private final byte[] bytes;

    //########################################################################
    // Public Constructors

    /**
     * Initializes this instance with the specified name, an identical
     * path and the specified character array.
     */
    public ByteArrayFile(String name, byte[] bytes) {
        this(name, name, bytes);
    }

    /**
     * Initializes this instance with the specified name, path and
     * character array.
     */
    public ByteArrayFile(String name, String path, byte[] bytes) {
        super(name, path);
        this.bytes = bytes;
    }

    //########################################################################
    // Public Methods

    /** Reads the content of this abstract file into a byte array. */
    public byte[] read() {
	byte[] newBytes = new byte[bytes.length];
	System.arraycopy(bytes, 0, newBytes, 0, bytes.length);
	return newBytes;
    }

    //########################################################################
}
