/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;

/** This class implements an empty abstract regular file. */
public class VirtualFile extends AbstractFile {

    //########################################################################
    // Private Fields

    /** The file name */
    private final String name;

    /** The file path */
    private final String path;

    //########################################################################
    // Public Constructors

    /**
     * Initializes this instance with the specified name and an
     * identical path.
     */
    public VirtualFile(String name) {
        this(name, name);
    }

    /** Initializes this instance with the specified name and path. */
    public VirtualFile(String name, String path) {
        this.name = name;
        this.path = path;
        assert name != null && path != null: name + " - " + path;
    }

    //########################################################################
    // Public Methods

    /** Returns the name of this abstract file. */
    public String getName() {
        return name;
    }

    /** Returns the path of this abstract file. */
    public String getPath() {
        return path;
    }

    /** Returns null. */
    public final File getFile() {
        return null;
    }

    /** Is this abstract file a directory? */
    public boolean isDirectory() {
        return false;
    }

    /** Returns the time that this abstract file was last modified. */
    public long lastModified() {
        return Long.MIN_VALUE;
    }

    /** Reads the content of this abstract file into a byte array. */
    public byte[] read() throws IOException {
        assert !isDirectory(): "cannot read directory '" + this + "'";
        return new byte[0];
    }

    /** Returns all abstract subfiles of this abstract directory. */
    public Iterator/*<AbstractFile>*/ list() {
        assert isDirectory(): "not a directory '" + this + "'";
        return EmptyIterator.object;
    }

    /**
     * Returns the abstract file in this abstract directory with the
     * specified name. If there is no such file, returns null. The
     * argument "directory" tells whether to look for a directory or
     * or a regular file.
     */
    public AbstractFile lookupName(String name, boolean directory) {
        assert isDirectory(): "not a directory '" + this + "'";
        return null;
    }

    //########################################################################
}
