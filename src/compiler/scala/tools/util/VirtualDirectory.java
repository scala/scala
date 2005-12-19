/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

/** This class implements an empty abstract directory. */
public class VirtualDirectory extends VirtualFile {

    //########################################################################
    // Public Constructors

    /**
     * Initializes this instance with the specified name and an
     * identical path.
     */
    public VirtualDirectory(String name) {
        this(name, name);
    }

    /** Initializes this instance with the specified name and path. */
    public VirtualDirectory(String name, String path) {
        super(name, path);
    }

    //########################################################################
    // Public Methods

    /** Is this abstract file a directory? */
    public boolean isDirectory() {
        return true;
    }

    //########################################################################
}
