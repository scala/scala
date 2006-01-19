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
import java.util.jar.JarFile;
import java.util.zip.ZipFile;

/**
 * This class implements an abstract representation of files and
 * directories. These files and directories may have some real counter
 * part within the file system but that is not necessarily true. For
 * example, there exist abstract files that represent files within a
 * zip archive or files that exist only in memory.
 *
 * Every abstract file has a path (i.e. a full name) and a name
 * (i.e. a short name) and may be backed by some real File. There are
 * two different kinds of abstract files: regular files and
 * directories. Regular files may be read and have a last modification
 * time. Directories may list their content and look for subfiles with
 * a specified name or path and of a specified kind.
 */
public abstract class AbstractFile {

    //########################################################################
    // Public Factories

    /** Returns "getFile(new File(path))". */
    public static AbstractFile getFile(String path) {
        return getFile(new File(path));
    }

    /**
     * If the specified File exists and is a regular file, returns an
     * abstract regular file backed by it. Otherwise, returns null.
     */
    public static AbstractFile getFile(File file) {
        return file.isFile() && file.exists() ? new PlainFile(file) : null;
    }


    /** Returns "getDirectory(new File(path))". */
    public static AbstractFile getDirectory(String path) {
        return getDirectory(new File(path));
    }

    /**
     * if the specified File exists and is either a directory or a
     * readable zip or jar archive, returns an abstract directory
     * backed by it. Otherwise, returns null.
     */
    public static AbstractFile getDirectory(File file) {
        if (file.isDirectory() && file.exists()) return new PlainFile(file);
        if (file.isFile() && file.exists()) {
            String path = file.getPath();
            if (path.endsWith(".jar") || path.endsWith(".zip"))
                return ZipArchive.fromFile(file);
        }
        return null;
    }

    //########################################################################
    // Public Methods

    /** Returns the name of this abstract file. */
    public abstract String getName();

    /** Returns the path of this abstract file. */
    public abstract String getPath();

    /** Returns the underlying File if any and null otherwise. */
    public abstract File getFile();

    /** Is this abstract file a directory? */
    public abstract boolean isDirectory();

    /** Returns the time that this abstract file was last modified. */
    public abstract long lastModified();

    /** Reads the content of this abstract file into a byte array. */
    public abstract byte[] read() throws IOException;

    /** Returns all abstract subfiles of this abstract directory. */
    public abstract Iterator/*<AbstractFile>*/ list();

    /**
     * Returns the abstract file in this abstract directory with the
     * specified name. If there is no such file, returns null. The
     * argument "directory" tells whether to look for a directory or
     * or a regular file.
     */
    public abstract AbstractFile lookupName(String name, boolean directory);

    /**
     * Returns the abstract file in this abstract directory with the
     * specified path relative to it, If there is no such file,
     * returns null. The argument "directory" tells whether to look
     * for a directory or or a regular file.
     */
    public final AbstractFile lookupPath(String path, boolean directory) {
        final int length = path.length();
        final char separator = File.separatorChar;
        assert 0 < length && path.lastIndexOf(separator) < length - 1: path;
        AbstractFile file = this;
        for (int start = 0, index; true; start = index + 1) {
            index = path.indexOf(separator, start);
            assert index < 0 || start < index: path+" - "+start+" - "+index;
            String name = path.substring(start, index < 0 ? length : index);
            file = file.lookupName(name, index < 0 ? directory : true);
            if (file == null || index < 0) return file;
        }
    }

    /** Returns the path of this abstract file. */
    public final String toString() {
        return getPath();
    }

    //########################################################################
}
