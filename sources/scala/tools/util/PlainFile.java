/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;

/** This class implements an abstract file backed by a File. */
public class PlainFile extends AbstractFile {

    //########################################################################
    // Public Factories

    /** Returns "fromFile(new File(path))". */
    public static AbstractFile fromPath(String path) {
        return fromFile(new File(path));
    }

    /**
     * If the specified File exists, returns an abstract file backed
     * by it. Otherwise, returns null.
     */
    public static AbstractFile fromFile(File file) {
        return file.exists() ? new PlainFile(file) : null;
    }

    //########################################################################
    // Private Fields

    /** The underlying File */
    private final File file;

    //########################################################################
    // Protected Constructors

    /** Initializes this instance with the specified File. */
    protected PlainFile(File file) {
        this.file = file;
        assert file != null;
    }

    //########################################################################
    // Public Methods

    /** Returns the name of this abstract file. */
    public String getName() {
        return file.getName();
    }

    /** Returns the path of this abstract file. */
    public String getPath() {
        return file.getPath();
    }

    /** Returns the underlying File if any and null otherwise. */
    public File getFile() {
        return file;
    }


    public int hashCode() {
	try {
	    return file.getCanonicalPath().hashCode();
	} catch (IOException ex) {
	    return 0;
	}
    }

    public boolean equals(Object that) {
	try {
	    return that instanceof PlainFile &&
		file.getCanonicalPath().equals(((PlainFile) that).file.getCanonicalPath());
	} catch (IOException ex) {
	    return that instanceof PlainFile &&
		file.getAbsolutePath().equals(((PlainFile) that).file.getAbsolutePath());
	}
    }

    /** Is this abstract file a directory? */
    public boolean isDirectory() {
        return file.isDirectory();
    }

    /** Returns the time that this abstract file was last modified. */
    public long lastModified() {
	return file.lastModified();
    }

    /** Reads the content of this abstract file into a byte array. */
    public byte[] read() throws IOException {
        assert !isDirectory(): "cannot read directory '" + this + "'";
        FileInputStream in = new FileInputStream(file);
        int rest = (int)file.length();
        byte[] buf = new byte[rest];
        do {
            int res = in.read(buf, buf.length - rest, rest);
            if (res == -1)
                throw new IOException("read error");
            rest -= res;
        } while (rest > 0);
        in.close();
        return buf;
    }

    /** Returns all abstract subfiles of this abstract directory. */
    public Iterator/*<AbstractFile>*/ list() {
        assert isDirectory(): "not a directory '" + this + "'";
        final String[] names = file.list();
        if (names == null || names.length == 0) return EmptyIterator.object;
        class ListIterator implements Iterator {
            private int i;
            public boolean hasNext() {
                return i < names.length;
            }
            public Object next() {
                if (i == names.length) throw new NoSuchElementException();
                return new PlainFile(new File(file, names[i++]));
            }
            public void remove() {
                throw new UnsupportedOperationException();
            }
        }
        return new ListIterator();
    }

    /**
     * Returns the abstract file in this abstract directory with the
     * specified name. If there is no such file, returns null. The
     * argument "directory" tells whether to look for a directory or
     * or a regular file.
     */
    public AbstractFile lookupName(String name, boolean directory) {
        assert isDirectory(): "not a directory '" + this + "'";
        File child = new File(file, name);
        if (directory ? !child.isDirectory() : !child.isFile()) return null;
        return new PlainFile(child);
    }

    //########################################################################
}
