/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * This class implements an abstract directory backed by a zip
 * archive.
 */
public final class ZipArchive extends PlainFile {

    //########################################################################
    // Public Factories

    /** Returns "fromFile(new File(path))". */
    public static AbstractFile fromPath(String path) {
        return fromFile(new File(path));
    }

    /**
     * If the specified File exists and is a readable zip archive,
     * returns an abstract file backed by it. Otherwise, returns null.
     */
    public static AbstractFile fromFile(File file) {
        try {
            return new ZipArchive(file, new ZipFile(file));
        } catch (IOException exception) {
            return null;
        }
    }

    /**
     * Returns an abstract directory backed by the specified archive.
     */
    public static AbstractFile fromArchive(ZipFile archive) {
        return new ZipArchive(new File(archive.getName()), archive);
    }

    //########################################################################
    // Private Fields

    /** The zip archive */
    private final ZipFile archive;

    /** The root directory or null if not yet initialized */
    private DirEntry root;

    //########################################################################
    // Protected Constructors

    /**
     * Initializes this instance with the specified File and ZipFile.
     */
    protected ZipArchive(File file, ZipFile archive) {
        super(file);
        this.archive = archive;
        assert archive != null;
    }

    //########################################################################
    // Public Methods

    /** Returns true. */
    public boolean isDirectory() {
        return true;
    }

    /** Returns all abstract subfiles of this abstract directory. */
    public Iterator/*<AbstractFile>*/ list() {
        if (root == null) load();
        return root.list();
    }

    /**
     * Returns the abstract file in this abstract directory with the
     * specified name. If there is no such file, returns null. The
     * argument "directory" tells whether to look for a directory or
     * or a regular file.
     */
    public AbstractFile lookupName(String name, boolean directory) {
        if (root == null) load();
        return root.lookupName(name, directory);
    }

    //########################################################################
    // Private Methods

    /** Loads the archive and creates the root directory. */
    private void load() {
        this.root = new DirEntry("<root>", "/");
        // A path to DirEntry map
        HashMap/*<String,DirEntry>*/ dirs = new HashMap();
        dirs.put("/", root);
        for (Enumeration e = archive.entries(); e.hasMoreElements(); ) {
            ZipEntry entry = (ZipEntry)e.nextElement();
            String path = entry.getName();
            assert entry.isDirectory() == path.endsWith("/"): this +"-"+ path;
            if (entry.isDirectory()) {
                DirEntry dir = getDir(dirs, path);
                assert dir.entry == null: this + " - " + path;
                dir.entry = entry;
            } else {
                int index = path.lastIndexOf('/');
                String name = index < 0 ? path : path.substring(index + 1);
                String home = index < 0 ? "/"  : path.substring(0, index + 1);
                DirEntry parent = getDir(dirs, home);
                assert !parent.entries.containsKey(path): this + " - " + path;
                parent.entries.put(path, new FileEntry(name, path, entry));
            }
        }
    }

    /**
     * Lookups the specified table for a DirEntry with the specified
     * path. If successful, returns the found DirEntry. Otherwise
     * creates a new DirEntry, enters it into the table and in the
     * table of its parent ZipDir and returns it.
     */
    private DirEntry getDir(HashMap/*<String,DirEntry>*/ dirs, String path) {
        DirEntry dir = (DirEntry)dirs.get(path);
        if (dir == null) {
            int index = path.lastIndexOf('/', path.length() - 2);
            String name = index < 0 ? path : path.substring(index + 1);
            String home = index < 0 ? "/"  : path.substring(0, index + 1);
            DirEntry parent = getDir(dirs, home);
            dir = new DirEntry(name.substring(0, name.length() - 1), path);
            parent.entries.put(name, dir);
            dirs.put(path, dir);
        }
        return dir;
    }

    //########################################################################
    // Private Class - Entry

    /** Superclass of archive entries */
    private abstract class Entry extends VirtualFile {

        public Entry(String name, String path) {
            super(name, path);
        }

        public final String getPath() {
            return ZipArchive.this + "(" + super.getPath() + ")";
        }

    }

    //########################################################################
    // Private Class - DirEntry

    /** A directory archive entry */
    private final class DirEntry extends Entry {

        public final HashMap/*<String,Entry>*/ entries;
        public ZipEntry entry;

        public DirEntry(String name, String path) {
            super(name, path);
            this.entries = new HashMap();
        }

        public boolean isDirectory() {
            return true;
        }

	public long lastModified() {
	    return entry != null ? entry.getTime() : super.lastModified();
	}

        public Iterator/*<AbstractFile>*/ list() {
            return entries.values().iterator();
        }

        public AbstractFile lookupName(String name, boolean directory) {
            return (AbstractFile)entries.get(directory ? name + "/" : name);
        }

    }

    //########################################################################
    // Private Class - FileEntry

    /** A regular file archive entry */
    private final class FileEntry extends Entry {

        public final ZipEntry entry;

        public FileEntry(String name, String path, ZipEntry entry) {
            super(name, path);
            this.entry = entry;
        }

	public long lastModified() {
	    return entry.getTime();
	}

        public byte[] read() throws IOException {
            InputStream in = archive.getInputStream(entry);
            int rest = (int)entry.getSize();
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

    }

    //########################################################################
}
