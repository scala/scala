/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.NoSuchElementException;

/**
 * This class implements an abstract directory backed by a list of
 * abstract directories. The content of the directories are merged
 * together. If a subfile occurs in several directories, then the
 * first occurrence hides the next ones. If a subdirectory occurs in
 * several directories then the content of the different occurrences
 * are merged together in the same way.
 */
public class DirectoryPath extends VirtualDirectory {

    //########################################################################
    // Public Factories

    /**
     * Returns an abstract directory with the specified name and
     * backed by the specified array of abstract directories.
     */
    public static AbstractFile fromArray(String name, AbstractFile[] dirs) {
        if (dirs.length == 0) return new VirtualDirectory(name, "");
        if (dirs.length == 1 && dirs[0].getName().equals(name)) return dirs[1];
        return new DirectoryPath(name, dirs);
    }

    //########################################################################
    // Private Fields

    /** The directories composing this directory path */
    private final AbstractFile[] dirs;

    //########################################################################
    // Protected Constructor

    /** Initializes this instance with given name and directories. */
    protected DirectoryPath(String name, AbstractFile[] dirs) {
        super(name);
        this.dirs = dirs;
        for (int i = 0; i < dirs.length; i++)
            assert dirs[i].isDirectory(): dirs[i];
    }

    //########################################################################
    // Public Methods

    /** Returns the path of this abstract file. */
    public String getPath() {
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < dirs.length; i++) {
            if (i > 0) buffer.append(File.pathSeparator);
            buffer.append(dirs[i]);
        }
        return buffer.toString();
    }

    /** Returns all abstract subfiles of this abstract directory. */
     public Iterator/*<AbstractFile>*/ list() {
         return new ListIterator();
     }

    /**
     * Returns the abstract file in this abstract directory with the
     * specified name. If there is no such file, returns null. The
     * argument "directory" tells whether to look for a directory or
     * or a regular file.
     */
    public AbstractFile lookupName(String name, boolean directory) {
        if (directory) {
            AbstractFile first = null;
            AbstractFile[] subdirs = null;
            int count = 0;
            for (int i = 0; i < dirs.length; i++) {
                AbstractFile subdir = dirs[i].lookupName(name, directory);
                if (subdir == null) continue;
                if (count == 0) {
                    first = subdir;
                    count++;
                } else {
                    if (count == 1) {
                        subdirs = new AbstractFile[dirs.length];
                        subdirs[0] = first;
                    }
                    subdirs[count++] = subdir;
                }
            }
            if (count == 0) return null;
            if (count == 1) return first;
            if (count != subdirs.length) {
                AbstractFile[] array = new AbstractFile[count];
                for (int i = 0; i < array.length; i++) array[i] = subdirs[i];
                subdirs = array;
            }
            return new DirectoryPath(name, subdirs);
        } else {
            for (int i = 0; i < dirs.length; i++) {
                AbstractFile file = dirs[i].lookupName(name, directory);
                if (file != null) return file;
            }
            return null;
        }
    }

    //########################################################################
    // Private Class - ListIterator

    /** An iterator over the files contained in this directory. */
    private class ListIterator implements Iterator {

        /** The type of the values in the subdirectory table */
        // type SubDirs = AbstractFile | ArrayList<AbstractFile>

        /** A table to collect subdirectories */
        private final HashMap/*<String,SubDirs>*/ subdirs = new HashMap();

        /** A table to track already returned regular subfiles */
        private final HashSet/*<String>*/ subfiles = new HashSet();

        /** The current iterator */
        private Iterator iterator;

        /** The index of the current directory */
        private int index;

        /** The next iteration value or null if no more */
        private Object next;

        /** Initializes this instance. */
        public ListIterator() {
            this.next = getNextValue();
        }

        /** Does this iteration have more elements? */
        public boolean hasNext() {
            return next != null;
        }

        /** Returns the next element in this iteration. */
        public Object next() {
            if (next == null) throw new NoSuchElementException();
            Object value = next;
            next = getNextValue();
            return value;
        }

        /** Throws UnsupportedOperationException. */
        public void remove() {
            throw new UnsupportedOperationException();
        }

        /** Returns the next iteration value or null if no more. */
        private AbstractFile getNextValue() {
            for (; index < dirs.length; iterator = null, index++) {
                // iterate over the files of directory "index"
                if (iterator == null) iterator = dirs[index].list();
                while (iterator.hasNext()) {
                    AbstractFile subfile = (AbstractFile)iterator.next();
                    String name = subfile.getName();
                    if (subfile.isDirectory()) {
                        addSubDir(name, subfile);
                    } else if (!subfiles.contains(name)) {
                        subfiles.add(name);
                        return subfile;
                    }
                }
            }
            // iterate over the collected subdirectories
            if (iterator == null) iterator = subdirs.entrySet().iterator();
            if (iterator.hasNext()) return getSubDir((Entry)iterator.next());
            return null;
        }

        /** Adds given subdirectory to the subdirectory table. */
        private void addSubDir(String name, AbstractFile subdir) {
            Object value = subdirs.get(name);
            if (value == null) {
                subdirs.put(name, subdir);
            } else {
                ArrayList list;
                if (value instanceof ArrayList) {
                    list = (ArrayList)value;
                } else {
                    list = new ArrayList();
                    subdirs.put(name, list);
                    list.add(value);
                }
                list.add(subdir);
            }
        }

        /** Turns given entry into an abstract directory. */
        private AbstractFile getSubDir(Entry/*<String,SubDirs>*/ entry) {
            Object value = entry.getValue();
            if (value instanceof ArrayList) {
                ArrayList list = (ArrayList)value;
                AbstractFile[] array = new AbstractFile[list.size()];
                list.toArray(array);
                return new DirectoryPath((String)entry.getKey(), array);
            } else {
                return (AbstractFile)value;
            }
        }

    }

    //########################################################################
}
