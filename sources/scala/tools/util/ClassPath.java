/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

/** This class represents a Java/Scala class path. */
public class ClassPath {

    //########################################################################
    // Public Constants

    /** The system-dependent path-separator character */
    public static final String PATH_SEPARATOR =
        System.getProperty("path.separator", ":");

    /** The default class path */
    public static final String CLASSPATH =
        System.getProperty("scala.class.path", ".");

    /** The default source path */
    public static final String SOURCEPATH = "";

    /** The default boot class path */
    public static final String BOOTCLASSPATH =
        getDefaultBootClassPath();

    /** The default extension directory path */
    public static final String EXTDIRS =
        System.getProperty("java.ext.dirs", "");

    //########################################################################
    // Public Functions

    /**
     * Adds all zip and jar archives found in the specified extension
     * directory path to the specified file set. See also remark about
     * file order in method "addFilesFromPath".
     */
    public static void addArchivesInExtDirPath(Set/*<File>*/files,String path){
        Set extdirs = new LinkedHashSet();
        addFilesInPath(extdirs, path);
        for (Iterator i = extdirs.iterator(); i.hasNext(); )
            addArchivesInExtDir(files, (File)i.next());
    }

    /**
     * Adds all zip and jar archives found in the specified extension
     * directory to the specified file set. See also remark about file
     * order in method "addFilesFromPath".
     */
    public static void addArchivesInExtDir(Set/*<File>*/ files, File extdir) {
        String[] names = extdir.list();
        if (names == null) return;
        for (int i = 0; i < names.length; i++) {
            if (names[i].endsWith(".jar") || names[i].endsWith(".zip")) {
                File archive = new File(extdir, names[i]);
                if (archive.isFile()) files.add(archive);
            }
        }
    }

    /**
     * Parses the specified path and adds all files that exist to the
     * specified file set. If order needs to be preserved, one should
     * pass in an order preserving implementation of Set.
     */
    public static void addFilesInPath(Set/*<File>*/ files, String path) {
        path += PATH_SEPARATOR;
        for (int i = 0; i < path.length(); ) {
            int j = path.indexOf(PATH_SEPARATOR, i);
            File file = new File(path.substring(i, j));
            if (file.exists()) files.add(file);
            i = j + 1;
        }
    }

    //########################################################################
    // Private Functions

    /** Returns the default boot class path. */
    private static String getDefaultBootClassPath() {
        String java = System.getProperty("sun.boot.class.path");
        String scala = System.getProperty("scala.boot.class.path");
        if (java == null) return scala == null ? "" : scala;
        return scala == null ? java : java + PATH_SEPARATOR + scala;
    }

    //########################################################################
    // Private Fields

    /** The abstract directory represented by this class path */
    private final AbstractFile root;

    //########################################################################
    // Public Constructors

    /** Initializes this instance with default paths. */
    public ClassPath() {
        this(CLASSPATH);
    }

    /**
     * Initializes this instance with the specified class path and
     * default source, boot class and extension directory paths.
     */
    public ClassPath(String classpath) {
        this(classpath, SOURCEPATH, BOOTCLASSPATH, EXTDIRS);
    }

    /** Initializes this instance with the specified paths. */
    public ClassPath(String classpath, String sourcepath, String bootclasspath,
        String extdirs)
    {
        // replace first empty path in bootclasspath by BOOTCLASSPATH
        if (!bootclasspath.equals(BOOTCLASSPATH)) {
            String path = PATH_SEPARATOR + bootclasspath + PATH_SEPARATOR;
            int index = path.indexOf(PATH_SEPARATOR + PATH_SEPARATOR);
            if (index >= 0)
                bootclasspath =
                    path.substring(1, index + 1) + BOOTCLASSPATH +
                    path.substring(index + 1, path.length() - 1);
        }
        Set files = new LinkedHashSet();
        addFilesInPath(files, bootclasspath);
        addArchivesInExtDirPath(files, extdirs);
        addFilesInPath(files, classpath);
        addFilesInPath(files, sourcepath);
        ArrayList dirs = new ArrayList(files.size());
        for (Iterator i = files.iterator(); i.hasNext(); ) {
            AbstractFile dir = AbstractFile.getDirectory((File)i.next());
            if (dir != null) dirs.add(dir);
        }
        Object[] array = dirs.toArray(new AbstractFile[dirs.size()]);
        this.root = DirectoryPath.fromArray("<root>", (AbstractFile[])array);
    }

    //########################################################################
    // Public Methods

    /** Returns the root of this class path. */
    public AbstractFile getRoot() {
        return root;
    }

    /** Returns a string representation of this class path. */
    public String toString() {
        return root.toString();
    }

    //########################################################################
}
