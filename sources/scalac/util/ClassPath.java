/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import java.io.*;
import java.util.*;

import scala.tools.util.AbstractFile;


/** This class represents a Java/Scala class path. */
public class ClassPath {

    //########################################################################
    // Public Constants

    /** The system-dependent filename-separator character */
    public static final String FILE_SEPARATOR = File.separator;

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
    // Private Functions

    /** Returns the default boot class path. */
    private static String getDefaultBootClassPath() {
        String java = System.getProperty("sun.boot.class.path");
        String scala = System.getProperty("scala.boot.class.path");
        if (java == null) return scala == null ? "" : scala;
        return scala == null ? java : java + PATH_SEPARATOR + scala;
    }

    /** the various class path roots
     */
    protected String[] root;

    /** print searches in the class path
     */
    public boolean printSearch;


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
        String path = "";
        path = appendPath(path, bootclasspath);
        path = appendExtDirs(path, extdirs);
        path = appendPath(path, classpath);
        path = appendPath(path, sourcepath);
        root = parse(path.substring(1));
    }

    /** append an additional path
     */
    protected static String appendPath(String path, String addpath) {
        return addpath == null ? path : path + PATH_SEPARATOR + addpath;
    }

    /** append files from the extension directories
     */
    protected String appendExtDirs(String path, String extdirs) {
        if (extdirs != null) {
            extdirs += PATH_SEPARATOR;
            int length = extdirs.length();
            int i = 0;
            while (i < length) {
                int k = extdirs.indexOf(PATH_SEPARATOR, i);
                String dirname = extdirs.substring(i, k);
                String[] ext;
                if ((dirname != null) &&
                    (dirname.length() > 0) &&
                    ((ext = new File(dirname).list()) != null)) {
                    if (!dirname.endsWith(FILE_SEPARATOR))
                        dirname += FILE_SEPARATOR;
                    for (int j = 0; j < ext.length; j++)
                        if (ext[j].endsWith(".jar") ||
                            ext[j].endsWith(".zip"))
                            path = appendPath(path, dirname + ext[j]);
                }
                i = k + 1;
            }
        }
        return path;
    }

    //########################################################################
    // Public Methods

    /** parse a class path specification and return an array
     *  of existing class file locations
     */
    public static String[] parse(String path) {
        path += PATH_SEPARATOR;
        Vector components = new Vector();
        int i = 0;
        while (i < path.length()) {
            int j = path.indexOf(PATH_SEPARATOR, i);
            String subpath = path.substring(i, j);
            if (new File(subpath).exists())
                components.add(subpath);
            i = j + 1;
        }
        return (String[])components.toArray(
            new String[components.size()]);
    }

    /** find file with given name in class path and return an abstract
     *  file representation
     */
    public AbstractFile openFile(String name) throws FileNotFoundException {
        if (printSearch)
            System.out.println("looking for " + name);
        for (int i = 0; i < root.length; i++) {
            if (printSearch)
                System.out.println("  in " + root[i]);
            AbstractFile f = AbstractFile.open(root[i], name);
            if (f != null)
                return f;
        }
        throw new FileNotFoundException("file '" + name +
                                        "' not found in classpath");
    }

    public String[] components() {
        return root;
    }

    /** return a textual representation of this class path
     */
    public String toString() {
        if (root.length == 0)
            return "";
        else if (root.length == 1)
            return root[0];
        String path = root[0];
        for (int i = 1; i < root.length; i++)
            path += PATH_SEPARATOR + root[i];
        return path;
    }

    //########################################################################
}
