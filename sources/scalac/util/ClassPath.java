/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.util;

import java.io.*;
import java.util.*;


public class ClassPath {

    /** the character separating files
     */
    protected static String FILE_SEP = File.separator;

    /** the separator in class path specifications
     */
    protected static String PATH_SEP =
        System.getProperty("path.separator");

    /** the default class path
     */
    public static String CLASS_PATH =
        System.getProperty("scala.class.path",
            System.getProperty("java.class.path"));

    /** the default source path
     */
    public static String SOURCE_PATH = null;

    /** the default boot class path
     */
    public static String BOOT_PATH =
        appendPath(appendPath("", System.getProperty("sun.boot.class.path")),
            System.getProperty("scala.boot.class.path")).substring(1);

    /** the default extension path
     */
    public static String EXTENSION_PATH =
        System.getProperty("java.ext.dirs");

    /** the various class path roots
     */
    protected String[] root;

    /** print searches in the class path
     */
    public boolean printSearch;


    /** classpath constructor
     */
    public ClassPath() {
        this(CLASS_PATH);
    }

    public ClassPath(String classpath) {
        this(classpath, SOURCE_PATH, BOOT_PATH, EXTENSION_PATH);
    }

    public ClassPath(String classpath, String sourcepath,
        String bootclasspath, String extdirs)
    {
        // replace first empty path in bootclasspath by BOOT_PATH
        if (!bootclasspath.equals(BOOT_PATH)) {
            String path = PATH_SEP + bootclasspath + PATH_SEP;
            int index = path.indexOf(PATH_SEP + PATH_SEP);
            if (index >= 0)
                bootclasspath =
                    path.substring(1, index + 1) + BOOT_PATH +
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
        return addpath == null ? path : path + PATH_SEP + addpath;
    }

    /** append files from the extension directories
     */
    protected String appendExtDirs(String path, String extdirs) {
        if (extdirs != null) {
            extdirs += PATH_SEP;
            int length = extdirs.length();
            int i = 0;
            while (i < length) {
                int k = extdirs.indexOf(PATH_SEP, i);
                String dirname = extdirs.substring(i, k);
                String[] ext;
                if ((dirname != null) &&
                    (dirname.length() > 0) &&
                    ((ext = new File(dirname).list()) != null)) {
                    if (!dirname.endsWith(FILE_SEP))
                        dirname += FILE_SEP;
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

    /** parse a class path specification and return an array
     *  of existing class file locations
     */
    public static String[] parse(String path) {
        path += PATH_SEP;
        Vector components = new Vector();
        int i = 0;
        while (i < path.length()) {
            int j = path.indexOf(PATH_SEP, i);
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

    public java.io.File openJavaFile(String name) throws FileNotFoundException {
        if (printSearch)
            System.out.println("looking for " + name);
        for (int i = 0; i < root.length; i++) {
            if (printSearch)
                System.out.println("  in " + root[i]);
            java.io.File f = new File(root[i], name);
	    if (f.exists()) return f;
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
            path += PATH_SEP + root[i];
        return path;
    }
}
