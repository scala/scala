/*     ___ ____ ___   __   ___  _____
**    / _// __// _ | / /  / _ |/_  _/     Scala test
**  __\ \/ /__/ __ |/ /__/ __ | / /       (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_//_/
**
**  $Id$
*/

package scala.tools.scalatest;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;


class FileUtils {

    public final static String FILE_SEP = System.getProperty("file.separator");
    public final static String PATH_SEP = System.getProperty("path.separator");

    /**
     * Creates a new directory.
     * If id
     */
    public static boolean createDir(File dir) {
        deleteDir(dir);
        return dir.mkdirs();
    }

    /**
     * Deletes all files and subdirectories under <code>dir</code>.
     * Returns <code>true</code> if all deletions were successful.
     * If a deletion fails, the method stops attempting to delete and returns false.
     * (see http://javaalmanac.com/egs/java.io/DeleteDir.html)
     */
    public static boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i = 0; i < children.length; i++) {
                boolean success = deleteDir(new File(dir, children[i]));
                if (!success) {
                    return false;
                }
            }
        }

        // The directory is now empty so delete it
        return dir.delete();
    }

    /**
     * Returns the default temporary-file directory.
     */
    public static File getTempDir() {
        File dir = null;
        try {
            File dummy = File.createTempFile("dummy", null);
            dir = dummy.getParentFile();
            dummy.delete();
        } catch (IOException e) {}
        return dir;
    }

    /**
     * Compares two files using a Java implementation of the GNU diff
     * available at http://www.bmsi.com/java/#diff.
     *
     * @param f1
     * @param f2
     */
    public static String compareFiles(File f1, File f2) {
        String res = null;
        try {
            ByteArrayOutputStream diffStream = new ByteArrayOutputStream();
            PrintStream diffOutput = new PrintStream(
                new BufferedOutputStream(diffStream), true);
            System.setOut(diffOutput);
            System.setErr(diffOutput);
            DiffPrint.main(new String[]{ f1.getCanonicalPath(), f2.getCanonicalPath() });
            System.setOut(System.out);
            System.setErr(System.err);
            res = diffStream.toString();
            if (res.startsWith("No"))
                res = "";
        } catch (IOException e) {}
        return res;
    }

}
