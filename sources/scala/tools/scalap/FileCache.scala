/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
**  $Id$
*/

package scalap;

import scala.collection._;
import java.io._;
import java.util.jar._;


class FileCache {

    /** table of all opened jar-files
     */
    protected val opened: mutable.Map[String, AbstractFile] =
        //new mutable.HashMap[String, AbstractFile];
        new mutable.JavaMapAdaptor(new java.util.HashMap());

    def open(name: String): AbstractFile = open(null, name);

    /** open file 'name' in directory 'dirname'; 'name' is a path
     *  relative to 'dirname'; 'dirname' might also refer to a .zip
     *  or .jar file
     */
    def open(dirname: String, name: String): AbstractFile = {
        if (dirname == null)
            new PlainFile(new File(name))
        else if (dirname.endsWith(".jar")) {
            if (!opened.isDefinedAt(dirname))
                opened(dirname) = new JarArchive(new File(dirname));
            if (name == null) opened(dirname) else opened(dirname).open(name)
        } else if (name == null)
            new PlainFile(new File(dirname))
        else
            new PlainFile(new File(dirname, name))
    }
}
