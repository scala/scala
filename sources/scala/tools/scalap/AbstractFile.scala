/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
**  $Id$
*/

package scala.tools.scalap;

import scala.collection._;
import java.io._;
import java.util.jar._;


trait AbstractFile {

    /** separator
     */
    protected val separator: Char = File.separatorChar;

    /** get name of the file
     */
    def getName: String;

    /** get path of the file
     */
    def getPath: String;

    /** does the file exist?
     */
    def exists: Boolean;

    /** is the file a directory?
     */
    def isDirectory: Boolean;

    /** read content of the file into a byte[] buffer
     */
    def content: Array[Byte];

    /** list contents of a directory
     */
    def elements: Iterator[String];

    /** open a new file
     */
    def open(name: String): AbstractFile;

    /** return an input stream for the file
     */
    def getInputStream: InputStream = new ByteArrayInputStream(content);
}

class PlainFile(f: File) with AbstractFile {

    def getName = f.getName();

    def getPath = f.getPath();

    def exists = f.exists();

    def isDirectory = f.isDirectory();

    def content = {
        val in = new FileInputStream(f);
        var rest = f.length().asInstanceOf[Int];
        val buf = new Array[Byte](rest);
        do {
            val res = in.read(buf, buf.length - rest, rest);
            if (res == -1)
                error("read error");
            rest = rest - res;
        } while (rest > 0);
        in.close();
        buf;
    }

    def elements = {
        val fs = f.listFiles();
        if (fs == null)
            Iterator.empty[String]
        else
            new Iterator[String] {
                var i = 0;
                def hasNext = (i < fs.length);
                def next = {
                    val res = fs(i).getName();
                    i = i + 1;
                    if (fs(i - 1).isDirectory() &&
                        !res.endsWith("/")) res + "/" else res;
                }
            }
    }

    def open(name: String) = new PlainFile(new File(f, name));
}

class JarArchive(f: File) with AbstractFile {
    val jarFile = try { new JarFile(f) } catch { case e => null };
    var entries: mutable.Map[String, JarArchiveEntry] = _;

    def getName = f.getName();

    def getPath = f.getPath();

    def exists = (jarFile != null);

    def isDirectory = (jarFile != null);

    def content: Array[Byte] = error("cannot read archive");

    private def load = {
        //entries = new mutable.HashMap[String, JarArchiveEntry];
        entries = new mutable.JavaMapAdaptor(new java.util.HashMap());
        if (jarFile != null) {
            val enum = jarFile.entries();
            while (enum.hasMoreElements()) {
                val candidate = enum.nextElement().asInstanceOf[JarEntry].getName();
                var i = candidate.indexOf('/');
                var j = 0;
                var files = entries;
                while (i >= 0) {
                    val dirname = candidate.substring(j, i + 1);
                    j = i + 1;
                    if (!files.isDefinedAt(dirname))
                        files(dirname) = new JarDirEntry(candidate.substring(0, j));
                    files = files(dirname).entries;
                    i = candidate.indexOf('/', j);
                }
                if (j < (candidate.length() - 1)) {
                    val filename = candidate.substring(j);
                    if (!files.isDefinedAt(filename))
                        files(filename) = new JarFileEntry(candidate);
                }
            }
        }
    }

    def list(prefix: String) = {
        val pref = prefix.replace(File.separatorChar, '/');
        if (entries == null)
            load;
        var i = pref.indexOf('/');
        var j = 0;
        var files = entries;
        var continue = true;
        while (continue && (i >= 0)) {
            val dirname = pref.substring(j, i + 1);
            j = i + 1;
            continue = files.isDefinedAt(dirname);
            if (continue) {
                files = files(dirname).entries;
                i = pref.indexOf('/', j);
            }
        }
        if (!continue)
            Iterator.empty;
        else if (j < (pref.length() - 1)) {
            if (files.isDefinedAt(pref.substring(j)))
                Predef.List(pref).elements;
            else
                Iterator.empty;
        } else
            files.keys;
    }

    def elements = list("");

    def open(fname: String) = {
        if (entries == null)
            load;
        val name = fname.replace(File.separatorChar, '/');
        var i = name.indexOf('/');
        var j = 0;
        var namelen = name.length();
        var files = entries;
        var res: AbstractFile = null;
        while ((res == null) && (i >= 0)) {
            val dirname = name.substring(j, i + 1);
            j = i + 1;
            if (files != null) {
                if (files.isDefinedAt(dirname)) {
                    if (j == namelen)
                        res = files(dirname);
                    else
                        files = files(dirname).entries;
                } else
                    files = null;
            }
            i = name.indexOf('/', j);
        }
        if (res != null)
            res
        else if (j < (namelen - 1)) {
            if (files == null)
                new JarArchiveEntry(name, false);
            else {
                val filename = name.substring(j);
                if (files.isDefinedAt(filename))
                    files(filename)
                else
                    new JarArchiveEntry(name, false)
            }
        } else
            new JarArchiveEntry(name, true);
    }

    class JarArchiveEntry(name: String, dir: Boolean) with AbstractFile {

        def getName = name.substring(
            name.lastIndexOf('/', name.length() - (if (dir) 2 else 1)) + 1);

        def getPath = name;

        def getFullName = name;

        def exists = false;

        def isDirectory = dir;

        def elements: Iterator[String] = error("nothing to iterate over");

        def content: Array[Byte] = error("cannot read archive");

        def open(n: String): AbstractFile =  error("cannot open archive entry");

        def entries: mutable.Map[String, JarArchiveEntry] = error("no entries");
    }

    final class JarDirEntry(name: String) extends JarArchiveEntry(name, true) {
        //val entr = new mutable.HashMap[String, JarArchiveEntry];
        val entr = new mutable.JavaMapAdaptor[String, JarArchiveEntry](new java.util.HashMap());

        override def getPath = JarArchive.this.getPath + "(" + name + ")";

        override def exists = true;

        override def elements = JarArchive.this.list(name);

        override def open(fname: String) = JarArchive.this.open(
            name + fname.replace(File.separatorChar, '/'));

        override def entries: mutable.Map[String, JarArchiveEntry] = entr;
    }

    final class JarFileEntry(name: String) extends JarArchiveEntry(name, false) {

        override def getPath = JarArchive.this.getPath + "(" + name + ")";

        override def exists = true;

        override def content: Array[Byte] = {
            val jarEntry = jarFile.getJarEntry(name);
            if (jarEntry == null)
                error("unable to read " + name);
            val in = jarFile.getInputStream(jarEntry);
            var rest = jarEntry.getSize().asInstanceOf[Int];
            val buf = new Array[Byte](rest);
            do {
                val res = in.read(buf, buf.length - rest, rest);
                if (res == -1)
                    error("read error");
                rest = rest - res;
            } while (rest > 0);
            in.close();
            buf;
        }
    }
}
