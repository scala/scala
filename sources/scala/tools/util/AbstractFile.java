/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.util.jar.*;

public abstract class AbstractFile {

    /** separator
     */
    protected char separator = File.separatorChar;

    /** table of all opened jar-files
     */
    protected static Hashtable  opened = new Hashtable();

    /** get name of the file
     */
    public abstract String getName();

    /** get path of the file
     */
    public abstract String getPath();

    /** does the file exist?
     */
    public abstract boolean exists();

    /** is the file a directory?
     */
    public abstract boolean isDirectory();

    /** date file was last modified
     */
    public abstract long lastModified();

    /** read content of the file into a byte[] buffer
     */
    public abstract byte[] read() throws IOException;

    /** list contents of a directory
     */
    public abstract String[] list() throws IOException;

    /** open a new file
     */
    public abstract AbstractFile open(String name);

    /** return an input stream for the file
     */
    public InputStream getInputStream() throws IOException {
        return new ByteArrayInputStream(read());
    }

    /** open file 'name' in directory 'dirname'
     */
    public static AbstractFile open(String dirname, String name) {
        AbstractFile res;
        if (dirname == null)
            res = new PlainFile(new File(name));
        else if (dirname.endsWith(".zip")) {
            AbstractFile dir = (AbstractFile)opened.get(dirname);
            if (dir == null) {
                dir = new ZipDir(new File(dirname));
                if (dir.isDirectory())
                    opened.put(dirname, dir);
            }
            res = (name == null) ? dir : dir.open(name);
        } else if (dirname.endsWith(".jar")) {
            AbstractFile dir = (AbstractFile)opened.get(dirname);
            if (dir == null) {
                dir = new JarArchive(new File(dirname));
                if (dir.isDirectory())
                    opened.put(dirname, dir);
            }
            res = (name == null) ? dir : dir.open(name);
        } else if (name == null)
            res = new PlainFile(new File(dirname));
        else
            res = new PlainFile(new File(dirname, name));
        if (!res.exists())
            res = null;
        return res;
    }

    /** create file given by a fully qualified name from root directory `outdir';
     *  create intermediate directories if they do not exist already
     */
    public static File create(File outdir, String name,
                              String suffix) throws IOException {
        int     start = 0;
        int     end = name.indexOf('.');
        while (end >= start) {
            outdir = new File(outdir, name.substring(start, end));
            if (!outdir.exists())
                outdir.mkdir();
            start = end + 1;
            end = name.indexOf('.', start);
        }
        return new File(outdir, name.substring(start) + suffix);
    }
}

class PlainFile extends AbstractFile {
    File f;

    PlainFile(File f) {
        this.f = f;
    }

    public String getName() {
        return f.getName();
    }

    public String getPath() {
        return f.getPath();
    }

    public boolean exists() {
        return f.exists();
    }

    public boolean isDirectory() {
        return f.isDirectory();
    }

    public long lastModified() {
        return f.lastModified();
    }

    public byte[] read() throws IOException {
        FileInputStream in = new FileInputStream(f);
        int rest = (int)f.length();
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

    public String[] list() throws IOException {
        File[] fs = f.listFiles();
        if (fs == null)
            return new String[0];
        String[] res = new String[fs.length];
        for (int i = 0; i < fs.length; i++) {
            res[i] = fs[i].getName();
            if (fs[i].isDirectory() &&
                !res[i].endsWith("/"))
                res[i] = res[i] + "/";
        }
        return res;
    }

    public AbstractFile open(String name) {
        return new PlainFile(new File(f, name));
    }
}

class ZippedFile extends AbstractFile {
    ZipDir      dir;
    ZipEntry    zipEntry;

    {
        separator = '/';
    }

    ZippedFile(ZipDir dir, String name) {
        this.dir = dir;
        if (dir.zipFile != null) {
            name = name.replace(File.separatorChar, separator);
            zipEntry = this.dir.zipFile.getEntry(name);
            if (zipEntry == null)
                zipEntry = this.dir.zipFile.getEntry(name + separator);
        }
    }

    public String getName() {
        return zipEntry.getName();
    }

    public String getPath() {
        return dir.getPath() + "(" + zipEntry.getName() + ")";
    }

    public boolean exists() {
        return (zipEntry != null);
    }

    public boolean isDirectory() {
        return zipEntry.isDirectory();
    }

    public long lastModified() {
        return zipEntry.getTime();
    }

    public byte[] read() throws IOException {
        InputStream in = dir.zipFile.getInputStream(zipEntry);
        int rest = (int)zipEntry.getSize();
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

    public String[] list() throws IOException {
        if (!isDirectory())
            throw new IOException("not a directory");
        return dir.list(zipEntry.getName());
    }

    public AbstractFile open(String name) {
        String  pathname = zipEntry.getName();
        return new ZippedFile(dir, pathname + name);
    }
}

class ZipDir extends AbstractFile {
    File f;
    ZipFile zipFile;

    {
        separator = '/';
    }

    ZipDir(File f) {
        this.f = f;
        try {
            zipFile = new ZipFile(f);
        } catch (ZipException e) {
        } catch (IOException e) {}
    }

    public String getName() {
        return f.getName();
    }

    public String getPath() {
        return f.getPath();
    }

    public boolean exists() {
        return (zipFile != null);
    }

    public boolean isDirectory() {
        return (zipFile != null);
    }

    public long lastModified() {
        return -1;
    }

    public byte[] read() throws IOException {
        throw new IOException("cannot read directory");
    }

    public String[] list(String prefix) {
        int n = 0;
        for (Enumeration enum = zipFile.entries(); enum.hasMoreElements();) {
            ZipEntry    e = (ZipEntry)enum.nextElement();
            if (e.getName().startsWith(prefix)) {
                String  candidate = e.getName().substring(prefix.length());
                if (candidate.indexOf(separator) < 0)
                    n++;
            }
        }
        String[] filenames = new String[n];
        n = 0;
        for (Enumeration enum = zipFile.entries(); enum.hasMoreElements();) {
            ZipEntry    e = (ZipEntry)enum.nextElement();
            if (e.getName().startsWith(prefix)) {
                String  candidate = e.getName().substring(prefix.length());
                if (candidate.indexOf(separator) < 0)
                    filenames[n++] = candidate;
            }
        }
        return filenames;
    }

    public String[] list() throws IOException {
        return list("");
    }

    public AbstractFile open(String name) {
        return new ZippedFile(this, name);
    }
}

final class JarArchive extends AbstractFile {
    File f;
    JarFile jarFile;
    HashMap entries;

    public final static String[] EMPTY = new String[0];


    JarArchive(File f) {
        try {
            jarFile = new JarFile(this.f = f);
        }
        catch (ZipException e) {}
        catch (IOException e) {}
    }

    public String getName() {
        return f.getName();
    }

    public String getPath() {
        return f.getPath();
    }

    public boolean exists() {
        return jarFile != null;
    }

    public boolean isDirectory() {
        return jarFile != null;
    }

    public long lastModified() {
        return -1;
    }

    public byte[] read() throws IOException {
        throw new IOException("cannot read archive");
    }

    private void load() {
        entries = new HashMap();
        if (jarFile == null)
            return;
        Enumeration enum = jarFile.entries();
        while (enum.hasMoreElements()) {
            String candidate = ((JarEntry)enum.nextElement()).getName();
            int i = candidate.indexOf('/');
            int j = 0;
            HashMap files = entries;
            while (i >= 0) {
                String dirname = candidate.substring(j, j = (i + 1));
                JarDirEntry dir = (JarDirEntry)files.get(dirname);
                if (dir == null)
                    files.put(dirname, dir = new JarDirEntry(
                        candidate.substring(0, j)));
                files = dir.entries;
                i = candidate.indexOf('/', j);
            }
            if (j < (candidate.length() - 1)) {
                String filename = candidate.substring(j);
                JarFileEntry file = (JarFileEntry)files.get(filename);
                if (file == null)
                    files.put(filename, new JarFileEntry(candidate));
            }
        }
    }

    public String[] list(String prefix) {
        prefix = prefix.replace(File.separatorChar, '/');
        if (entries == null)
            load();
        int i = prefix.indexOf('/');
        int j = 0;
        HashMap files = entries;
        while (i >= 0) {
            String dirname = prefix.substring(j, j = (i + 1));
            JarDirEntry dir = (JarDirEntry)files.get(dirname);
            if (dir == null)
                return EMPTY;
            files = dir.entries;
            i = prefix.indexOf('/', j);
        }
        if (j < (prefix.length() - 1)) {
            String filename = prefix.substring(j);
            return (files.get(filename) != null) ? new String[]{prefix}
                                                 : EMPTY;
        } else
            return (String[])files.keySet().toArray(new String[files.size()]);
    }

    public String[] list() throws IOException {
        return list("");
    }

    public AbstractFile open(String name) {
        if (entries == null)
            load();
        name = name.replace(File.separatorChar, '/');
        int i = name.indexOf('/');
        int j = 0;
        int namelen = name.length();
        HashMap files = entries;
        while (i >= 0) {
            String dirname = name.substring(j, j = (i + 1));
            if (files != null) {
                JarDirEntry dir = (JarDirEntry)files.get(dirname);
                if (dir == null)
                    files = null;
                else if (j == namelen)
                    return dir;
                else
                    files = dir.entries;
            }
            i = name.indexOf('/', j);
        }
        if (j < (namelen - 1)) {
            String filename = name.substring(j);
            if (files == null)
                return new NoJarFileEntry(name);
            JarFileEntry file = (JarFileEntry)files.get(filename);
            if (file == null)
                return new NoJarFileEntry(name);
            else
                return file;
        } else
            return new NoJarDirEntry(name);
    }

    static class NoJarDirEntry extends AbstractFile {
        String name;

        NoJarDirEntry(String name) {
            this.name = name;
        }

        public String getName() {
            return name.substring(
                name.lastIndexOf('/', name.length() - 2) + 1);
        }

        public String getPath() {
            return name;
        }

        public String getFullName() {
            return name;
        }

        public boolean exists() {
            return false;
        }

        public boolean isDirectory() {
            return true;
        }

        public long lastModified() {
            return -1;
        }

        public String[] list() throws IOException {
            throw new IOException("not a directory");
        }

        public byte[] read() throws IOException {
            throw new IOException("cannot read archive");
        }

        public AbstractFile open(String fname) {
            throw new Error("cannot open archive entry");
        }
    }

    final class JarDirEntry extends NoJarDirEntry {
        HashMap entries;

        JarDirEntry(String name) {
            super(name);
            this.entries = new HashMap();
        }

        public String getPath() {
            return JarArchive.this.getPath() + "(" + name + ")";
        }

        public boolean exists() {
            return true;
        }

        public String[] list() throws IOException {
            return JarArchive.this.list(name);
        }

        public AbstractFile open(String fname) {
            fname = fname.replace(File.separatorChar, '/');
            return JarArchive.this.open(name + fname);
        }
    }

    static class NoJarFileEntry extends AbstractFile {
        String name;

        NoJarFileEntry(String name) {
            this.name = name;
        }

        public String getName() {
            return name.substring(
                name.lastIndexOf('/', name.length() - 1) + 1);
        }

        public String getFullName() {
            return name;
        }

        public String getPath() {
            return name;
        }

        public boolean exists() {
            return false;
        }

        public boolean isDirectory() {
            return false;
        }

        public long lastModified() {
            return -1;
        }

        public String[] list() throws IOException {
            throw new IOException("not a directory");
        }

        public byte[] read() throws IOException {
            throw new IOException("cannot read archive");
        }

        public AbstractFile open(String fname) {
            throw new Error("not a directory");
        }
    }

    final class JarFileEntry extends NoJarFileEntry {

        JarFileEntry(String name) {
            super(name);
        }

        public String getPath() {
            return JarArchive.this.getPath() + "(" + name + ")";
        }

        public boolean exists() {
            return true;
        }

        public long lastModified() {
            return jarFile.getJarEntry(name).getTime();
        }

        public byte[] read() throws IOException {
            JarEntry jarEntry = jarFile.getJarEntry(name);
            if (jarEntry == null)
                throw new IOException("unable to read " + name);
            InputStream in = jarFile.getInputStream(jarEntry);
            int rest = (int)jarEntry.getSize();
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
}
