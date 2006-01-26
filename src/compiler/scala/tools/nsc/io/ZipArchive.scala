package scala.tools.nsc.io;

import java.io._;
import java.util.zip._;
import scala.collection.mutable.HashMap;

object ZipArchive {
  def fromPath(path : String) : AbstractFile = fromFile(new File(path));
  def fromFile(file : File  ) : AbstractFile = {
      try {
        new ZipArchive(file, new ZipFile(file));
      } catch {
      case e : IOException => null;
      }
  }
  def fromArchive(archive : ZipFile) =
    new ZipArchive(new File(archive.getName()), archive);
}

final class ZipArchive(file : File, archive : ZipFile) extends PlainFile(file) {

	assert(archive != null);
  private var root0 : DirEntry = _;
  assert(!super.isDirectory);

  private def root = {
    if (root0 == null) load else {}
    root0
  }



  override def isDirectory = true;
  override def list : List[AbstractFile] = for (val e : AbstractFile <- root.list) yield e.asInstanceOf[AbstractFile];
  override def lookupName(name : String, isDirectory : Boolean) : AbstractFile = root.lookupName(name, isDirectory);


  class Entry(name : String, path : String) extends VirtualFile(name, path) {
    //override def path = ZipArchive.this.path  + "(" + super.path + ")";
  }
  class DirEntry(name : String, path : String) extends Entry(name, path) {
    val entries = new HashMap[String,Entry];
    var entry : ZipEntry = _;

    override def isDirectory = true;
    override def lastModified = if (entry == null) super.lastModified else entry.getTime();

   override def list = for (val e : AbstractFile <- entries.values.toList) yield e;

   override def lookupName(name : String, isDirectory : Boolean) : AbstractFile =
     entries(name + (if (isDirectory) "/" else ""));

  }
  class FileEntry(name : String, path : String, entry : ZipEntry) extends Entry(name, path) {
    override def lastModified = entry.getTime();
    override def read = {
      val in = archive.getInputStream(entry);
      var rest = entry.getSize().asInstanceOf[Int];
      val buf = new Array[Byte](rest);

      var break = false;
      while (!break) {
        val res = in.read(buf, buf.length - rest, rest);
        if (res == -1)
            throw new IOException("read error");
        rest = rest - res;
        break = !(rest > 0);
      }
      in.close();
      buf;
    }
  }


  /** Loads the archive and creates the root directory. */
  private def load : Unit = {
    root0 = new DirEntry("<root>", "/");
    val dirs = new HashMap[String,DirEntry];
    dirs("/") = root;

    val e = archive.entries();
    while (e.hasMoreElements()) {
      val entry = e.nextElement().asInstanceOf[ZipEntry];
      val path = entry.getName();
      assert(entry.isDirectory() == path.endsWith("/"), "" + this +"-"+ path);
      if (entry.isDirectory()) {
        val dir = getDir(dirs, path);
        assert(dir.entry == null, "" + this + " - " + path);
        dir.entry = entry;
      } else {
        val index = path.lastIndexOf('/');
        val name = if (index < 0) path else path.substring(index + 1);
        val home = if (index < 0) "/"  else path.substring(0, index + 1);
        val parent = getDir(dirs, home);
        assert(!parent.entries.contains(path), ""+ this + " - " + path);
        parent.entries(name) = new FileEntry(name, path, entry);
      }
    }
  }

  /**
   * Lookups the specified table for a DirEntry with the specified
   * path. If successful, returns the found DirEntry. Otherwise
   * creates a new DirEntry, enters it into the table and in the
   * table of its parent ZipDir and returns it.
   */
  private def getDir(dirs : HashMap[String,DirEntry], path : String): DirEntry = {
    var dir = dirs(path);
    if (dir == null) {
      val index = path.lastIndexOf('/', path.length() - 2);
      val name = if(index < 0) path else path.substring(index + 1);
      val home = if(index < 0) "/"  else path.substring(0, index + 1);
      val parent = getDir(dirs, home);
      dir = new DirEntry(name.substring(0, name.length() - 1), path);
      parent.entries(name) = dir;
      dirs(path) = dir;
    }
    dir
  }

}
