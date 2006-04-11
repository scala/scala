/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2006, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$


package scala.tools.util;


import java.io.{File, FileInputStream, IOException};

object PlainFile {

  /** Returns "fromFile(new File(path))". */
  def fromPath(path: String): AbstractFile = fromFile(new File(path));

  /**
   * If the specified File exists, returns an abstract file backed
   * by it. Otherwise, returns null.
   */
  def fromFile(file: File): AbstractFile =
    if (file.exists()) new PlainFile(file) else null;

}

/** This class implements an abstract file backed by a File. */
class PlainFile(val file: File) extends AbstractFile {

  assert(file != null);
  assert(file.exists(), "non-existent file: " + file);

  //########################################################################
  // Public Methods

  /** Returns the name of this abstract file. */
  def name = file.getName();

  /** Returns the path of this abstract file. */
  def path = file.getPath();


  override def hashCode(): Int =
    try { file.getCanonicalPath().hashCode() }
    catch { case _: IOException => 0 }

  override def equals(that: Any): Boolean =
    try {
      that.isInstanceOf[PlainFile] &&
      file.getCanonicalPath().equals(that.asInstanceOf[PlainFile].file.getCanonicalPath());
    } catch {
      case _: IOException =>
        that.isInstanceOf[PlainFile] &&
        file.getAbsolutePath().equals(that.asInstanceOf[PlainFile].file.getAbsolutePath());
    }

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = file.isDirectory();

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = file.lastModified();

  /** Reads the content of this abstract file into a byte array. */
  def read: Array[Byte] = {
    assert(!isDirectory, "cannot read directory '" + this + "'");
    val in = new FileInputStream(file);
    var rest: Int = file.length().toInt;
    val buf: Array[Byte] = new Array[Byte](rest);
    while (rest > 0) {
      val res = in.read(buf, buf.length - rest, rest);
      if (res == -1)
        throw new IOException("read error");
      rest = rest - res;
    }
    in.close();
    return buf;
  }

  /** Returns all abstract subfiles of this abstract directory. */
  def elements: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'");
    val names: Array[String] = file.list();
    if (names == null || names.length == 0) Iterator.empty;
    else Iterator.fromArray(names).map(name: String => new File(file, name))
      .filter(.exists()).map(file => new PlainFile(file))
  }

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    //assert(isDirectory, "not a directory '" + this + "'");
    val child = new File(file, name);
    if (!child.exists() || (directory != child.isDirectory) ||
        directory == child.isFile()) null;
    else new PlainFile(child);
  }

  //########################################################################
}
