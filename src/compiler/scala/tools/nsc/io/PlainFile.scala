/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc.io

import java.io.{File, FileInputStream, IOException}

object PlainFile {

  /** Returns "fromFile(new File(path))". */
  //def fromPath(path: String): AbstractFile = fromFile(new File(path));

  /**
   * If the specified File exists, returns an abstract file backed
   * by it. Otherwise, returns null.
   */
  def fromFile(file: File): AbstractFile =
    if (file.exists()) new PlainFile(file) else null

}

/** This class implements an abstract file backed by a File.
 */
class PlainFile(val file: File) extends AbstractFile {

  assert(file ne null)
  assert(file.exists(), "non-existent file: " + file)

  //########################################################################
  // Public Methods

  /** Returns the name of this abstract file. */
  def name = file.getName()

  /** Returns the path of this abstract file. */
  def path = file.getPath()

  override def read = new FileInputStream(file)

  override def size = Some(file.length.toInt)

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
  def isDirectory: Boolean = file.isDirectory()

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = file.lastModified()

  /** Returns all abstract subfiles of this abstract directory. */
  def elements: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'")
    val names: Array[String] = file.list()
    if ((names eq null) || names.length == 0) Iterator.empty
    else Iterator.fromArray(names).map { name: String => new File(file, name) }
      .filter(.exists()).map(file => new PlainFile(file))
  }

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   *
   * @param name      ...
   * @param directory ...
   * @return          ...
   */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    //assert(isDirectory, "not a directory '" + this + "'")
    val child = new File(file, name)
    if (!child.exists() || (directory != child.isDirectory) ||
        directory == child.isFile()) null
    else new PlainFile(child)
  }

  //########################################################################
}
