/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc
package io

import java.io.{File, FileInputStream, FileOutputStream, IOException}

object PlainFile {
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
  private val fpath = try { file.getCanonicalPath }
                      catch { case _: IOException => file.getAbsolutePath }

  assert(file ne null)
//  assert(file.exists(), "non-existent file: " + file)

  /** Returns the name of this abstract file. */
  def name = file.getName()

  /** Returns the path of this abstract file. */
  def path = file.getPath()

  /** The absolute file. */
  def absolute = new PlainFile(file.getCanonicalFile())

  override def container : AbstractFile = new PlainFile(file.getParentFile)

  override def input = new FileInputStream(file)
  override def output = new FileOutputStream(file)

  override def sizeOption = Some(file.length.toInt)

  override def hashCode(): Int = fpath.hashCode

  override def equals(that: Any): Boolean =
    that.isInstanceOf[PlainFile] &&
      fpath.equals(that.asInstanceOf[PlainFile].fpath)

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = file.isDirectory()

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = file.lastModified()

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'")
    val names: Array[String] = file.list()
    if ((names eq null) || names.length == 0) Iterator.empty
    else names.iterator.map { name: String => new File(file, name) }
      .filter(_.exists).map(file => new PlainFile(file))
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

  /** Does this abstract file denote an existing file? */
  def create {
    if (!exists)
      file.createNewFile()
  }

  /** Delete the underlying file or directory (recursively). */
  def delete {
    if (file.isFile) file.delete
    else if (file.isDirectory) {
      elements.foreach(_.delete)
      file.delete
    }
  }

  /** Returns a plain file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = {
    val f = new File(file, name)
    new PlainFile(f)
  }

}
