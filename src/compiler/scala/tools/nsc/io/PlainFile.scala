/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package io

object PlainFile {
  /**
   * If the specified File exists, returns an abstract file backed
   * by it. Otherwise, returns null.
   */
  def fromPath(file: Path): PlainFile =
    if (file.exists) new PlainFile(file) else null
}

/** This class implements an abstract file backed by a File.
 */
class PlainFile(val givenPath: Path) extends AbstractFile {
  assert(path ne null)

  def name     = givenPath.name
  def path     = givenPath.path
  def file     = givenPath.jfile
  def absolute = new PlainFile(givenPath.toAbsolute)

  override def underlyingSource = Some(this)
  override def container: AbstractFile = new PlainFile(givenPath.parent)
  override def input = givenPath.toFile.inputStream()
  override def output = givenPath.toFile.outputStream()
  override def sizeOption = Some(givenPath.length.toInt)

  override def hashCode(): Int = path.hashCode
  override def equals(that: Any): Boolean = that match {
    case other: PlainFile => path == other.path
    case _                => false
  }

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = givenPath.isDirectory

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = givenPath.lastModified

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    if (!isDirectory) Iterator.empty
    else givenPath.toDirectory.list filter (_.exists) map (new PlainFile(_))
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
    val child = givenPath / name
    if ((child.isDirectory && directory) || (child.isFile && !directory)) new PlainFile(child)
    else null
  }

  /** Does this abstract file denote an existing file? */
  def create(): Unit = if (!exists) givenPath.createFile()

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit =
    if (givenPath.isFile) givenPath.delete()
    else if (givenPath.isDirectory) givenPath.toDirectory.deleteRecursively()

  /** Returns a plain file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    new PlainFile(givenPath / name)
}
