/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package io

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream }
import java.io.{ File => JFile }

/** This class implements an in-memory file.
 *
 *  @author  Philippe Altherr
 *  @version 1.0, 23/03/2004
 */
class VirtualFile(val name: String, override val path: String) extends AbstractFile {
  /**
   * Initializes this instance with the specified name and an
   * identical path.
   *
   * @param name the name of the virtual file to be created
   * @return     the created virtual file
   */
  def this(name: String) = this(name, name)

  override def hashCode = path.hashCode
  override def equals(that: Any) = that match {
    case x: VirtualFile => x.path == path
    case _              => false
  }

  //########################################################################
  // Private data
  private var content = new Array[Byte](0)

  //########################################################################
  // Public Methods
  def absolute = this

  /** Returns null. */
  final def file: JFile = null

  override def sizeOption: Option[Int] = Some(content.size)

  def input : InputStream = new ByteArrayInputStream(content);

  override def output: OutputStream = {
    new ByteArrayOutputStream() {
      override def close() {
        super.close()
        content = toByteArray()
      }
    }
  }

  def container: AbstractFile =  unsupported

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = false

  /** Returns the time that this abstract file was last modified. */
  private var _lastModified: Long = 0
  def lastModified: Long = _lastModified
  def lastModified_=(x: Long) = _lastModified = x

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'")
    Iterator.empty
  }

  /** Does this abstract file denote an existing file? */
  def create() { unsupported }

  /** Delete the underlying file or directory (recursively). */
  def delete() { unsupported }

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
    assert(isDirectory, "not a directory '" + this + "'")
    null
  }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported

  //########################################################################
}
