/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc
package io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream,
                File, InputStream, OutputStream}

/** This class implements an in-memory file.
 *
 *  @author  Philippe Altherr
 *  @version 1.0, 23/03/2004
 */
class VirtualFile(val name: String, _path: String) extends AbstractFile {

  assert((name ne null) && (path ne null), name + " - " + path)

  //########################################################################
  // Public Constructors

  /**
   * Initializes this instance with the specified name and an
   * identical path.
   *
   * @param name the name of the virtual file to be created
   * @return     the created virtual file
   */
  def this(name: String) = this(name, name)

  override def hashCode = name.hashCode
  override def equals(that : Any) = that match {
  case that : VirtualFile => name == that.name
  case _ => false
  }


  //########################################################################
  // Private data
  private var content = new Array[Byte](0)

  //########################################################################
  // Public Methods

  def path = _path

  def absolute = this

  /** Returns null. */
  final def file: File = null

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

  def container : AbstractFile = throw new Error("not supported")

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = false

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = Math.MIN_LONG

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'")
    Iterator.empty
  }

  /** Does this abstract file denote an existing file? */
  def create {
    throw new UnsupportedOperationException
  }

  /** Delete the underlying file or directory (recursively). */
  def delete {
    throw new UnsupportedOperationException
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
    assert(isDirectory, "not a directory '" + this + "'")
    null
  }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    throw new UnsupportedOperationException()

  //########################################################################
}
