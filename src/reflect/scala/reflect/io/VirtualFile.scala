/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package io

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream, File => JFile }

/** This class implements an in-memory file.
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
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

  private[this] var content = Array.emptyByteArray

  def absolute = this

  /** Returns null. */
  def file: JFile = null

  override def sizeOption: Option[Int] = Some(content.length)

  def input : InputStream = new ByteArrayInputStream(content)

  override def output: OutputStream = {
    new ByteArrayOutputStream() {
      override def close(): Unit = {
        super.close()
        content = toByteArray()
      }
    }
  }

  override def unsafeToByteArray: Array[Byte] = {
    content
  }

  def container: AbstractFile = NoAbstractFile

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = false

  /** @inheritdoc */
  override def isVirtual: Boolean = true

  // private var _lastModified: Long = 0
  // _lastModified

  /** Returns the time that this abstract file was last modified. */
  // !!! Except it doesn't - it's private and never set - so I replaced it
  // with constant 0 to save the field.
  def lastModified: Long = 0

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'")
    Iterator.empty
  }

  /** Does this abstract file denote an existing file? */
  def create(): Unit = unsupported()

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit = unsupported()

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    assert(isDirectory, "not a directory '" + this + "'")
    null
  }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported()
}
