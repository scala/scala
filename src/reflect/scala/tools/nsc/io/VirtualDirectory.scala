/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 */

package scala.tools.nsc
package io

import scala.collection.mutable

/**
 * An in-memory directory.
 *
 * @author Lex Spoon
 */
class VirtualDirectory(val name: String, maybeContainer: Option[VirtualDirectory])
extends AbstractFile {
  def path: String =
    maybeContainer match {
      case None => name
      case Some(parent) => parent.path+'/'+ name
    }

  def absolute = this

  def container = maybeContainer.get
  def isDirectory = true
  var lastModified: Long = System.currentTimeMillis

  override def file = null
  override def input = sys.error("directories cannot be read")
  override def output = sys.error("directories cannot be written")

  /** Does this abstract file denote an existing file? */
  def create() { unsupported }

  /** Delete the underlying file or directory (recursively). */
  def delete() { unsupported }

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported

  private val files = mutable.Map.empty[String, AbstractFile]

  // the toList is so that the directory may continue to be
  // modified while its elements are iterated
  def iterator = files.values.toList.iterator

  override def lookupName(name: String, directory: Boolean): AbstractFile =
    files get name filter (_.isDirectory == directory) orNull

  override def fileNamed(name: String): AbstractFile =
    Option(lookupName(name, false)) getOrElse {
      val newFile = new VirtualFile(name, path+'/'+name)
      files(name) = newFile
      newFile
    }

  override def subdirectoryNamed(name: String): AbstractFile =
    Option(lookupName(name, true)) getOrElse {
      val dir = new VirtualDirectory(name, Some(this))
      files(name) = dir
      dir
    }

  def clear() {
    files.clear();
  }
}
