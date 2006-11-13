/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc.io

import compat.Math.MIN_LONG
import java.io.{File,InputStream}

/** This class implements an empty abstract regular file.
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

  //########################################################################
  // Public Methods

  def path = _path

  /** Returns null. */
  final def file: File = null
  def read : InputStream = throw new Error("not suported");

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = false

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = MIN_LONG

  /** Returns all abstract subfiles of this abstract directory. */
  def elements: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'")
    Iterator.empty
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

  //########################################################################
}
