/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2006, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$


package scala.tools.nsc.io;


import java.io.{File,IOException};

/** This class implements an empty abstract regular file. */
class VirtualFile(val name: String, _path: String) extends AbstractFile {

  assert(name != null && path != null, name + " - " + path);

  //########################################################################
  // Public Constructors

  /**
   * Initializes this instance with the specified name and an
   * identical path.
   */
  def this(name: String) = this(name, name);

  //########################################################################
  // Public Methods

  def path = _path;

  /** Returns null. */
  final def file: File = null;

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = false;

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = Long.MIN_VALUE;

  /** Returns all abstract subfiles of this abstract directory. */
  def elements: Iterator[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'");
    Iterator.empty;
  }

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    assert(isDirectory, "not a directory '" + this + "'");
    null;
  }

  //########################################################################
}
