/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2006, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$


package scala.tools.nsc.io;


import java.io.File;

object AbstractFile {

  /** Returns "getFile(new File(path))". */
  def getFile(path: String): AbstractFile = getFile(new File(path));

  /**
   * If the specified File exists and is a regular file, returns an
   * abstract regular file backed by it. Otherwise, returns null.
   */
  def getFile(file: File): AbstractFile =
    if (file.isFile() && file.exists()) new PlainFile(file) else  null;


  /** Returns "getDirectory(new File(path))". */
  def getDirectory(path: String): AbstractFile = getDirectory(new File(path));

  /**
   * if the specified File exists and is either a directory or a
   * readable zip or jar archive, returns an abstract directory
   * backed by it. Otherwise, returns null.
   */
  def getDirectory(file: File): AbstractFile = {
    if (file.isDirectory() && file.exists()) return new PlainFile(file);
    if (file.isFile() && file.exists()) {
      val path = file.getPath();
      if (path.endsWith(".jar") || path.endsWith(".zip"))
        return ZipArchive.fromFile(file);
    }
    null
  }

}

/**
 * This class implements an abstract representation of files and
 * directories. These files and directories may have some real counter
 * part within the file system but that is not necessarily true. For
 * example, there exist abstract files that represent files within a
 * zip archive or files that exist only in memory.
 *
 * Every abstract file has a path (i.e. a full name) and a name
 * (i.e. a short name) and may be backed by some real File. There are
 * two different kinds of abstract files: regular files and
 * directories. Regular files may be read and have a last modification
 * time. Directories may list their content and look for subfiles with
 * a specified name or path and of a specified kind.
 */
abstract class AbstractFile extends Object with Iterable[AbstractFile] {

  //########################################################################
  // Public Methods

  /** Returns the name of this abstract file. */
  def name: String;

  /** Returns the path of this abstract file. */
  def path: String;

  /** Returns the underlying File if any and null otherwise. */
  def file:  File;

  /** Is this abstract file a directory? */
  def isDirectory: Boolean;

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long;

  /** Reads the content of this abstract file into a byte array. */
  def read: Array[Byte];

  /** Returns all abstract subfiles of this abstract directory. */
  def elements: Iterator[AbstractFile];

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile;

  /**
   * Returns the abstract file in this abstract directory with the
   * specified path relative to it, If there is no such file,
   * returns null. The argument "directory" tells whether to look
   * for a directory or a regular file.
   */
  def lookupPath(path: String, directory: Boolean): AbstractFile = {
    val length = path.length();
    val separator = File.separatorChar;
    assert(0 < length && path.lastIndexOf(separator) < length - 1, path);
    var file = this;
    var start = 0;
    while (true) {
      val index = path.indexOf(separator, start);
      assert(index < 0 || start < index, path+" - "+start+" - "+index);
      val name = path.substring(start, if (index < 0) length else index);
      file = file.lookupName(name, if (index < 0) directory else true);
      if (file == null || index < 0) return file;
      start = index + 1;
    }
    file
  }

  /** Returns the path of this abstract file. */
  override def toString() = path;

  //########################################################################
}
