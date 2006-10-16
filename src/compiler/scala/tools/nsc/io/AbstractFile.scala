/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc.io


import java.io.{File,InputStream,DataInputStream}

object AbstractFile {

  /** Returns "getFile(new File(path))". */
  def getFile(path: String): AbstractFile = getFile(new File(path))

  /**
   * If the specified File exists and is a regular file, returns an
   * abstract regular file backed by it. Otherwise, returns <code>null</code>.
   */
  def getFile(file: File): AbstractFile =
    if (file.isFile() && file.exists()) new PlainFile(file) else null


  /** Returns "getDirectory(new File(path))". */
  def getDirectory(path: String): AbstractFile = getDirectory(new File(path))

  /**
   * if the specified File exists and is either a directory or a
   * readable zip or jar archive, returns an abstract directory
   * backed by it. Otherwise, returns null.
   *
   * @param file ...
   * @return     ...
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
 * <p>
 *   This class and its children serve to unify handling of files and
 *   directories. These files and directories may or may not have some
 *   real counter part within the file system. For example, some file
 *   handles reference files within a zip archive or virtual ones
 *   that exist only in memory.
 * </p>
 * <p>
 *   Every abstract file has a path (i.e. a full name) and a name
 *   (i.e. a short name) and may be backed by some real File. There are
 *   two different kinds of abstract files: regular files and
 *   directories. Regular files may be read and have a last modification
 *   time. Directories may list their content and look for subfiles with
 *   a specified name or path and of a specified kind.
 * </p>
 * <p>
 *   The interface does <b>not</b> allow to access the content.
 *   The class <code>symtab.classfile.AbstractFileReader</code> accesses
 *   bytes, knowing that the character set of classfiles is UTF-8. For
 *   all other cases, the class <code>SourceFile</code> is used, which honors
 *   <code>global.settings.encoding.value</code>.
 * </p>
 */
abstract class AbstractFile extends Object with Iterable[AbstractFile] {

  //########################################################################
  // Public Methods

  /** Returns the name of this abstract file. */
  def name: String

  /** Returns the path of this abstract file. */
  def path: String

  /** Returns the underlying File if any and null otherwise. */
  def file: File

  /** Is this abstract file a directory? */
  def isDirectory: Boolean

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long

  /** returns an input stream so the file can be read */
  def read : InputStream;
  final def toCharArray = {
    val input = read;
    var str : String = "";
    val buf = new Array[Byte](100);
    var eof = false;
    while (!eof) {
      val x = input.read(buf);
      eof = x == -1;
      if (!eof) str = str + new String(buf, 0, x);
    }
    input.close;
    str.toCharArray;
  }

  /** Reads the content of this abstract file into a byte array. */
  //def getBytes: Array[Byte] = error("getBytes not supported by "+this.getClass())

  /** Reads the content of this abstract file into a char array. */
  //def getChars: Array[Char] = error("getChars not supported by "+this.getClass())

  /** Returns all abstract subfiles of this abstract directory. */
  def elements: Iterator[AbstractFile]

  /** Returns the abstract file in this abstract directory with the specified
   *  name. If there is no such file, returns null. The argument
   *  <code>directory</code> tells whether to look for a directory or
   *  a regular file.
   *
   *  @param name      ...
   *  @param directory ...
   *  @return          ...
   */
  def lookupName(name: String, directory: Boolean): AbstractFile

  /** Returns the abstract file in this abstract directory with the specified
   *  path relative to it, If there is no such file, returns null. The argument
   *  <code>directory</code> tells whether to look for a directory or a regular
   *  file.
   *
   *  @param path      ...
   *  @param directory ...
   *  @return          ...
   */
  def lookupPath(path: String, directory: Boolean): AbstractFile = {
    val length = path.length()
    val separator = File.separatorChar
    assert(0 < length && path.lastIndexOf(separator) < length - 1, path)
    var file = this
    var start = 0
    while (true) {
      val index = path.indexOf(separator, start)
      assert(index < 0 || start < index)
      val name = path.substring(start, if (index < 0) length else index)
      file = file.lookupName(name, if (index < 0) directory else true)
      if (file == null || index < 0) return file
      start = index + 1
    }
    file
  }

  /** Returns the path of this abstract file. */
  override def toString() = path

  //########################################################################
}
