/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package io

import java.io.{ FileOutputStream, IOException, InputStream, OutputStream, BufferedOutputStream }
import java.net.URL
import scala.collection.mutable.ArrayBuffer

/**
 * @author Philippe Altherr
 * @version 1.0, 23/03/2004
 */
object AbstractFile {
  /** Returns "getFile(new File(path))". */
  def getFile(path: String): AbstractFile = getFile(File(path))
  def getFile(path: Path): AbstractFile = getFile(path.toFile)

  /**
   * If the specified File exists and is a regular file, returns an
   * abstract regular file backed by it. Otherwise, returns <code>null</code>.
   */
  def getFile(file: File): AbstractFile =
    if (file.isFile) new PlainFile(file) else null

  /** Returns "getDirectory(new File(path))". */
  def getDirectory(path: Path): AbstractFile = getDirectory(path.toFile)

  /**
   * If the specified File exists and is either a directory or a
   * readable zip or jar archive, returns an abstract directory
   * backed by it. Otherwise, returns <code>null</code>.
   *
   * @param file ...
   * @return     ...
   */
  def getDirectory(file: File): AbstractFile =
    if (file.isDirectory) new PlainFile(file)
    else if (file.isFile && Path.isExtensionJarOrZip(file.jfile)) ZipArchive fromFile file
    else null

  /**
   * If the specified URL exists and is a readable zip or jar archive,
   * returns an abstract directory backed by it. Otherwise, returns
   * <code>null</code>.
   *
   * @param file ...
   * @return     ...
   */
  def getURL(url: URL): AbstractFile = {
    if (url == null || !Path.isExtensionJarOrZip(url.getPath)) null
    else ZipArchive fromURL url
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
abstract class AbstractFile extends AnyRef with Iterable[AbstractFile] {

  /** Returns the name of this abstract file. */
  def name: String

  /** Returns the path of this abstract file. */
  def path: String

  /** Returns the path of this abstract file in a canonical form. */
  def canonicalPath: String = if (file == null) path else file.getCanonicalPath

  /** Checks extension case insensitively. */
  def hasExtension(other: String) = extension == other.toLowerCase
  private lazy val extension: String = Path.extension(name)

  /** The absolute file, if this is a relative file. */
  def absolute: AbstractFile

  /** Returns the containing directory of this abstract file */
  def container : AbstractFile

  /** Returns the underlying File if any and null otherwise. */
  def file: JFile

  /** An underlying source, if known.  Mostly, a zip/jar file. */
  def underlyingSource: Option[AbstractFile] = None

  /** Does this abstract file denote an existing file? */
  def exists: Boolean = (file eq null) || file.exists

  /** Does this abstract file represent something which can contain classfiles? */
  def isClassContainer = isDirectory || (file != null && (extension == "jar" || extension == "zip"))

  /** Create a file on disk, if one does not exist already. */
  def create(): Unit

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit

  /** Is this abstract file a directory? */
  def isDirectory: Boolean

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long

  /** returns an input stream so the file can be read */
  def input: InputStream

  /** Returns an output stream for writing the file */
  def output: OutputStream

  /** Returns a buffered output stream for writing the file - defaults to out */
  def bufferedOutput: BufferedOutputStream = new BufferedOutputStream(output)

  /** size of this file if it is a concrete file. */
  def sizeOption: Option[Int] = None

  def toURL: URL = if (file == null) null else file.toURI.toURL

  /** Returns contents of file (if applicable) in a Char array.
   *  warning: use <code>Global.getSourceFile()</code> to use the proper
   *  encoding when converting to the char array.
   */
  @throws(classOf[IOException])
  def toCharArray = new String(toByteArray).toCharArray

  /** Returns contents of file (if applicable) in a byte array.
   */
  @throws(classOf[IOException])
  def toByteArray: Array[Byte] = {
    val in = input
    var rest = sizeOption.getOrElse(0)
    val arr = new Array[Byte](rest)
    while (rest > 0) {
      val res = in.read(arr, arr.length - rest, rest)
      if (res == -1)
        throw new IOException("read error")
      rest -= res
    }
    in.close()
    arr
  }

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile]

  /** Returns the abstract file in this abstract directory with the specified
   *  name. If there is no such file, returns <code>null</code>. The argument
   *  <code>directory</code> tells whether to look for a directory or
   *  a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile

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
    lookup((f, p, dir) => f.lookupName(p, dir), path, directory)
  }

  /** Return an abstract file that does not check that `path` denotes
   *  an existing file.
   */
  def lookupPathUnchecked(path: String, directory: Boolean): AbstractFile = {
    lookup((f, p, dir) => f.lookupNameUnchecked(p, dir), path, directory)
  }

  private def lookup(getFile: (AbstractFile, String, Boolean) => AbstractFile,
                     path0: String,
                     directory: Boolean): AbstractFile = {
    val separator = java.io.File.separatorChar
    // trim trailing '/'s
    val path: String = if (path0.last == separator) path0 dropRight 1 else path0
    val length = path.length()
    assert(length > 0 && !(path.last == separator), path)
    var file = this
    var start = 0
    while (true) {
      val index = path.indexOf(separator, start)
      assert(index < 0 || start < index, ((path, directory, start, index)))
      val name = path.substring(start, if (index < 0) length else index)
      file = getFile(file, name, if (index < 0) directory else true)
      if ((file eq null) || index < 0) return file
      start = index + 1
    }
    file
  }

  private def fileOrSubdirectoryNamed(name: String, isDir: Boolean): AbstractFile = {
    val lookup = lookupName(name, isDir)
    if (lookup != null) lookup
    else {
      val jfile = new JFile(file, name)
      if (isDir) jfile.mkdirs() else jfile.createNewFile()
      new PlainFile(jfile)
    }
  }

  /**
   * Get the file in this directory with the given name,
   * creating an empty file if it does not already existing.
   */
  def fileNamed(name: String): AbstractFile = {
    assert(isDirectory, "Tried to find '%s' in '%s' but it is not a directory".format(name, path))
    fileOrSubdirectoryNamed(name, false)
  }

  /**
   * Get the subdirectory with a given name, creating it if it
   * does not already exist.
   */
  def subdirectoryNamed(name: String): AbstractFile = {
    assert (isDirectory, "Tried to find '%s' in '%s' but it is not a directory".format(name, path))
    fileOrSubdirectoryNamed(name, true)
  }

  protected def unsupported(): Nothing = unsupported(null)
  protected def unsupported(msg: String): Nothing = throw new UnsupportedOperationException(msg)

  /** Returns the path of this abstract file. */
  override def toString() = path

}
