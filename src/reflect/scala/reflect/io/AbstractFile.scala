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

import java.io.{BufferedOutputStream, ByteArrayOutputStream, IOException, InputStream, OutputStream}
import java.io.{File => JFile}
import java.net.URL
import java.nio.ByteBuffer

import scala.collection.AbstractIterable

/**
 * An abstraction over files for use in the reflection/compiler libraries.
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object AbstractFile {
  /** Returns "getFile(new File(path))". */
  def getFile(path: String): AbstractFile = getFile(File(path))
  def getFile(path: Path): AbstractFile = getFile(path.toFile)

  /**
   * If the specified File exists and is a regular file, returns an
   * abstract regular file backed by it. Otherwise, returns `null`.
   */
  def getFile(file: File): AbstractFile =
    if (!file.isDirectory) new PlainFile(file) else null

  /** Returns "getDirectory(new File(path))". */
  def getDirectory(path: Path): AbstractFile = getDirectory(path.toFile)

  /**
   * If the specified File exists and is either a directory or a
   * readable zip or jar archive, returns an abstract directory
   * backed by it. Otherwise, returns `null`.
   */
  def getDirectory(file: File): AbstractFile =
    if (file.isDirectory) new PlainFile(file)
    else if (file.isFile && Path.isExtensionJarOrZip(file.jfile)) ZipArchive.fromFile(file)
    else null

  /**
   * If the specified URL exists and is a regular file or a directory, returns an
   * abstract regular file or an abstract directory, respectively, backed by it.
   * Otherwise, returns `null`.
   */
  def getURL(url: URL): AbstractFile =
    if (url.getProtocol == "file") {
      val f = new java.io.File(url.toURI)
      if (f.isDirectory) getDirectory(f)
      else getFile(f)
    } else null

  def getResources(url: URL): AbstractFile = ZipArchive.fromManifestURL(url)
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
 *   The class `symtab.classfile.AbstractFileReader` accesses
 *   bytes, knowing that the character set of classfiles is UTF-8. For
 *   all other cases, the class `SourceFile` is used, which honors
 *   `global.settings.encoding.value`.
 * </p>
 *
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
abstract class AbstractFile extends AbstractIterable[AbstractFile] {

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
  def exists: Boolean = {
    //if (settings.areStatisticsEnabled) statistics.incCounter(IOStats.fileExistsCount)
    (file eq null) || file.exists
  }

  /** Does this abstract file represent something which can contain classfiles? */
  def isClassContainer = isDirectory || (file != null && (extension == "jar" || extension == "zip"))

  /** Create a file on disk, if one does not exist already. */
  def create(): Unit

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit

  /** Is this abstract file a directory? */
  def isDirectory: Boolean

  /** Does this abstract file correspond to something on-disk? */
  def isVirtual: Boolean = false

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
   *  warning: use `Global.getSourceFile()` to use the proper
   *  encoding when converting to the char array.
   */
  @throws(classOf[IOException])
  def toCharArray = new String(toByteArray).toCharArray

  /** Returns contents of file (if applicable) in a byte array.
   */
  @throws(classOf[IOException])
  def toByteArray: Array[Byte] = {
    val in = input
    sizeOption match {
      case Some(size) =>
        var rest = size
        val arr = new Array[Byte](rest)
        while (rest > 0) {
          val res = in.read(arr, arr.length - rest, rest)
          if (res == -1)
            throw new IOException("read error")
          rest -= res
        }
        in.close()
        arr
      case None =>
        val out = new ByteArrayOutputStream()
        var c = in.read()
        while(c != -1) {
          out.write(c)
          c = in.read()
        }
        in.close()
        out.toByteArray()
    }
  }
  def toByteBuffer: ByteBuffer = ByteBuffer.wrap(toByteArray)

  /** Returns the context of this file (if applicable) in a byte array. This array might _not_ be defensively copied. */
  def unsafeToByteArray: Array[Byte] = toByteArray

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile]
  override def isEmpty: Boolean = iterator.isEmpty
  /** Returns the abstract file in this abstract directory with the specified
   *  name. If there is no such file, returns `null`. The argument
   *  `directory` tells whether to look for a directory or
   *  a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile

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
    fileOrSubdirectoryNamed(name, isDir = false)
  }

  /**
   * Get the subdirectory with a given name, creating it if it
   * does not already exist.
   */
  def subdirectoryNamed(name: String): AbstractFile = {
    assert (isDirectory, "Tried to find '%s' in '%s' but it is not a directory".format(name, path))
    fileOrSubdirectoryNamed(name, isDir = true)
  }

  protected def unsupported(): Nothing = unsupported(null)
  protected def unsupported(msg: String): Nothing = throw new UnsupportedOperationException(msg)

  /** Returns the path of this abstract file. */
  override def toString() = path

}
