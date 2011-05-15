/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package io

import java.net.URL
import java.io.{ IOException, InputStream, ByteArrayInputStream }
import java.util.zip.{ ZipEntry, ZipFile, ZipInputStream }
import scala.collection.{ immutable, mutable }

/** An abstraction for zip files and streams.  Everything is written the way
 *  it is for performance: we come through here a lot on every run.  Be careful
 *  about changing it.
 *
 *  @author  Philippe Altherr (original version)
 *  @author  Paul Phillips (this one)
 *  @version 2.0,
 */
object ZipArchive {
  def fromPath(path: Path): ZipArchive = fromFile(path.toFile)

  /**
   * @param   file  a File
   * @return  A ZipArchive if `file` is a readable zip file, otherwise null.
   */
  def fromFile(file: File): ZipArchive =
    try new FileZipArchive(file, new ZipFile(file.jfile))
    catch { case _: IOException => null }

  /**
   * @param   zipFile  a ZipFile
   * @return  A ZipArchive if `zipFile` is a readable zip file, otherwise null.
   */
  def fromArchive(zipFile: ZipFile): ZipArchive =
    new FileZipArchive(File(zipFile.getName()), zipFile)

  /**
   * @param   url  the url of a zip file
   * @return  A ZipArchive backed by the given url.
   */
  def fromURL(url: URL): ZipArchive = new URLZipArchive(url)
}
import ZipArchive._

trait ZipArchive extends AbstractFile {
  self =>

  // The root of this archive, populated lazily.  Implemented by File and URL.
  def root: DirEntry

  // AbstractFile members with common implementations in File and URL
  override def lookupName(name: String, directory: Boolean) = root.lookupName(name, directory)
  override def lookupNameUnchecked(name: String, directory: Boolean) = unsupported
  override def iterator = root.iterator
  override def isDirectory = true

  // Collected Paths -> DirEntries
  private val dirs = newHashMap[DirEntry]()
  protected def traverseAndClear(body: => Unit): DirEntry = {
    val root = new DirEntry("/")
    dirs("/") = root
    try     { body ; root }
    finally dirs.clear()
  }

  // Override if there's one available
  def zipFile: ZipFile = null

  sealed abstract class Entry(override val container: DirEntry, path: String) extends VirtualFile(baseName(path), path) {
    // have to keep this apparently for compat with sbt's compiler-interface
    final def getArchive: ZipFile = self.zipFile
    final def addToParent() = container.entries(name) = this
    override def underlyingSource = Some(self)
    override def toString = self + "(" + path + ")"
  }
  class DirEntry(override val path: String) extends Entry(getParent(path), path) {
    val entries = newHashMap[Entry]()

    override def isDirectory = true
    override def iterator = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): Entry = {
      if (directory) entries(name + "/")
      else entries(name)
    }
  }
  class FileEntry(zipEntry: ZipEntry) extends Entry(getDir(zipEntry), zipEntry.getName) {
    lastModified = zipEntry.getTime()

    override def input        = getArchive getInputStream zipEntry
    override def sizeOption   = Some(zipEntry.getSize().toInt)
  }

  private def dirName(path: String)  = splitPath(path, true)
  private def baseName(path: String) = splitPath(path, false)
  private def splitPath(path0: String, front: Boolean): String = {
    val isDir = path0.charAt(path0.length - 1) == '/'
    val path  = if (isDir) path0.substring(0, path0.length - 1) else path0
    val idx   = path.lastIndexOf('/')

    if (idx < 0)
      if (front) "/"
      else path
    else
      if (front) path.substring(0, idx + 1)
      else path.substring(idx + 1)
  }

  // Not using withDefault to be certain of performance.
  private def newHashMap[T >: Null]() =
    new mutable.HashMap[String, T] { override def default(key: String) = null }

  private def newDir(path: String, zipEntry: ZipEntry): DirEntry = {
    val dir = new DirEntry(path)
    if (zipEntry != null)
      dir.lastModified = zipEntry.getTime()
    dir.addToParent()
    dirs(path) = dir
    dir
  }
  private def getParent(path: String): DirEntry = {
    if (path == "/") null
    else {
      val parentPath = dirName(path)
      val existing   = dirs(parentPath)
      if (existing != null) existing
      else newDir(parentPath, null)
    }
  }
  protected def getDir(entry: ZipEntry): DirEntry = {
    if (entry.isDirectory) {
      val existing = dirs(entry.getName)
      if (existing != null) {
        if (existing.lastModified < 0)
          existing.lastModified = entry.getTime()

        existing
      }
      else newDir(entry.getName, entry)
    }
    else {
      val path = dirName(entry.getName)
      val existing = dirs(path)
      if (existing != null) existing
      else newDir(path, null)
    }
  }
}

final class FileZipArchive(file: File, override val zipFile: ZipFile) extends PlainFile(file) with ZipArchive {
  def traverseZipFile(z: ZipFile) {
    val enum = z.entries()
    while (enum.hasMoreElements) {
      val zipEntry = enum.nextElement
      if (zipEntry.isDirectory) getDir(zipEntry)
      else new FileEntry(zipEntry).addToParent()
    }
  }
  lazy val root: DirEntry = traverseAndClear(traverseZipFile(zipFile))
}

final class URLZipArchive(url: URL) extends AbstractFile with ZipArchive {
  def traverseStream(input: InputStream) {
    val in = new ZipInputStream(new ByteArrayInputStream(Streamable.bytes(input)))

    @annotation.tailrec def loop() {
      if (in.available != 0) {
        val zipEntry = in.getNextEntry()
        if (zipEntry != null) {
          if (zipEntry.isDirectory) getDir(zipEntry)
          else new FileEntry(zipEntry).addToParent()
          in.closeEntry()
          loop()
        }
      }
    }
    try loop()
    finally in.close()
  }

  lazy val root: DirEntry = traverseAndClear(traverseStream(input))

  def name: String = url.getFile()
  def path: String = url.getPath()
  def input: InputStream = url.openStream()
  def lastModified =
    try url.openConnection().getLastModified()
    catch { case _: IOException => 0 }

  /** Methods we don't support but have to implement because of the design */
  def absolute  = this
  def file      = null
  def create()  = unsupported
  def delete()  = unsupported
  def output    = unsupported
  def container = unsupported
}
