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

  class ZipFileIterable(z: ZipFile) extends Iterable[ZipEntry] {
    def iterator = new Iterator[ZipEntry] {
      val enum    = z.entries()
      def hasNext = enum.hasMoreElements
      def next    = enum.nextElement
    }
  }
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
  dirs("/") = new RootEntry
  protected def rootDir = dirs("/")

  // Override if there's one available
  def zipFile: ZipFile = null

  sealed abstract class Entry(override val container: DirEntry, path: String) extends VirtualFile(baseName(path), path) {
    // have to keep this apparently for compat with sbt's compiler-interface
    final def getArchive: ZipFile = self.zipFile
    override def underlyingSource = Some(self)
    final override def path = self + "(" + super.path + ")"
  }
  class RootEntry extends DirEntry(rootDir, "/") { }
  class DirEntry(container: DirEntry, path: String) extends Entry(container, path) {
    def this(path: String) = this(getDir(dirName(path)), path)

    val entries = newHashMap[Entry]()

    override def isDirectory = true
    override def iterator = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): Entry = {
      if (directory) entries(name + "/")
      else entries(name)
    }
  }
  class FileEntry(path: String, val zipEntry: ZipEntry) extends Entry(getDir(path), path) {
    override def input        = getArchive getInputStream zipEntry
    override def lastModified = zipEntry.getTime()
    override def sizeOption   = Some(zipEntry.getSize().toInt)
  }

  private def dirName(path: String)  = splitPath(path, true)
  private def baseName(path: String) = splitPath(path, false)
  private def splitPath(path0: String, front: Boolean): String = {
    val start = path0.length - 1
    val path  = if (path0.charAt(start) == '/') path0.substring(0, start) else path0
    var i     = path.length - 1

    while (i >= 0 && path.charAt(i) != '/')
      i -= 1

    if (i < 0)
      if (front) "/"
      else path
    else
      if (front) path.substring(0, i + 1)
      else path.substring(i + 1)
  }

  // Not using withDefault to be certain of performance.
  private def newHashMap[T >: Null]() =
    new mutable.HashMap[String, T] { override def default(key: String) = null }

  /** If the given path entry has already been created, return the DirEntry.
   *  Otherwise create it and update both dirs and the parent's dirEntries.
   *  Not using getOrElseUpdate to be sure of performance.
   */
  private def getDir(path: String): DirEntry = {
    if (dirs contains path) dirs(path)
    else {
      val newEntry = new DirEntry(path)
      newEntry.container.entries(newEntry.name) = newEntry
      dirs(path) = newEntry
      newEntry
    }
  }

  protected final def addZipEntry(entry: ZipEntry) {
    val path = entry.getName
    if (entry.isDirectory) {
      getDir(path)
    }
    else {
      val parent   = getDir(dirName(path))
      val newEntry = new FileEntry(path, entry)

      parent.entries(newEntry.name) = newEntry
    }
  }
}

final class FileZipArchive(file: File, override val zipFile: ZipFile) extends PlainFile(file) with ZipArchive {
  lazy val root: DirEntry = {
    new ZipFileIterable(zipFile) foreach addZipEntry
    rootDir
  }
}

final class URLZipArchive(url: URL) extends AbstractFile with ZipArchive {
  lazy val root: DirEntry = {
    val in = new ZipInputStream(new ByteArrayInputStream(Streamable.bytes(input)))

    @annotation.tailrec def loop() {
      if (in.available != 0) {
        val entry = in.getNextEntry()
        if (entry != null) {
          addZipEntry(entry)
          in.closeEntry()
          loop()
        }
      }
    }
    try loop()
    finally in.close()

    rootDir
  }

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
