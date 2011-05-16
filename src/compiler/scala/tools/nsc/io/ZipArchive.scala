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
  def fromPath(path: String): ZipArchive = fromFile(new JFile(path))
  def fromPath(path: Path): ZipArchive = fromFile(path.toFile)

  /**
   * @param   file  a File
   * @return  A ZipArchive if `file` is a readable zip file, otherwise null.
   */
  def fromFile(file: File): ZipArchive = fromFile(file.jfile)
  def fromFile(file: JFile): ZipArchive =
    try   { new FileZipArchive(file) }
    catch { case _: IOException => null }

  /**
   * @param   url  the url of a zip file
   * @return  A ZipArchive backed by the given url.
   */
  def fromURL(url: URL): ZipArchive = new URLZipArchive(url)
}
import ZipArchive._

abstract class ZipArchive(override val file: JFile) extends AbstractFile with Equals {
  self =>

  // The root of this archive, populated lazily.  Implemented by File and URL.
  def root: DirEntry
  // The underlying zip file, or null if that's not what underlies it.
  def zipFile: ZipFile

  override def underlyingSource = Some(this)
  def lookupName(name: String, directory: Boolean) = root.lookupName(name, directory)
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported
  def iterator = root.iterator
  def isDirectory = true
  def create()  = unsupported
  def delete()  = unsupported
  def output    = unsupported

  // Accumulated directories in this zip.
  // Violating my usual rule about vars holding mutable structures because
  // I want to make abundantly certain it can be collected.
  private var dirs: mutable.HashMap[String, DirEntry] = _
  protected def traverseAndClear(body: => Unit): DirEntry = {
    dirs = mutable.HashMap[String, DirEntry]("/" -> new DirEntry("/"))
    try     { body ; dirs("/") }
    finally { dirs.clear() ; dirs = null }
  }

  sealed abstract class Entry(override val container: DirEntry, path: String) extends VirtualFile(baseName(path), path) {
    // have to keep this apparently for compat with sbt's compiler-interface
    final def getArchive: ZipFile = self.zipFile
    final def addToParent() = container.entries(name) = this
    override def underlyingSource = Some(self)
    override def toString = self + "(" + path + ")"
  }
  class DirEntry(override val path: String) extends Entry(getParent(path), path) {
    val entries = mutable.HashMap[String, Entry]()

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
      if (dirs contains parentPath) dirs(parentPath)
      else newDir(parentPath, null)
    }
  }
  protected def getDir(entry: ZipEntry): DirEntry = {
    val name = entry.getName
    if (entry.isDirectory) {
      if (dirs contains name) {
        val existing = dirs(name)
        if (existing.lastModified <= 0)
          existing.lastModified = entry.getTime()
        existing
      }
      else newDir(name, entry)
    }
    else {
      val path = dirName(name)
      if (dirs contains path) dirs(path)
      else newDir(path, null)
    }
  }
}

final class FileZipArchive(file: JFile) extends ZipArchive(file) {
  lazy val root = traverseAndClear(traverseZipFile(zipFile))
  def zipFile = new ZipFile(file)

  def traverseZipFile(z: ZipFile) {
    val enum = z.entries()
    while (enum.hasMoreElements) {
      val zipEntry = enum.nextElement
      if (zipEntry.isDirectory) getDir(zipEntry)
      else new FileEntry(zipEntry).addToParent()
    }
  }
  override def sizeOption = Some(file.length.toInt)

  def name         = file.getName
  def path         = file.getPath
  def input        = File(file).inputStream()
  def lastModified = file.lastModified
  def absolute     = if (file.isAbsolute) this else new FileZipArchive(file.getAbsoluteFile)
  def container    = new PlainFile(file.getParent)

  override def canEqual(other: Any) = other.isInstanceOf[FileZipArchive]
  override def hashCode() = file.hashCode
  override def equals(that: Any) = that match {
    case x: ZipArchive => file.getAbsoluteFile == x.file.getAbsoluteFile
    case _             => false
  }
}

final class URLZipArchive(val url: URL) extends ZipArchive(null) {
  lazy val root = traverseAndClear(traverseStream(input))
  def zipFile = null

  def traverseStream(input: InputStream) {
    val in = new ZipInputStream(new ByteArrayInputStream(Streamable.bytes(input)))

    @annotation.tailrec def loop() {
      val zipEntry = in.getNextEntry()
      if (zipEntry != null) {
        if (zipEntry.isDirectory) getDir(zipEntry)
        else new FileEntry(zipEntry).addToParent()
        in.closeEntry()
        loop()
      }
    }
    try loop()
    finally in.close()
  }

  def name  = url.getFile()
  def path  = url.getPath()
  def input = url.openStream()
  def lastModified =
    try url.openConnection().getLastModified()
    catch { case _: IOException => 0 }
  def absolute  = this
  def container = unsupported

  override def canEqual(other: Any) = other.isInstanceOf[URLZipArchive]
  override def hashCode() = url.hashCode
  override def equals(that: Any) = that match {
    case x: URLZipArchive => url == x.url
    case _                => false
  }
}
