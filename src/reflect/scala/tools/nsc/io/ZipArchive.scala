/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package io

import java.net.URL
import java.io.{ IOException, InputStream, ByteArrayInputStream }
import java.io.{ File => JFile }
import java.util.zip.{ ZipEntry, ZipFile, ZipInputStream }
import scala.collection.{ immutable, mutable }
import annotation.tailrec

/** An abstraction for zip files and streams.  Everything is written the way
 *  it is for performance: we come through here a lot on every run.  Be careful
 *  about changing it.
 *
 *  @author  Philippe Altherr (original version)
 *  @author  Paul Phillips (this one)
 *  @version 2.0,
 */
object ZipArchive {
  def fromPath(path: String): FileZipArchive = fromFile(new JFile(path))
  def fromPath(path: Path): FileZipArchive = fromFile(path.toFile)

  /**
   * @param   file  a File
   * @return  A ZipArchive if `file` is a readable zip file, otherwise null.
   */
  def fromFile(file: File): FileZipArchive = fromFile(file.jfile)
  def fromFile(file: JFile): FileZipArchive =
    try   { new FileZipArchive(file) }
    catch { case _: IOException => null }

  /**
   * @param   url  the url of a zip file
   * @return  A ZipArchive backed by the given url.
   */
  def fromURL(url: URL): URLZipArchive = new URLZipArchive(url)
  def fromURL(url: String): URLZipArchive = fromURL(new URL(url))

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
}
import ZipArchive._

abstract class ZipArchive(override val file: JFile) extends AbstractFile with Equals {
  self =>

  override def underlyingSource = Some(this)
  def isDirectory = true
  def lookupName(name: String, directory: Boolean) = unsupported
  def lookupNameUnchecked(name: String, directory: Boolean) = unsupported
  def create()  = unsupported
  def delete()  = unsupported
  def output    = unsupported
  def container = unsupported
  def absolute  = unsupported

  private def walkIterator(its: Iterator[AbstractFile]): Iterator[AbstractFile] = {
    its flatMap { f =>
      if (f.isDirectory) walkIterator(f.iterator)
      else Iterator(f)
    }
  }
  def deepIterator = walkIterator(iterator)

  sealed abstract class Entry(path: String) extends VirtualFile(baseName(path), path) {
    // have to keep this name for compat with sbt's compiler-interface
    def getArchive: ZipFile = null
    override def underlyingSource = Some(self)
    override def toString = self.path + "(" + path + ")"
  }
  class DirEntry(path: String) extends Entry(path) {
    val entries = mutable.HashMap[String, Entry]()

    override def isDirectory = true
    override def iterator: Iterator[Entry] = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): Entry = {
      if (directory) entries(name + "/")
      else entries(name)
    }
  }

  private def ensureDir(dirs: mutable.Map[String, DirEntry], path: String, zipEntry: ZipEntry): DirEntry = {
    dirs.getOrElseUpdate(path, {
      val parent = ensureDir(dirs, dirName(path), null)
      val dir    = new DirEntry(path)
      parent.entries(baseName(path)) = dir
      dir
    })
  }
  protected def getDir(dirs: mutable.Map[String, DirEntry], entry: ZipEntry): DirEntry = {
    if (entry.isDirectory) ensureDir(dirs, entry.getName, entry)
    else ensureDir(dirs, dirName(entry.getName), null)
  }
}

final class FileZipArchive(file: JFile) extends ZipArchive(file) {
  def iterator: Iterator[Entry] = {
    val zipFile = new ZipFile(file)
    val root    = new DirEntry("/")
    val dirs    = mutable.HashMap[String, DirEntry]("/" -> root)
    val enum    = zipFile.entries()

    while (enum.hasMoreElements) {
      val zipEntry = enum.nextElement
      val dir = getDir(dirs, zipEntry)
      if (zipEntry.isDirectory) dir
      else {
        class FileEntry() extends Entry(zipEntry.getName) {
          override def getArchive   = zipFile
          override def lastModified = zipEntry.getTime()
          override def input        = getArchive getInputStream zipEntry
          override def sizeOption   = Some(zipEntry.getSize().toInt)
        }
        val f = new FileEntry()
        dir.entries(f.name) = f
      }
    }

    try root.iterator
    finally dirs.clear()
  }

  def name         = file.getName
  def path         = file.getPath
  def input        = File(file).inputStream()
  def lastModified = file.lastModified

  override def sizeOption = Some(file.length.toInt)
  override def canEqual(other: Any) = other.isInstanceOf[FileZipArchive]
  override def hashCode() = file.hashCode
  override def equals(that: Any) = that match {
    case x: FileZipArchive => file.getAbsoluteFile == x.file.getAbsoluteFile
    case _                 => false
  }
}

final class URLZipArchive(val url: URL) extends ZipArchive(null) {
  def iterator: Iterator[Entry] = {
    val root     = new DirEntry("/")
    val dirs     = mutable.HashMap[String, DirEntry]("/" -> root)
    val in       = new ZipInputStream(new ByteArrayInputStream(Streamable.bytes(input)))

    @tailrec def loop() {
      val zipEntry = in.getNextEntry()
      class EmptyFileEntry() extends Entry(zipEntry.getName) {
        override def toByteArray: Array[Byte] = null
        override def sizeOption = Some(0)
      }
      class FileEntry() extends Entry(zipEntry.getName) {
        override val toByteArray: Array[Byte] = {
          val len    = zipEntry.getSize().toInt
          val arr    = new Array[Byte](len)
          var offset = 0

          def loop() {
            if (offset < len) {
              val read = in.read(arr, offset, len - offset)
              if (read >= 0) {
                offset += read
                loop()
              }
            }
          }
          loop()

          if (offset == arr.length) arr
          else throw new IOException("Input stream truncated: read %d of %d bytes".format(offset, len))
        }
        override def sizeOption = Some(zipEntry.getSize().toInt)
      }

      if (zipEntry != null) {
        val dir = getDir(dirs, zipEntry)
        if (zipEntry.isDirectory)
          dir
        else {
          val f = if (zipEntry.getSize() == 0) new EmptyFileEntry() else new FileEntry()
          dir.entries(f.name) = f
        }
        in.closeEntry()
        loop()
      }
    }

    loop()
    try root.iterator
    finally dirs.clear()
  }

  def name  = url.getFile()
  def path  = url.getPath()
  def input = url.openStream()
  def lastModified =
    try url.openConnection().getLastModified()
    catch { case _: IOException => 0 }

  override def canEqual(other: Any) = other.isInstanceOf[URLZipArchive]
  override def hashCode() = url.hashCode
  override def equals(that: Any) = that match {
    case x: URLZipArchive => url == x.url
    case _                => false
  }
}
