/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc
package io

import scala.io.File
import java.net.URL
import java.util.Enumeration
import java.io.{ File => JFile, IOException, InputStream, BufferedInputStream }
import java.util.zip.{ ZipEntry, ZipFile, ZipInputStream }
import PartialFunction._

import scala.collection.mutable.{ Map, HashMap }
import scala.collection.immutable.{ StringVector => SV }
import scala.collection.JavaConversions.asIterator

/**
 * @author  Philippe Altherr
 * @version 1.0, 23/03/2004
 */
object ZipArchive {

  //########################################################################

  /**
   * ...
   *
   * @param path ...
   *  @return     ...
   */
  def fromPath(path: String): AbstractFile = fromFile(File(path))

  /**
   * If the specified file <code>file</code> exists and is a readable
   * zip archive, returns an abstract file backed by it. Otherwise,
   * returns <code>null</code>.
   *
   * @param file ...
   * @return     ...
   */
  def fromFile(file: File): AbstractFile =
    try { new ZipArchive(file, new ZipFile(file.jfile)) }
    catch { case _: IOException => null }

  /**
   * Returns an abstract directory backed by the specified archive.
   *
   * @param archive ...
   * @return        ...
   */
  def fromArchive(archive: ZipFile): AbstractFile =
    new ZipArchive(File(archive.getName()), archive)

  /**
   * Returns an abstract directory backed by the specified archive.
   *
   * @param url ...
   * @return    ...
   */
  def fromURL(url: URL): AbstractFile =
    new URLZipArchive(url)
}

private[io] trait ZipFileCommon
{
  import SV.{ splitAt }

  protected def splitPath(path: String): (String, String) = {
    (path lastIndexOf '/') match {
      case -1   => ("/", path)
      case idx  => splitAt(path, idx + 1)
    }
  }

  protected def byteInputStream(in: InputStream): InputStream = {
    val buf = new BufferedInputStream(in)
    val bytes = Iterator continually in.read().toByte takeWhile (_ != -1)
    new java.io.ByteArrayInputStream(bytes.toSequence.toArray)
  }

  protected def zipEntryIterator(zis: ZipInputStream): Iterator[ZipEntry] = {
    Iterator continually zis.getNextEntry() takeWhile (_ != null)
  }
}

/**
 * This class implements an abstract directory backed by a zip
 * archive. We let the encoding be <code>null</code>, because we behave like
 * a directory.
 *
 * @author  Philippe Altherr
 * @version 1.0, 23/03/2004
 */
final class ZipArchive(file: File, val archive: ZipFile) extends PlainFile(file) with ZipFileCommon
{
  assert(archive ne null)

  /** The root directory  */
  private lazy val root = {
    val root = new DirEntry(this, "<root>", "/")

    // A path to DirEntry map
    val dirs: Map[String, DirEntry] = new HashMap()
    dirs("/") = root
    val entries = asIterator(archive.entries())

    entries foreach { case entry: ZipEntry =>
      val path = entry.getName
      if (entry.isDirectory) {
        val dir: DirEntry = getDir(dirs, path)
        if (dir.entry == null) dir.entry = entry
      }
      else {
        val (home, name) = splitPath(path)
        val parent: DirEntry = getDir(dirs, home)
        parent.entries.update(name, new FileEntry(parent, name, path, entry))
      }
    }

    root
  }

  /** Returns true. */
  override def isDirectory = true

  /** Returns all abstract subfiles of this abstract directory. */
  override def iterator: Iterator[AbstractFile] = root.iterator

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  override def lookupName(name: String, directory: Boolean): AbstractFile =
    root.lookupName(name, directory)

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  override def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    throw new UnsupportedOperationException()

  /**
   * Lookups the specified table for a DirEntry with the specified
   * path. If successful, returns the found DirEntry. Otherwise
   * creates a new DirEntry, enters it into the table and in the
   * table of its parent ZipDir and returns it.
   */
  private def getDir(dirs: Map[String, DirEntry], path: String): DirEntry =
    (dirs get path) getOrElse {
      val (home, name) = splitPath(SV.dropRight(path, 1))
      val parent: DirEntry = getDir(dirs, home)
      val dir = new DirEntry(parent, name, path)
      parent.entries.update(name + SV.last(path), dir)
      dirs.update(path, dir)
      dir
    }

  //########################################################################
  // Private Class - Entry

  /** Superclass of archive entries */
  abstract class Entry(
    override val container: AbstractFile,
    name: String,
    path: String
  ) extends VirtualFile(name, path)
  {
    final override def path = "%s(%s)".format(ZipArchive.this, pathInArchive)
    final def getArchive = ZipArchive.this.archive
    def pathInArchive = super.path

    override def hashCode = super.hashCode + container.hashCode
    override def equals(that : Any) =
      super.equals(that) && (cond(that) {
        case e: Entry => container == e.container
      })
  }

  //########################################################################
  // Private Class - DirEntry

  /** A directory archive entry */
  private final class DirEntry(
    container: AbstractFile,
    name: String,
    path: String
  ) extends Entry(container, name, path)
  {
    val entries: Map[String, Entry] = new HashMap()
    var entry: ZipEntry = _

    override def isDirectory = true
    override def input = throw new Error("cannot read directories")

    override def lastModified: Long =
      if (entry ne null) entry.getTime() else super.lastModified

    override def iterator: Iterator[AbstractFile] = entries.valuesIterator

    override def lookupName(name: String, directory: Boolean): AbstractFile = {
      def slashName = if (directory) name + "/" else name
      entries.getOrElse(slashName, null)
    }
  }

  /** A regular file archive entry */
  final class FileEntry(
    container: AbstractFile,
    name: String,
    path: String,
    val entry: ZipEntry
  ) extends Entry(container, name, path)
  {
    def archive = ZipArchive.this.archive
    override def lastModified: Long = entry.getTime()
    override def input = archive.getInputStream(entry)
    override def sizeOption = Some(entry.getSize().toInt)
  }
}

/**
 * This class implements an abstract directory backed by a specified
 * zip archive.
 *
 * @author  Stephane Micheloud
 * @version 1.0, 29/05/2007
 */
final class URLZipArchive(url: URL) extends AbstractFile with ZipFileCommon {
  assert(url ne null)

  private lazy val root: DirEntry = {
    val root = new DirEntry("<root>", "/")

    // A path to DirEntry map
    val dirs: Map[String, DirEntry] = new HashMap()
    dirs("/") = root

    val zis = new ZipInputStream(input)
    zipEntryIterator(zis) foreach { case entry =>
      val path = entry.getName()
      assert(entry.isDirectory() == path.endsWith("/"),
             this.toString() + " - " + path);
      if (entry.isDirectory()) {
        val dir: DirEntry = getDir(dirs, path)
        assert(dir.entry eq null, this.toString() + " - " + path)
        dir.entry = entry
      } else {
        val index = path.lastIndexOf('/')
        val name = if (index < 0) path else path.substring(index + 1)
        val home = if (index < 0) "/"  else path.substring(0, index + 1)
        val parent: DirEntry = getDir(dirs, home)
        assert(!parent.entries.contains(path), this.toString() + " - " + path)
        val in = byteInputStream(zis)
        parent.entries.update(name, new FileEntry(name, path, entry, in))
      }
      zis.closeEntry()
    }
    root
  }

  def container = throw new Error("unsupported")

  def name: String = url.getFile()
  def path: String = url.getPath()
  def file: JFile = null
  def absolute: AbstractFile = this
  def isDirectory: Boolean = true

  def lastModified: Long =
    try url.openConnection().getLastModified()
    catch { case _: IOException => 0 }

  def create: Unit = throw new UnsupportedOperationException
  def delete: Unit = throw new UnsupportedOperationException

  def input: InputStream = url.openStream()
  def output = throw new Error("unsupported")

  override def iterator: Iterator[AbstractFile] = root.iterator

  override def lookupName(name: String, directory: Boolean): AbstractFile =
    root.lookupName(name, directory)

  /** Returns an abstract file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    throw new UnsupportedOperationException()

  private def getDir(dirs: Map[String, DirEntry], path: String): DirEntry =
    dirs.get(path) match {
      case Some(dir) => dir
      case None =>
        val index = path.lastIndexOf('/', path.length() - 2)
        val name = if (index < 0) path else path.substring(index + 1)
        val home = if (index < 0) "/"  else path.substring(0, index + 1)
        val parent: DirEntry = getDir(dirs, home)
        val dir = new DirEntry(name.substring(0, name.length() - 1), path)
        parent.entries.update(name, dir)
        dirs.update(path, dir)
        dir
    }

  /** Superclass of archive entries */
  abstract class Entry(name: String, path: String) extends VirtualFile(name, path) {
    final override def path = "%s(%s)".format(URLZipArchive.this, super.path)
  }

  /** A directory archive entry */
  private final class DirEntry(name: String, path: String) extends Entry(name, path) {
    val entries: Map[String, Entry] = new HashMap()
    var entry: ZipEntry = _

    override def isDirectory = true
    override def input = throw new Error("cannot read directories")

    override def lastModified: Long =
      if (entry ne null) entry.getTime() else super.lastModified

    override def iterator: Iterator[AbstractFile] = entries.valuesIterator

    override def lookupName(name: String, directory: Boolean): AbstractFile =
      entries.get(if (directory) name + "/" else name) match {
        case Some(dir) => dir
        case None => null
      }
  }

  final class FileEntry(name: String, path: String,
                        val entry: ZipEntry, val in: InputStream)
        extends Entry(name, path) {
    override def lastModified: Long = entry.getTime()
    override def input = in
    override def sizeOption = Some(entry.getSize().toInt)
  }
}
