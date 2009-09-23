/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc
package io

import java.net.URL
import java.util.Enumeration
import java.io.{ File => JFile, IOException, InputStream, BufferedInputStream, ByteArrayInputStream }
import java.util.zip.{ ZipEntry, ZipFile, ZipInputStream }
import PartialFunction._

import scala.collection.Traversable
import scala.collection.mutable.{ Map, HashMap }
import scala.collection.JavaConversions.asIterator

/**
 * @author  Philippe Altherr
 * @version 1.0, 23/03/2004
 */
object ZipArchive
{
  def fromPath(path: Path): ZipArchive = fromFile(path.toFile)

  /**
   * If the specified file <code>file</code> exists and is a readable
   * zip archive, returns an abstract file backed by it. Otherwise,
   * returns <code>null</code>.
   *
   * @param file ...
   * @return     ...
   */
  def fromFile(file: File): ZipArchive =
    try new ZipArchive(file, new ZipFile(file.jfile))
    catch { case _: IOException => null }

  /**
   * Returns an abstract directory backed by the specified archive.
   */
  def fromArchive(archive: ZipFile): ZipArchive =
    new ZipArchive(File(archive.getName()), archive)

  /**
   * Returns an abstract directory backed by the specified archive.
   */
  def fromURL(url: URL): AbstractFile = new URLZipArchive(url)

  private[io] class ZipEntryTraversableClass(in: InputStream) extends Traversable[ZipEntry] {
    val zis = new ZipInputStream(in)

    def foreach[U](f: ZipEntry => U) = {
      def loop(x: ZipEntry): Unit = if (x != null) {
        f(x)
        zis.closeEntry()
        loop(zis.getNextEntry())
      }
      loop(zis.getNextEntry())
    }
  }
}

/** This abstraction aims to factor out the common code between
 *  ZipArchive (backed by a zip file) and URLZipArchive (backed
 *  by an InputStream.)
 */
private[io] trait ZipContainer extends AbstractFile
{
  /** Abstract types */
  type SourceType             // InputStream or AbstractFile
  type CreationType           // InputStream or ZipFile
  type ZipTrav = Traversable[ZipEntry] { def zis: ZipInputStream }

  /** Abstract values */
  protected val creationSource: CreationType
  protected val root: DirEntryInterface
  protected def DirEntryConstructor: (AbstractFile, String, String) => DirEntryInterface
  protected def FileEntryConstructor: (SourceType, String, String, ZipEntry) => FileEntryInterface
  protected def ZipTravConstructor: CreationType => ZipTrav

  protected[io] trait EntryInterface extends VirtualFile {
    def name: String
    def path: String
  }

  protected[io] trait DirEntryInterface extends EntryInterface {
    def source: SourceType
    val entries: Map[String, EntryInterface] = new HashMap()
    var entry: ZipEntry = _

    override def input = throw new Error("cannot read directories")
    override def lastModified: Long =
      if (entry ne null) entry.getTime() else super.lastModified

    override def isDirectory = true
    override def iterator: Iterator[AbstractFile] = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): AbstractFile = {
      def slashName = if (directory) name + "/" else name
      entries.getOrElse(slashName, null)
    }
  }

  protected[io] trait FileEntryInterface extends EntryInterface {
    def entry: ZipEntry

    override def lastModified: Long = entry.getTime()
    override def sizeOption = Some(entry.getSize().toInt)
  }

  class ZipRootCreator(f: ZipRootCreator => SourceType) {
    val root = DirEntryConstructor(ZipContainer.this, "<root>", "/")

    // Map from paths to DirEntries
    val dirs = HashMap[String, DirEntryInterface]("/" -> root)
    val traverser = ZipTravConstructor(creationSource)
    private[this] var _parent: DirEntryInterface = _
    def parent = _parent

    def addEntry(entry: ZipEntry) {
      val path = entry.getName
      if (entry.isDirectory) {
        val dir: DirEntryInterface = getDir(dirs, path)
        if (dir.entry == null) dir.entry = entry
      }
      else {
        val (home, name) = splitPath(path)
        _parent = getDir(dirs, home)
        _parent.entries(name) = FileEntryConstructor(f(this), name, path, entry)
      }
    }

    def apply() = {
      traverser foreach addEntry
      root
    }
  }

  protected def splitPath(path: String): (String, String) = {
    (path lastIndexOf '/') match {
      case -1   => ("/", path)
      case idx  => path splitAt (idx + 1)
    }
  }

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

  /** Returns all abstract subfiles of this abstract directory. */
  override def iterator: Iterator[AbstractFile] = root.iterator

  /**
   * Looks up the path in the given map and returns if found.
   * If not present, creates a new DirEntry, adds to both given
   * map and parent.entries, and returns it.
   */
  protected def getDir(dirs: Map[String, DirEntryInterface], path: String): DirEntryInterface =
    dirs.getOrElseUpdate(path, {
      val (home, name) = splitPath(path init)
      val parent = getDir(dirs, home)
      val dir = DirEntryConstructor(parent, name, path)
      parent.entries(name + path.last) = dir
      dir
    })

  override def isDirectory = true
}

/**
 * This class implements an abstract directory backed by a zip
 * archive. We let the encoding be <code>null</code>, because we behave like
 * a directory.
 *
 * @author  Philippe Altherr
 * @version 1.0, 23/03/2004
 */
final class ZipArchive(file: File, val archive: ZipFile) extends PlainFile(file) with ZipContainer
{
  self =>

  type SourceType = AbstractFile
  type CreationType = ZipFile

  protected val creationSource        = archive
  protected lazy val root             = new ZipRootCreator(_.parent)()
  protected def DirEntryConstructor   = new DirEntry(_, _, _)
  protected def FileEntryConstructor  = new FileEntry(_, _, _, _)
  protected def ZipTravConstructor    = zipTraversableFromZipFile _

  abstract class Entry(
    override val container: AbstractFile,
    name: String,
    path: String
  ) extends VirtualFile(name, path)
  {
    final override def path = "%s(%s)".format(self, pathInArchive)
    final def getArchive = self.archive
    def pathInArchive = super.path

    override def hashCode = super.hashCode + container.hashCode
    override def equals(that : Any) =
      super.equals(that) && (cond(that) {
        case e: Entry => container == e.container
      })
  }

  final class DirEntry(
    container: AbstractFile,
    name: String,
    path: String
  ) extends Entry(container, name, path) with DirEntryInterface
  {
    def source = container
  }

  final class FileEntry(
    container: AbstractFile,
    name: String,
    path: String,
    val entry: ZipEntry
  ) extends Entry(container, name, path) with FileEntryInterface
  {
    def archive = self.archive
    override def input = archive getInputStream entry
  }

  private def zipTraversableFromZipFile(z: ZipFile): ZipTrav =
    new Traversable[ZipEntry] {
      def zis: ZipInputStream = null  // not valid for this type
      val itStream = asIterator(z.entries()).toStream
      def foreach[U](f: ZipEntry => U) = itStream foreach f
    }
}

/**
 * This class implements an abstract directory backed by a specified
 * zip archive.
 *
 * @author  Stephane Micheloud
 * @version 1.0, 29/05/2007
 */
final class URLZipArchive(url: URL) extends AbstractFile with ZipContainer
{
  type SourceType   = InputStream
  type CreationType = InputStream

  protected lazy val creationSource = input
  protected lazy val root = new ZipRootCreator(x => byteInputStream(x.traverser.zis))()

  protected def DirEntryConstructor   = (_, name, path) => new DirEntry(name, path)
  protected def FileEntryConstructor  = new FileEntry(_, _, _, _)
  protected def ZipTravConstructor    = new ZipArchive.ZipEntryTraversableClass(_)

  def name: String = url.getFile()
  def path: String = url.getPath()
  def input: InputStream = url.openStream()
  def absolute: AbstractFile = this
  def lastModified: Long =
    try url.openConnection().getLastModified()
    catch { case _: IOException => 0 }

  /** Methods we don't support but have to implement because of the design */
  def file: JFile = null
  def create: Unit = throw new UnsupportedOperationException
  def delete: Unit = throw new UnsupportedOperationException
  def output = throw new Error("unsupported")
  def container = throw new Error("unsupported")

  abstract class Entry(name: String, path: String) extends VirtualFile(name, path) {
    final override def path = "%s(%s)".format(URLZipArchive.this, super.path)
  }
  final class DirEntry(name: String, path: String) extends Entry(name, path) with DirEntryInterface {
    def source = input
  }
  final class FileEntry(
    val in: InputStream,
    name: String,
    path: String,
    val entry: ZipEntry
  ) extends Entry(name, path) with FileEntryInterface
  {
    override def input = in
  }

  /** Private methods **/
  private def byteInputStream(in: InputStream): InputStream = {
    val minusOne = (-1).toByte
    val buf = new BufferedInputStream(in)
    val bytes = Iterator continually in.read().toByte takeWhile (_ != minusOne)
    new ByteArrayInputStream(bytes.toSequence.toArray)
  }
}
