/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc.io

import java.io.{File, IOException, InputStream}
import java.net.URL
import java.util.Enumeration
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream}

import scala.collection.mutable.{Map, HashMap}

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
  def fromPath(path: String): AbstractFile = fromFile(new File(path))

  /**
   * If the specified file <code>file</code> exists and is a readable
   * zip archive, returns an abstract file backed by it. Otherwise,
   * returns <code>null</code>.
   *
   * @param file ...
   * @return     ...
   */
  def fromFile(file: File): AbstractFile =
    try { new ZipArchive(file, new ZipFile(file)) }
    catch { case _: IOException => null }

  /**
   * Returns an abstract directory backed by the specified archive.
   *
   * @param archive ...
   * @return        ...
   */
  def fromArchive(archive: ZipFile): AbstractFile =
    new ZipArchive(new File(archive.getName()), archive)

  /**
   * Returns an abstract directory backed by the specified archive.
   *
   * @param url ...
   * @return    ...
   */
  def fromURL(url: URL): AbstractFile =
    new URLZipArchive(url)
}

/**
 * This class implements an abstract directory backed by a zip
 * archive. We let the encoding be <code>null</code>, because we behave like
 * a directory.
 *
 * @author  Philippe Altherr
 * @version 1.0, 23/03/2004
 */
final class ZipArchive(file: File, val archive: ZipFile) extends PlainFile(file) {

  assert(archive ne null)
  //########################################################################
  // Private Fields

  /** The root directory or null if not yet initialized */
  private var root: DirEntry = _

  //########################################################################
  // Public Methods

  /** Returns true. */
  override def isDirectory = true

  /** Returns all abstract subfiles of this abstract directory. */
  override def iterator: Iterator[AbstractFile] = {
    if (root eq null) load()
    root.iterator
  }

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  override def lookupName(name: String, directory: Boolean): AbstractFile = {
    if (root eq null) load()
    root.lookupName(name, directory)
  }

  //########################################################################
  // Private Methods

  /** Loads the archive and creates the root directory. */
  private def load() {
    this.root = new DirEntry(this, "<root>", "/")
    // A path to DirEntry map
    val dirs: Map[String, DirEntry] = new HashMap()
    dirs.update("/", root)
    val entries = archive.entries()
    while (entries.hasMoreElements()) {
      val entry = entries.nextElement().asInstanceOf[ZipEntry]
      val path = entry.getName()
      assert(entry.isDirectory() == path.endsWith("/"),
             this.toString() + " - " + path);
      if (entry.isDirectory()) {
        val dir: DirEntry = getDir(dirs, path)
        // this assertion causes an unnecessary bomb if a directory is twice listed in the jar
        // assert(dir.entry eq null, this.toString() + " - " + path)
        if (dir.entry eq null) dir.entry = entry
      } else {
        val index = path.lastIndexOf('/')
        val name = if (index < 0) path else path.substring(index + 1)
        val home = if (index < 0) "/"  else path.substring(0, index + 1)
        val parent: DirEntry = getDir(dirs, home)
        // OLD: assert(!parent.entries.contains(path))
        // MAYBE: assert(!parent.entries.contains(name))
        //if (parent.entries.contains(name))
        //  Console.println("XXX: " + this.toString() + " - " + home + "/" + name)
        parent.entries.update(name, new FileEntry(parent, name, path, entry))
      }
    }
  }

  /**
   * Lookups the specified table for a DirEntry with the specified
   * path. If successful, returns the found DirEntry. Otherwise
   * creates a new DirEntry, enters it into the table and in the
   * table of its parent ZipDir and returns it.
   */
  private def getDir(dirs: Map[String,DirEntry], path: String): DirEntry =
    dirs.get(path) match {
      case Some(dir) => dir
      case None =>
        val index = path.lastIndexOf('/', path.length() - 2);
        val name = if (index < 0) path else path.substring(index + 1);
        val home = if (index < 0) "/"  else path.substring(0, index + 1);
        val parent: DirEntry = getDir(dirs, home);
        val dir = new DirEntry(parent, name.substring(0, name.length() - 1), path);
        parent.entries.update(name, dir);
        dirs.update(path, dir);
        dir
   }

  //########################################################################
  // Private Class - Entry

  /** Superclass of archive entries */
  abstract class Entry(override val container : AbstractFile, name: String, path: String)
    extends VirtualFile(name, path) {
    final override def path = ZipArchive.this.toString() + "(" + pathInArchive + ")"
    final def getArchive = ZipArchive.this.archive
    def pathInArchive = super.path
    override def hashCode = super.hashCode + container.hashCode
    override def equals(that : Any) = super.equals(that) && (that match {
    case entry : Entry => container == entry.container
    case _ => false
    })
  }

  //########################################################################
  // Private Class - DirEntry

  /** A directory archive entry */
  private final class DirEntry(container : AbstractFile, name: String, path: String)
                extends Entry(container, name, path)
  {

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

  //########################################################################
  // Private Class - FileEntry

  /** A regular file archive entry */
  final class FileEntry(container : AbstractFile, name: String, path: String, val entry: ZipEntry)
        extends Entry(container, name, path) {
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
final class URLZipArchive(url: URL) extends AbstractFile {
  assert(url ne null)

  private var root: DirEntry = _

  def container = throw new Error("unsupported")

  def name: String = url.getFile()

  def path: String = url.getPath()

  def file: File = null

  def isDirectory: Boolean = true

  def lastModified: Long =
    try { url.openConnection().getLastModified() }
    catch { case _ => 0 }

  def input: InputStream = url.openStream()

  def output = throw new Error("unsupported")

  override def iterator: Iterator[AbstractFile] = {
    if (root eq null) load()
    root.iterator
  }

  override def lookupName(name: String, directory: Boolean): AbstractFile = {
    if (root eq null) load()
    root.lookupName(name, directory)
  }

  private def load() {
    def getEntryInputStream(in: InputStream): InputStream = {
      val buf = new scala.collection.mutable.ArrayBuffer[Byte]
      val data = new Array[Byte](1024)
      var n = in.read(data)
      while (n > 0) {
        buf.++=(data, 0, n)
        n = in.read(data)
      }
      new java.io.ByteArrayInputStream(buf.toArray)
    }
    this.root = new DirEntry("<root>", "/")
    // A path to DirEntry map
    val dirs: Map[String, DirEntry] = new HashMap()
    dirs.update("/", root)
    val zis = new ZipInputStream(input)
    var entry = zis.getNextEntry()
    while (entry ne null) {
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
        val in = getEntryInputStream(zis)
        parent.entries.update(name, new FileEntry(name, path, entry, in))
      }
      zis.closeEntry()
      entry = zis.getNextEntry()
    }
  }

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
  abstract class Entry(name: String, path: String)
  extends VirtualFile(name, path) {
    final override def path = URLZipArchive.this.toString() + "(" + super.path + ")"
    //final def getArchive = URLZipArchive.this.archive
  }

  /** A directory archive entry */
  private final class DirEntry(name: String, path: String)
  extends Entry(name, path)
  {
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
