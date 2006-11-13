/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$


package scala.tools.nsc.io

import java.io.{File, IOException, InputStream}
import java.util.Enumeration
import java.util.zip.{ZipEntry, ZipFile}

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
    new ZipArchive(new File(archive.getName()), archive);
}

/**
 * This class implements an abstract directory backed by a zip
 * archive. We let the encoding be null, because we behave like
 * a directory
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
  override def elements: Iterator[AbstractFile] = {
    if (root eq null) load()
    root.elements
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
  private def load(): Unit = {
    this.root = new DirEntry("<root>", "/")
    // A path to DirEntry map
    val dirs: Map[String,DirEntry] = new HashMap()
    dirs.update("/", root)
    val entries = archive.entries()
    while (entries.hasMoreElements()) {
      val entry = entries.nextElement().asInstanceOf[ZipEntry]
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
        parent.entries.update(name, new FileEntry(name, path, entry))
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
      case None => {
        val index = path.lastIndexOf('/', path.length() - 2);
        val name = if (index < 0) path else path.substring(index + 1);
        val home = if (index < 0) "/"  else path.substring(0, index + 1);
        val parent: DirEntry = getDir(dirs, home);
        val dir = new DirEntry(name.substring(0, name.length() - 1), path);
        parent.entries.update(name, dir);
        dirs.update(path, dir);
        dir
      }
    }

  //########################################################################
  // Private Class - Entry

  /** Superclass of archive entries */
  abstract class Entry(name: String, path: String)
  extends VirtualFile(name, path) {
    final override def path = ZipArchive.this.toString() + "(" + super.path + ")"
    final def getArchive = ZipArchive.this.archive
  }

  //########################################################################
  // Private Class - DirEntry

  /** A directory archive entry */
  private final class DirEntry(name: String, path: String)
                extends Entry(name, path)
  {

    val entries: Map[String,Entry] = new HashMap()

    var entry: ZipEntry = _

    override def isDirectory = true
    override def read = throw new Error("cannot read directories");

    override def lastModified: Long =
      if (entry ne null) entry.getTime() else super.lastModified

    override def elements: Iterator[AbstractFile] = entries.values

    override def lookupName(name: String, directory: Boolean): AbstractFile =
      entries.get(if (directory) name + "/" else name) match {
        case Some(dir) => dir
        case None => null
      }
  }

  //########################################################################
  // Private Class - FileEntry

  /** A regular file archive entry */
  final class FileEntry(name: String, path: String, val entry: ZipEntry)
        extends Entry(name, path)
  {

    override def lastModified: Long = entry.getTime()

    override def read = archive.getInputStream(entry);

    override def size = Some(entry.getSize().toInt)
  }

}
