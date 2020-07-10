package xsbt

import java.nio.file.Path
import scala.reflect.io.{ AbstractFile, Directory }

class PlainNioFile(val nioPath: Path) extends AbstractFile {
  import java.nio.file._

  assert(nioPath ne null)

  /** Returns the underlying File if any and null otherwise. */
  override def file: java.io.File =
    try {
      nioPath.toFile
    } catch {
      case _: UnsupportedOperationException => null
    }

  override lazy val canonicalPath = super.canonicalPath

  override def underlyingSource = Some(this)

  private val fpath = nioPath.toAbsolutePath.toString

  /** Returns the name of this abstract file. */
  def name = nioPath.getFileName.toString

  /** Returns the path of this abstract file. */
  def path = nioPath.toString

  /** The absolute file. */
  def absolute = new PlainNioFile(nioPath.toAbsolutePath)

  override def container: AbstractFile = new PlainNioFile(nioPath.getParent)
  override def input = Files.newInputStream(nioPath)
  override def output = Files.newOutputStream(nioPath)
  override def sizeOption = Some(Files.size(nioPath).toInt)
  override def hashCode(): Int = fpath.hashCode()
  override def equals(that: Any): Boolean = that match {
    case x: PlainNioFile => fpath == x.fpath
    case _               => false
  }

  /** Is this abstract file a directory? */
  def isDirectory: Boolean = Files.isDirectory(nioPath)

  /** Returns the time that this abstract file was last modified. */
  def lastModified: Long = Files.getLastModifiedTime(nioPath).toMillis

  /** Returns all abstract subfiles of this abstract directory. */
  def iterator: Iterator[AbstractFile] = {
    try {
      import scala.collection.JavaConverters._
      val it = Files.newDirectoryStream(nioPath).iterator()
      it.asScala.map(new PlainNioFile(_))
    } catch {
      case _: NotDirectoryException => Iterator.empty
    }
  }

  /**
   * Returns the abstract file in this abstract directory with the
   * specified name. If there is no such file, returns null. The
   * argument "directory" tells whether to look for a directory or
   * or a regular file.
   */
  def lookupName(name: String, directory: Boolean): AbstractFile = {
    val child = nioPath.resolve(name)
    if ((Files.isDirectory(child) && directory) || (Files.isRegularFile(child) && !directory))
      new PlainNioFile(child)
    else null
  }

  /** Does this abstract file denote an existing file? */
  def create(): Unit = {
    if (!exists) Files.createFile(nioPath)
    ()
  }

  /** Delete the underlying file or directory (recursively). */
  def delete(): Unit = {
    if (Files.isRegularFile(nioPath)) Files.deleteIfExists(nioPath)
    else if (Files.isDirectory(nioPath)) new Directory(nioPath.toFile).deleteRecursively()
    ()
  }

  /** Returns a plain file with the given name. It does not
   *  check that it exists.
   */
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    new PlainNioFile(nioPath.resolve(name))
}
