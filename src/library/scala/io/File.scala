/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.io

import java.io.{ FileInputStream, FileOutputStream, File => JFile }

object File
{
  final val extensionRegex = """^.*\.([^.]+)$""".r
  implicit def fileWrapper(file: JFile): File = new File(file)

  def apply(fileName: String) = new File(new JFile(fileName))
  def apply(file: JFile)      = new File(file)
}
import File._

/** An abstraction for files.  For now it is little more than a wrapper
 *  around java.io.File.
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
class File(val file: JFile) extends collection.Iterable[File]
{
  /** If file is a directory, an iterator over its contents.
   *  If not, an empty iterator.
   */
  def iterator: Iterator[File] =
    if (file.isDirectory) file.listFiles.iterator map (x => new File(x))
    else Iterator.empty

	/** Deletes the file or directory recursively. Returns false if it failed.
	 *  Use with caution!
	 */
	def deleteRecursively(): Boolean = deleteRecursively(file)
	private def deleteRecursively(f: JFile): Boolean = {
	  if (f.isDirectory) file.listFiles match {
	    case null =>
	    case xs   => xs foreach deleteRecursively
    }
    f.delete()
  }

	/** Obtains an InputStream. */
	def inputStream = new FileInputStream(file)

	/** Obtains a OutputStream. */
	def outputStream(append: Boolean = false) = new FileOutputStream(file, append)

	/** Attempts to return the file extension. */
	def extension = file.getName match {
	  case extensionRegex(x)  => Some(x)
	  case _                  => None
  }

  /** Creates a Source from this file. */
  def toSource = Source.fromFile(file)()

  /** Creates a new File with the specified path appended. */
	def /(child: String) = new File(new JFile(file, child))

	override def toString() = file.toString
	override def equals(other: Any) = other match {
	  case x: File    => this.file == x.file
	  case _          => false
  }
  override def hashCode = file.hashCode
}
