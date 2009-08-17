/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io

import java.io.{ File => JFile }
import collection.Traversable

object Directory
{
  def apply(fileName: String) = new Directory(new JFile(fileName))
  def apply(file: JFile)      = new Directory(file)
  def apply(file: File)       = new Directory(file.file)
}
import Directory._

/** An abstraction for directories.
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
class Directory(val file: JFile) extends collection.Iterable[File]
{
  /** At creation time we enforce that if the path in question
   *  exists, it is a directory.  Obviously you can fool it by
   *  changing the situation after this instance is created, so
   *  don't consider this a lasting guarantee.
   */
  require(!file.exists() || file.isDirectory())

  /** An iterator over the contents of this directory.
   */
  def iterator: Iterator[File] =
    file.listFiles match {
      case null   => Iterator.empty
      case xs     => xs.iterator map File.apply
    }

  /** An iterator over the directories underneath this directory,
   *  to the (optionally) given depth.
   */
  def subdirs(depth: Int = 1): Iterator[Directory] =
    if (depth == 0) Iterator.empty else {
      val (d1, d2) = iterator filter (_.file.isDirectory) map Directory.apply duplicate

      d1 ++ (d2 flatMap (_ subdirs (depth - 1)))
    }

  override def toString() = "Directory(%s)".format(file.getCanonicalPath())
  override def equals(other: Any) = other match {
    case x: Directory => this.file == x.file
    case _            => false
  }
  override def hashCode = file.hashCode
}
