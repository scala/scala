/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.nsc
package io

import java.io.{ File => JFile }

object Directory {
  import scala.util.Properties.{ tmpDir, userHome, userDir }

  private def normalizePath(s: String) = Some(apply(Path(s).normalize))
  def Current: Option[Directory]  = if (userDir == "") None else normalizePath(userDir)
  def Home: Option[Directory]     = if (userHome == "") None else normalizePath(userHome)
  def TmpDir: Option[Directory]   = if (tmpDir == "") None else normalizePath(tmpDir)

  def apply(path: Path): Directory = path.toDirectory

  // Like File.makeTemp but creates a directory instead
  def makeTemp(prefix: String = Path.randomPrefix, suffix: String = null, dir: JFile = null): Directory = {
    val path = File.makeTemp(prefix, suffix, dir)
    path.delete()
    path.createDirectory()
  }
}
import Path._

/** An abstraction for directories.
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
class Directory(jfile: JFile) extends Path(jfile) {
  override def toAbsolute: Directory = if (isAbsolute) this else super.toAbsolute.toDirectory
  override def toDirectory: Directory = this
  override def toFile: File = new File(jfile)
  override def isValid = jfile.isDirectory() || !jfile.exists()
  override def normalize: Directory = super.normalize.toDirectory

  /** An iterator over the contents of this directory.
   */
  def list: Iterator[Path] =
    jfile.listFiles match {
      case null   => Iterator.empty
      case xs     => xs.iterator map Path.apply
    }

  def dirs: Iterator[Directory] = list collect { case x: Directory => x }
  def files: Iterator[File] = list collect { case x: File => x }

  override def walkFilter(cond: Path => Boolean): Iterator[Path] =
    list filter cond flatMap (_ walkFilter cond)

  def deepDirs: Iterator[Directory] = Path.onlyDirs(deepList())
  def deepFiles: Iterator[File] = Path.onlyFiles(deepList())

  /** If optional depth argument is not given, will recurse
   *  until it runs out of contents.
   */
  def deepList(depth: Int = -1): Iterator[Path] =
    if (depth < 0) list ++ (dirs flatMap (_ deepList (depth)))
    else if (depth == 0) Iterator.empty
    else list ++ (dirs flatMap (_ deepList (depth - 1)))

  /** An iterator over the directories underneath this directory,
   *  to the (optionally) given depth.
   */
  def subdirs(depth: Int = 1): Iterator[Directory] =
    deepList(depth) collect { case x: Directory => x }
}
