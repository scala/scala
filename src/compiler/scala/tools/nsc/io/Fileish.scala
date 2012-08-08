/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package io

import java.io.{ InputStream }
import java.util.jar.JarEntry

/** A common interface for File-based things and Stream-based things.
 *  (In particular, io.File and JarEntry.)
 */
class Fileish(val path: Path, val input: () => InputStream) extends Streamable.Chars {
  def inputStream() = input()

  def parent       = path.parent
  def name         = path.name
  def isSourceFile = path.hasExtension("java", "scala")

  private lazy val pkgLines = lines() collect { case x if x startsWith "package " => x stripPrefix "package" trim }
  lazy val pkgFromPath      = parent.path.replaceAll("""[/\\]""", ".")
  lazy val pkgFromSource    = pkgLines map (_ stripSuffix ";") mkString "."

  override def toString = path.path
}

object Fileish {
  def apply(f: File): Fileish = new Fileish(f, () => f.inputStream())
  def apply(f: JarEntry, in: () => InputStream): Fileish  = new Fileish(Path(f.getName), in)
  def apply(path: String, in: () => InputStream): Fileish = new Fileish(Path(path), in)
}
