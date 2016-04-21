/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package util

import io.{ AbstractFile, Directory, File, Jar }
import java.net.MalformedURLException
import java.net.URL
import java.util.regex.PatternSyntaxException

import File.pathSeparator
import Jar.isJarOrZip

object ClassPath {
  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    /* Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list.filter(x => filt(x.name) && (x.isDirectory || isJarOrZip(x))).map(_.path).toList

    if (pattern == "*") lsDir(Directory("."))
    else if (pattern endsWith wildSuffix) lsDir(Directory(pattern dropRight 2))
    else if (pattern contains '*') {
      try {
        val regexp = ("^" + pattern.replaceAllLiterally("""\*""", """.*""") + "$").r
        lsDir(Directory(pattern).parent, regexp.findFirstIn(_).isDefined)
      }
      catch { case _: PatternSyntaxException => List(pattern) }
    }
    else List(pattern)
  }

  /** Split classpath using platform-dependent path separator */
  def split(path: String): List[String] = (path split pathSeparator).toList.filterNot(_ == "").distinct

  /** Join classpath using platform-dependent path separator */
  def join(paths: String*): String  = paths filterNot (_ == "") mkString pathSeparator

  /** Split the classpath, apply a transformation function, and reassemble it. */
  def map(cp: String, f: String => String): String = join(split(cp) map f: _*)

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) split(path) flatMap expandS
    else split(path)

  /** Expand dir out to contents, a la extdir */
  def expandDir(extdir: String): List[String] = {
    AbstractFile getDirectory extdir match {
      case null => Nil
      case dir  => dir filter (_.isClassContainer) map (x => new java.io.File(dir.file, x.name) getPath) toList
    }
  }

  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  def expandManifestPath(jarPath: String): List[URL] = {
    val file = File(jarPath)
    if (!file.isFile) return Nil

    val baseDir = file.parent
    new Jar(file).classPathElements map (elem =>
      specToURL(elem) getOrElse (baseDir / elem).toURL
    )
  }

  def specToURL(spec: String): Option[URL] =
    try Some(new URL(spec))
    catch { case _: MalformedURLException => None }

  def manifests: List[java.net.URL] = {
    import scala.collection.convert.WrapAsScala.enumerationAsScalaIterator
    Thread.currentThread().getContextClassLoader()
      .getResources("META-INF/MANIFEST.MF")
      .filter(_.getProtocol == "jar").toList
  }

  @deprecated("Shim for sbt's compiler interface", since = "2.12")
  sealed abstract class ClassPathContext

  @deprecated("Shim for sbt's compiler interface", since = "2.12")
  sealed abstract class JavaContext
}

@deprecated("Shim for sbt's compiler interface", since = "2.12")
sealed abstract class DirectoryClassPath

@deprecated("Shim for sbt's compiler interface", since = "2.12")
sealed abstract class MergedClassPath

@deprecated("Shim for sbt's compiler interface", since = "2.12")
sealed abstract class JavaClassPath
