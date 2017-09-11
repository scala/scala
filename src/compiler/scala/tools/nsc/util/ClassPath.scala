/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package util

import io.{AbstractFile, Directory, File, Jar}
import java.net.MalformedURLException
import java.net.URL
import java.util.regex.PatternSyntaxException

import File.pathSeparator
import Jar.isJarOrZip
import scala.concurrent.ExecutionContext

/**
  * A representation of the compiler's class- or sourcepath.
  */
trait ClassPath {
  import scala.tools.nsc.classpath._
  def asURLs: Seq[URL]

  /*
   * These methods are mostly used in the ClassPath implementation to implement the `list` and
   * `findX` methods below.
   *
   * However, there are some other uses in the compiler, to implement `invalidateClassPathEntries`,
   * which is used by the repl's `:require` (and maybe the spark repl, https://github.com/scala/scala/pull/4051).
   * Using these methods directly is more efficient than calling `list`.
   *
   * The `inPackage` string is a full package name, e.g. "" or "scala.collection".
   */

  private[nsc] def hasPackage(pkg: String): Boolean
  private[nsc] def packages(inPackage: String): Seq[PackageEntry]
  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry]
  private[nsc] def sources(inPackage: String): Seq[SourceFileEntry]

  /**
   * Returns packages and classes (source or classfile) that are members of `inPackage` (not
   * recursively). The `inPackage` string is a full package name, e.g., "scala.collection".
   *
   * This is the main method uses to find classes, see class `PackageLoader`. The
   * `rootMirror.rootLoader` is created with `inPackage = ""`.
   */
  private[nsc] def list(inPackage: String): ClassPathEntries

  /**
   * Returns the class file and / or source file for a given external name, e.g., "java.lang.String".
   * If there is both a class file and source file, the compiler can decide whether to read the
   * class file or compile the source file.
   *
   * Internally this seems to be used only by `ScriptRunner`, but only to call `.isDefined`. That
   * could probably be implemented differently.
   *
   * Externally, it is used by sbt's compiler interface:
   * https://github.com/sbt/sbt/blob/v0.13.15/compile/interface/src/main/scala/xsbt/CompilerInterface.scala#L249
   * Jason has some improvements for that in the works (https://github.com/scala/bug/issues/10289#issuecomment-310022699)
   */
  def findClass(className: String): Option[ClassRepresentation] = {
    // A default implementation which should be overridden, if we can create the more efficient
    // solution for a given type of ClassPath
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)

    val foundClassFromClassFiles = classes(pkg).find(_.name == simpleClassName)
    def findClassInSources = sources(pkg).find(_.name == simpleClassName)

    foundClassFromClassFiles orElse findClassInSources
  }

  /**
   * Returns the classfile for an external name, e.g., "java.lang.String". This method does not
   * return source files.
   *
   * This method is used by the classfile parser. When parsing a Java class, its own inner classes
   * are entered with a `ClassfileLoader` that parses the classfile returned by this method.
   * It is also used in the backend, by the inliner, to obtain the bytecode when inlining from the
   * classpath. It's also used by scalap.
   */
  def findClassFile(className: String): Option[AbstractFile]

  def asClassPathStrings: Seq[String]

  /** The whole classpath in the form of one String.
    */
  def asClassPathString: String = ClassPath.join(asClassPathStrings: _*)
  // for compatibility purposes
  @deprecated("use asClassPathString instead of this one", "2.11.5")
  def asClasspathString: String = asClassPathString

  /** The whole sourcepath in the form of one String.
    */
  def asSourcePathString: String


  def startInUse(executionContext: ExecutionContext, proactive:Boolean) = ()
  def endInUse() = ()

  /**
    * make some upstream cache valid, and perform to initial work if the underlying data may have changed
    * return value is used by upstream caches to resync
    * @param executionContext for any background work to be done
    * @param proactive a hint as to whether to expend effort
    * @return the time of the last known or possible change.
    */
  def makeCacheValid(executionContext: ExecutionContext, proactive:Boolean):Long = System.nanoTime()
}

object ClassPath {
  val RootPackage = ""

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
      case dir  => dir.filter(_.isClassContainer).map(x => new java.io.File(dir.file, x.name).getPath).toList
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
    import scala.collection.JavaConverters._
    val resources = Thread.currentThread().getContextClassLoader().getResources("META-INF/MANIFEST.MF")
    resources.asScala.filter(_.getProtocol == "jar").toList
  }

  @deprecated("shim for sbt's compiler interface", since = "2.12.0")
  sealed abstract class ClassPathContext

  @deprecated("shim for sbt's compiler interface", since = "2.12.0")
  sealed abstract class JavaContext
}
trait Named {
  def name: String
}

trait ClassRepresentation extends Named {
  def binary: Option[AbstractFile]
  def source: Option[AbstractFile]
}

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class DirectoryClassPath

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class MergedClassPath

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class JavaClassPath
