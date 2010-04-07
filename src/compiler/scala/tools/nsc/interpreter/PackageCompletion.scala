/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.net.URL
import java.lang.reflect
import java.util.concurrent.ConcurrentHashMap
import io.{ Path, Directory, File, Streamable }
import scala.tools.util.PathResolver.Defaults.scalaHomeDir
import scala.concurrent.DelayedLazyVal
import scala.reflect.NameTransformer.{ decode, encode }
import PackageCompletion._

/** Completion among all known packages.  It examines the jars in a
 *  separate thread so as not to slow down startup.  If it arrives at
 *  an object, it delegates to StaticCompletion for that object.
 */
class PackageCompletion(classpath: List[URL]) extends CompletionAware {
  // it takes a little while to look through the jars so we use a future and a concurrent map
  class CompletionAgent {
    val dottedPaths: ConcurrentHashMap[String, List[CompletionInfo]] = new ConcurrentHashMap[String, List[CompletionInfo]]
    val topLevelPackages = new DelayedLazyVal(
      () => enumToList(dottedPaths.keys) filterNot (_ contains '.'),
      getDottedPaths(dottedPaths, classpath)
    )
  }
  val agent = new CompletionAgent
  import agent._

  def completions() = topLevelPackages()
  override def follow(id: String) =
    if (dottedPaths containsKey id) Some(new SubCompletor(id))
    else None

  class SubCompletor(root: String) extends CompletionAware {
    // Look for a type alias
    private def aliasCompletor(path: String): Option[CompletionAware] =
      for (name <- ByteCode aliasForType path ; clazz <- classForName(name + "$")) yield
        new StaticCompletion(clazz)

    lazy val pkgObject = classForName(root + ".package$") map (x => new PackageObjectCompletion(x))
    def pkgObjectMembers = pkgObject map (_ completionsFor Parsed("")) getOrElse Nil

    private def infos = Option(dottedPaths get root) getOrElse Nil
    def completions() = {
      val xs = infos map (_.visibleName) filterNot (_ == "package")
      xs ::: pkgObjectMembers
    }

    override def follow(segment: String): Option[CompletionAware] = {
      PackageCompletion.this.follow(root + "." + segment) orElse {
        for (CompletionInfo(`segment`, className) <- infos ; clazz <- classForName(className)) {
          return Some(new StaticCompletion(clazz))
        }

        aliasCompletor(root + "." + segment)
      }
    }
    override def toString = "SubCompletor(%s)" format root
  }
}

object PackageCompletion {
  import java.util.jar.{ JarEntry, JarFile }

  val EXPAND_SEPARATOR_STRING = "$$"
  val ANON_CLASS_NAME = "$anon"
  val TRAIT_SETTER_SEPARATOR_STRING = "$_setter_$"
  val IMPL_CLASS_SUFFIX ="$class"

  def ignoreClassName(x: String) =
    (x contains EXPAND_SEPARATOR_STRING) ||
    (x contains ANON_CLASS_NAME) ||
    (x contains TRAIT_SETTER_SEPARATOR_STRING) ||
    (x endsWith IMPL_CLASS_SUFFIX) ||
    (x matches """.*\$\d+$""")

  def enumToList[T](e: java.util.Enumeration[T]): List[T] = enumToListInternal(e, Nil)
  private def enumToListInternal[T](e: java.util.Enumeration[T], xs: List[T]): List[T] =
    if (e == null || !e.hasMoreElements) xs else enumToListInternal(e, e.nextElement :: xs)

  private def isClass(s: String) = s endsWith ".class"
  private def processNames(xs: List[String]) = xs map (_ dropRight 6) filterNot ignoreClassName distinct

  def getDirClassFiles(dir: Directory): List[String] =
    processNames(dir.deepList() map (dir relativize _ path) filter isClass toList)

  def getJarClassFiles(jar: File): List[String] =
    if (!jar.exists) Nil
    else processNames(enumToList(new JarFile(jar.path).entries) map (_.getName) filter isClass)

  object CompletionInfo {
    def unapply(that: Any) = that match {
      case x: CompletionInfo    => Some((x.visibleName, x.className))
      case _                    => None
    }
  }

  abstract class CompletionInfo {
    def visibleName: String
    def className: String
    def getBytes(): Array[Byte]

    override def hashCode = visibleName.hashCode
    override def equals(other: Any) = other match {
      case x: CompletionInfo  => visibleName == x.visibleName
      case _                  => false
    }
  }

  case class DirCompletionInfo(visibleName: String, className: String, dir: Directory) extends CompletionInfo {
    lazy val file = dir / File(className)

    def getBytes(): Array[Byte] = try file.toByteArray() catch { case _: Exception => Array() }
  }

  case class JarCompletionInfo(visibleName: String, className: String, jar: File) extends CompletionInfo {
    lazy val jarfile = new JarFile(jar.path)
    lazy val entry = jarfile getEntry className

    def getBytes(): Array[Byte] = {
      if (entry == null) Array() else {
        val x = new Streamable.Bytes { def inputStream() = jarfile getInputStream entry }
        x.toByteArray()
      }
    }
  }

  // all the dotted path to classfiles we can find by poking through the jars
  def getDottedPaths(map: ConcurrentHashMap[String, List[CompletionInfo]], classpath: List[URL]): Unit = {
    val cp = classpath.distinct map (x => Path(x.getPath))
    val jars = cp filter (_ hasExtension "jar") map (_.toFile)

    /** If we process all dirs uncritically, someone who has '.' in their classpath and
     *  runs scala from the filesystem root directory will induce a traversal of their
     *  entire filesystem.  We could apply some heuristics to avoid this, but for now we
     *  will look only in the scalaHome directories, which is most of what we want.
     */
    def isUnderScalaHome(d: Directory) = d.parents exists (_ == scalaHomeDir)
    val dirs = cp collect { case x: Directory => x } filter isUnderScalaHome

    // for e.g. foo.bar.baz.C, returns (foo -> bar), (foo.bar -> baz), (foo.bar.baz -> C)
    // and scala.Range$BigInt needs to go scala -> Range -> BigInt
    def subpaths(s: String): List[(String, String)] = {
      val segs = decode(s).split("""[/.]""")
      val components = segs dropRight 1

      (1 to components.length).toList flatMap { i =>
        val k = components take i mkString "."
        if (segs(i) contains "$") {
          val dollarsegs = segs(i).split("$").toList
          for (j <- 1 to (dollarsegs.length - 1) toList) yield {
            val newk = k + "." + (dollarsegs take j mkString ".")
            (k -> dollarsegs(j))
          }
        }
        else List(k -> segs(i))
      }
    }

    def addToMap(key: String, info: CompletionInfo) = {
      if (map containsKey key) {
        val vs = map.get(key)
        if (vs contains info) ()
        else map.put(key, info :: vs)
      }
      else map.put(key, List(info))
    }

    def oneDir(dir: Directory) {
      for (cl <- getDirClassFiles(dir) ; (k, v) <- subpaths(cl))
        addToMap(k, DirCompletionInfo(v, cl, dir))
    }

    def oneJar(jar: File) {
      for (cl <- getJarClassFiles(jar) ; (k, v) <- subpaths(cl))
        addToMap(k, JarCompletionInfo(v, cl, jar))
    }

    jars foreach oneJar
    dirs foreach oneDir
  }
}