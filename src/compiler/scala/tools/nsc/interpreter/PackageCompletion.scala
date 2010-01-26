/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.net.URL
import java.lang.reflect
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.DelayedLazyVal
import scala.util.NameTransformer.{ decode, encode }
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

    lazy val pkgObject = classForName(root + ".package$") map (x => new InstanceCompletion(x))
    def pkgObjectMembers = pkgObject map (_.completions) getOrElse Nil

    private def infos = Option(dottedPaths get root) getOrElse Nil
    def completions() = {
      val xs = infos map (_.visibleName) filterNot (_ == "package")
      xs ::: pkgObjectMembers
    }

    override def follow(segment: String): Option[CompletionAware] = {
      PackageCompletion.this.follow(root + "." + segment) orElse {
        for (CompletionInfo(`segment`, className, _) <- infos) {
          return Some(new StaticCompletion(className))
        }

        aliasCompletor(root + "." + segment)
      }
    }
  }
}

object PackageCompletion {
  import java.io.File
  import java.util.jar.{ JarEntry, JarFile }
  import scala.tools.nsc.io.Streamable

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

  def getClassFiles(path: String): List[String] = {
    def exists(path: String) = { new File(path) exists }
    if (!exists(path)) Nil else {
      (enumToList(new JarFile(path).entries) map (_.getName))
      . partialMap { case x: String if x endsWith ".class" => x dropRight 6 }
      . filterNot { ignoreClassName }
    }
  }

  case class CompletionInfo(visibleName: String, className: String, jar: String) {
    lazy val jarfile = new JarFile(jar)
    lazy val entry = jarfile getEntry className

    override def hashCode = visibleName.hashCode
    override def equals(other: Any) = other match {
      case x: CompletionInfo  => visibleName == x.visibleName
      case _                  => false
    }

    def getBytes(): Array[Byte] = {
      if (entry == null) Array() else {
        val x = new Streamable.Bytes { def inputStream() = jarfile getInputStream entry }
        x.toByteArray()
      }
    }
  }

  // all the dotted path to classfiles we can find by poking through the jars
  def getDottedPaths(map: ConcurrentHashMap[String, List[CompletionInfo]], classpath: List[URL]): Unit = {
    val cp = classpath map (_.getPath)
    val jars = cp.removeDuplicates filter (_ endsWith ".jar")

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

    def oneJar(jar: String): Unit = {
      val classfiles = getClassFiles(jar)

      for (cl <- classfiles.removeDuplicates ; (k, _v) <- subpaths(cl)) {
        val v = CompletionInfo(_v, cl, jar)

        if (map containsKey k) {
          val vs = map.get(k)
          if (vs contains v) ()
          else map.put(k, v :: vs)
        }
        else map.put(k, List(v))
      }
    }

    jars foreach oneJar
  }
}