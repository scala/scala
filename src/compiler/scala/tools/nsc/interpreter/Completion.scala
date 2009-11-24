/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Paul Phillips
 */
// $Id$

//
// TODO, if practical:
//
// 1) Types: val s: String = x.<tab> should only show members which result in a String.
//      Possible approach: evaluate buffer as if current identifier is
// 2) Implicits: x.<tab> should show not only x's members but those of anything for which
//      there is an implicit conversion from x.
// 3) Chaining: x.foo(bar).<tab> should complete on foo's result type.
// 4) Imports: after import scala.collection.mutable._, HashMap should be among
//      my top level identifiers.
// 5) Caching: it's silly to parse all the jars on every startup, we should have
//      a peristent store somewhere we can write and only check last-mod dates.
// 6) Security: Are we using the filesystem unnecessarily?
//

package scala.tools.nsc
package interpreter

import jline._
import java.net.URL
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.DelayedLazyVal

// REPL completor - queries supplied interpreter for valid completions
// based on current contents of buffer.
class Completion(val interpreter: Interpreter) extends Completor {
  import Completion._
  import java.util.{ List => JList }
  import interpreter.compilerClasspath

  // it takes a little while to look through the jars so we use a future and a concurrent map
  class CompletionAgent {
    val dottedPaths = new ConcurrentHashMap[String, List[String]]
    // TODO - type aliases defined in package objects, like scala.List.<tab>
    // val typeAliases = new ConcurrentHashMap[String, String]
    // val packageObjects = new ConcurrentHashMap[String, List[String]]
    val topLevelPackages = new DelayedLazyVal(
      () => enumToList(dottedPaths.keys) filterNot (_ contains '.'),
      getDottedPaths(dottedPaths, interpreter)
    )
  }
  val agent = new CompletionAgent
  import agent._

  // One instance of a command line
  class Buffer(s: String) {
    val buffer = if (s == null) "" else s
    val segments = buffer.split("\\.", -1).toList
    val lastDot = buffer.lastIndexOf('.')
    val hasDot = segments.size > 0 && segments.last == ""

    // given foo.bar.baz, path = foo.bar and stub = baz
    val (path, stub) = segments.size match {
      case 0    => ("", "")
      case 1    => (segments.head, "")
      case _    => (segments.init.mkString("."), segments.last)
    }

    def filt(xs: List[String]) = xs filter (_ startsWith stub)

    case class Result(candidates: List[String], position: Int) {
      def getCandidates() = (candidates map (_.trim) removeDuplicates) sortWith (_ < _)
    }

    // work out completion candidates and position
    def analyzeBuffer(clist: JList[String]): Result = {
      lazy val ids = idsStartingWith(path)
      lazy val pkgs = pkgsStartingWith(path)
      lazy val count = (ids ::: pkgs).size

      def doSimple(): Result = count match {
        case 0                                    => Result(Nil, 0)
        case 1 if pkgs.size > 0                   => Result(pkgs, 0)
        case 1 if buffer.length < ids.head.length => Result(ids, 0)
        case 1                                    => Result(ids, 0)
          // XXX for now commented out "dot inference" because it's overcomplicated
          // val members = membersOfId(ids.head) filter (_ startsWith stub)
          // if (members.isEmpty) Result(Nil, 0)
          // else Result(members, path.length + 1)
        case _                                    => Result(ids ::: pkgs, 0)
      }

      // a few keywords which don't appear as methods via reflection
      val memberKeywords = List("isInstanceOf", "asInstanceOf")
      def doDotted(): Result = {
        def pkgs = membersOfPath(path)
        def ids = membersOfId(path)
        def idExtras = memberKeywords  // isInstanceOf and asInstanceOf
        def statics = completeStaticMembers(path)
        def pkgMembers = completePackageMembers(path)

        val idList = filt(
          if (!pkgs.isEmpty) pkgs
          else if (!ids.isEmpty) ids ::: idExtras
          else statics ::: idExtras
        ) ::: filt(pkgMembers)

        Result(idList, path.length + 1)
      }

      segments.size match {
        case 0            => Result(Nil, 0)
        case 1            => doSimple()
        case _            => doDotted()
      }
    }

    def getOrElse[K, V](map: ConcurrentHashMap[K, V], key: K, value: => V) =
      if (map containsKey key) map get key
      else value

    def isValidId(s: String) = interpreter.unqualifiedIds contains s
    def membersOfId(s: String) = interpreter membersOfIdentifier s

    def membersOfPath(s: String) = getOrElse(dottedPaths, s, Nil)

    // XXX generalize this to look through imports
    def membersOfScala() = membersOfPath("scala")
    def membersOfJavaLang() = membersOfPath("java.lang")
    def membersOfPredef() = membersOfId("scala.Predef")
    def defaultMembers = {
      val xs = membersOfScala ::: membersOfJavaLang ::: membersOfPredef
      val excludes = List("""Tuple\d+""".r, """Product\d+""".r, """Function\d+""".r,
        """.*Exception$""".r, """.*Error$""".r)
      xs filter (x => excludes forall (r => r.findFirstMatchIn(x).isEmpty))
    }

    def pkgsStartingWith(s: String) = topLevelPackages() filter (_ startsWith s)
    def idsStartingWith(s: String) = (interpreter.unqualifiedIds ::: defaultMembers) filter (_ startsWith s)

    def complete(clist: JList[String]): Int = {
      val res = analyzeBuffer(clist)
      res.getCandidates foreach (clist add _)
      res.position
    }
  }

  import java.lang.reflect.Modifier.{ isPrivate, isProtected, isStatic }
  private def isVisible(x: Int) = !isPrivate(x) && !isProtected(x)
  private def isSingleton(x: Int, isJava: Boolean) = !isJava || isStatic(x)

  private def getMembers(c: Class[_], isJava: Boolean): List[String] =
    c.getMethods.toList .
      filter (x => isVisible(x.getModifiers)) .
      filter (x => isSingleton(x.getModifiers, isJava)) .
      map (_.getName) .
      filter (isValidCompletion)

  private def getClassObject(path: String): Option[Class[_]] =
    (interpreter getClassObject path) orElse
    (interpreter getClassObject ("scala." + path)) orElse
    (interpreter getClassObject ("java.lang." + path))

  // jline's completion comes through here - we ask a Buffer for the candidates.
  override def complete(_buffer: String, cursor: Int, candidates: JList[String]): Int =
    new Buffer(_buffer).complete(candidates)

  def completePackageMembers(path: String): List[String] =
    getClassObject(path + "." + "package") map (getMembers(_, false)) getOrElse Nil

  def completeStaticMembers(path: String): List[String] = {
    // java style, static methods
    val js = getClassObject(path) map (getMembers(_, true)) getOrElse Nil
    // scala style, methods on companion object
    val ss = getClassObject(path + "$") map (getMembers(_, false)) getOrElse Nil

    js ::: ss
  }
}

object Completion
{
  import java.io.File
  import java.util.jar.{ JarEntry, JarFile }

  def enumToList[T](e: java.util.Enumeration[T]): List[T] = enumToList(e, Nil)
  def enumToList[T](e: java.util.Enumeration[T], xs: List[T]): List[T] =
    if (e == null || !e.hasMoreElements) xs else enumToList(e, e.nextElement :: xs)

  // methods to leave out of completion
  val excludeMethods = List("", "hashCode", "equals", "wait", "notify", "notifyAll")

  private def exists(path: String) = new File(path) exists

  def isValidCompletion(x: String) = !(x contains "$$") && !(excludeMethods contains x)
  def isClass(x: String)    = x endsWith ".class"
  def dropClass(x: String)  = x.substring(0, x.length - 6)  // drop .class

  def getClassFiles(path: String): List[String] = {
    if (!exists(path)) return Nil

    enumToList(new JarFile(path).entries) .
      map (_.getName) .
      filter (isClass) .
      map (dropClass) .
      filter (isValidCompletion)
  }

  // all the dotted path to classfiles we can find by poking through the jars
  def getDottedPaths(
    map: ConcurrentHashMap[String, List[String]],
    interpreter: Interpreter): Unit =
  {
    val cp =
      interpreter.compilerClasspath.map(_.getPath) :::            // compiler jars, scala-library.jar etc.
      interpreter.settings.bootclasspath.value.split(':').toList  // boot classpath, java.lang.* etc.

    val jars = cp.removeDuplicates filter (_ endsWith ".jar")

    // for e.g. foo.bar.baz.C, returns (foo -> bar), (foo.bar -> baz), (foo.bar.baz -> C)
    // and scala.Range$BigInt needs to go scala -> Range -> BigInt
    def subpaths(s: String): List[(String, String)] = {
      val segs = s.split('.')
      for (i <- List.range(0, segs.length - 1)) yield {
        val k = segs.take(i+1).mkString(".")
        val v = segs(i+1)
        (k -> v)
      }
    }

    def oneJar(jar: String): Unit = {
      def cleanup(s: String): String = {
        val tr = s map { c => if (c == '/' || c == '$') '.' else c }
        tr stripSuffix "." stripSuffix ".class"
      }
      val classfiles = Completion getClassFiles jar map cleanup

      for (cl <- classfiles.removeDuplicates ; (k, v) <- subpaths(cl)) {
        if (map containsKey k) map.put(k, v :: map.get(k))
        else map.put(k, List(v))
      }
    }

    jars foreach oneJar
  }
}
