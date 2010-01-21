/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
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
import java.lang.reflect
import java.util.{ List => JList }
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.DelayedLazyVal
import scala.collection.mutable.HashSet
import scala.util.NameTransformer.{ decode, encode }

// REPL completor - queries supplied interpreter for valid completions
// based on current contents of buffer.
class Completion(
  val interpreter: Interpreter,
  val intLoop: InterpreterLoop)
extends Completor {
  def this(interpreter: Interpreter) = this(interpreter, null)

  import Completion._
  import interpreter.compilerClasspath

  // it takes a little while to look through the jars so we use a future and a concurrent map
  class CompletionAgent {
    val dottedPaths = new ConcurrentHashMap[String, List[CompletionInfo]]
    val topLevelPackages = new DelayedLazyVal(
      () => enumToList(dottedPaths.keys) filterNot (_ contains '.'),
      getDottedPaths(dottedPaths, interpreter)
    )
  }
  val agent = new CompletionAgent
  import agent._

  import reflect.Modifier.{ isPrivate, isProtected, isPublic, isStatic }
  private def isSingleton(x: Int, isJava: Boolean) = !isJava || isStatic(x)
  private def existsAndPublic(s: String): Boolean =
    (dottedPaths containsKey s) || {
      val clazz =
        try Class.forName(s)
        catch { case _: ClassNotFoundException | _: SecurityException => return false }

      isPublic(clazz.getModifiers)
    }

  // One instance of a command line
  class Buffer(s: String, verbose: Boolean) {
    val buffer = if (s == null) "" else s
    def isEmptyBuffer = buffer == ""

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

      def doDotted(): Result = {
        def pkgs = membersOfPath(path)
        def ids = membersOfId(path)
        def idExtras = List("isInstanceOf", "asInstanceOf", "toString")
        def statics = completeStaticMembers(path)
        def pkgMembers = completePackageMembers(path)

        def calcList = if (pkgs.isEmpty) ids ::: idExtras ::: statics else pkgs
        def idList = filt(calcList ::: pkgMembers)

        Result(idList.removeDuplicates, path.length + 1)
      }

      segments.size match {
        case 0            => Result(Nil, 0)
        case 1            => doSimple()
        case _            => doDotted()
      }
    }

    def isValidId(s: String) = interpreter.unqualifiedIds contains s
    def membersOfId(s: String) = interpreter membersOfIdentifier s
    def membersOfPath(s: String) = {
      val xs =
        if (dottedPaths containsKey s) dottedPaths get s map (_.visibleName)
        else Nil

      s match {
        case "scala"      => xs filterNot scalaToHide
        case "java.lang"  => xs filterNot javaLangToHide
        case _            => xs
      }
    }
    def membersOfPredef() = membersOfId("scala.Predef")

    def javaLangToHide(s: String) = (
      (s endsWith "Exception") ||
      (s endsWith "Error") ||
      (s endsWith "Impl") ||
      (s startsWith "CharacterData") ||
      !existsAndPublic("java.lang." + s)
    )

    def scalaToHide(s: String) =
      (List("Tuple", "Product", "Function") exists (x => (x + """\d+""").r findPrefixMatchOf s isDefined)) ||
      (List("Exception", "Error") exists (s endsWith _))

    /** Hide all default members not verbose */
    def defaultMembers =
      if (verbose) (List("scala", "java.lang") flatMap membersOfPath) ::: membersOfPredef
      else Nil

    def pkgsStartingWith(s: String) = topLevelPackages() filter (_ startsWith s)
    def idsStartingWith(s: String) = {
      // only print res* when verbose
      val unqIds =
        if (verbose) interpreter.unqualifiedIds
        else interpreter.unqualifiedIds filterNot (_ startsWith INTERPRETER_VAR_PREFIX)

      (unqIds ::: defaultMembers) filter (_ startsWith s)
    }

    def complete(clist: JList[String]): Int = {
      val res = analyzeBuffer(clist)
      res.getCandidates foreach (x => clist add decode(x))
      res.position
    }
  }

  private def getMembers(c: Class[_], isJava: Boolean): List[String] =
    c.getMethods.toList .
      filter (x => isPublic(x.getModifiers)) .
      filter (x => isSingleton(x.getModifiers, isJava)) .
      map (_.getName) .
      filterNot (shouldHide)

  private def getClassObject(path: String): Option[Class[_]] =
    (interpreter getClassObject path) orElse
    (interpreter getClassObject ("scala." + path)) orElse
    (interpreter getClassObject ("java.lang." + path))

  def lastHistoryItem = Option(intLoop) map (_.historyList.last)

  // For recording the buffer on the last tab hit
  private var lastTab: (String, String) = (null, null)

  // Does this represent two consecutive tabs?
  def isConsecutiveTabs(buf: String) = (buf, lastHistoryItem orNull) == lastTab

  // jline's completion comes through here - we ask a Buffer for the candidates.
  override def complete(_buffer: String, cursor: Int, candidates: JList[String]): Int = {
    // println("_buffer = %s, cursor = %d".format(_buffer, cursor))
    val verbose = isConsecutiveTabs(_buffer)
    lastTab = (_buffer, lastHistoryItem orNull)

    new Buffer(_buffer, verbose) complete candidates
  }

  def completePackageMembers(path: String): List[String] =
    getClassObject(path + "." + "package") map (getMembers(_, false)) getOrElse Nil

  def completeStaticMembers(path: String): List[String] = {
    // java style, static methods
    val js = getClassObject(path) map (getMembers(_, true)) getOrElse Nil
    // scala style, methods on companion object
    // if getClassObject fails, see if there is a type alias
    val clazz = getClassObject(path + "$") orElse {
      (ByteCode aliasForType path) flatMap (x => getClassObject(x + "$"))
    }
    val ss = clazz map (getMembers(_, false)) getOrElse Nil

    js ::: ss
  }
}

object Completion
{
  import java.io.File
  import java.util.jar.{ JarEntry, JarFile }
  import scala.tools.nsc.io.Streamable

  val EXPAND_SEPARATOR_STRING = "$$"
  val ANON_CLASS_NAME = "$anon"
  val TRAIT_SETTER_SEPARATOR_STRING = "$_setter_$"
  val IMPL_CLASS_SUFFIX ="$class"
  val INTERPRETER_VAR_PREFIX = "res"

  /** Interface for objects which supply their own completion contents.
   */
  trait Special {
    def tabCompletions(): List[String]
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

  def enumToList[T](e: java.util.Enumeration[T]): List[T] = enumToList(e, Nil)
  def enumToList[T](e: java.util.Enumeration[T], xs: List[T]): List[T] =
    if (e == null || !e.hasMoreElements) xs else enumToList(e, e.nextElement :: xs)

  // methods to leave out of completion
  val excludeMethods = List("", "hashCode", "equals", "wait", "notify", "notifyAll")

  private def exists(path: String) = new File(path) exists

  def shouldHide(x: String) =
    (excludeMethods contains x) ||
    (x contains ANON_CLASS_NAME) ||
    (x contains TRAIT_SETTER_SEPARATOR_STRING) ||
    (x endsWith IMPL_CLASS_SUFFIX)

  def getClassFiles(path: String): List[String] = {
    if (!exists(path)) return Nil

    (enumToList(new JarFile(path).entries) map (_.getName)) .
      partialMap { case x: String if x endsWith ".class" => x dropRight 6 } .
      filterNot { shouldHide }
  }

  // all the dotted path to classfiles we can find by poking through the jars
  def getDottedPaths(
    map: ConcurrentHashMap[String, List[CompletionInfo]],
    interpreter: Interpreter): Unit =
  {
    val cp =
      interpreter.compilerClasspath.map(_.getPath) :::            // compiler jars, scala-library.jar etc.
      interpreter.settings.bootclasspath.value.split(':').toList  // boot classpath, java.lang.* etc.

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
      val classfiles = Completion getClassFiles jar

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

  /** The methods below this point exist to simplify repl-generated code.
   */

  // XXX at the moment this is imperfect because scala's protected semantics
  // differ from java's, so protected methods appear public via reflection;
  // yet scala enforces the protection.  The result is that protected members
  // appear in completion yet cannot actually be called.  Fixing this
  // properly requires a scala.reflect.* API.  Fixing it uglily is possible
  // too (cast to structural type!) but I deem poor use of energy.
  private def skipModifiers(m: reflect.Method) = {
    import java.lang.reflect.Modifier._
    val flags = STATIC | PRIVATE | PROTECTED
    (m.getModifiers & flags) == 0
  }
  private def getAnyClass(x: Any): Class[_] = x.asInstanceOf[AnyRef].getClass

  def methodsOf(target: Any): List[String] =
    getAnyClass(target).getMethods filter skipModifiers map (_.getName) toList

  // getAnyClass(target).getInterfaces exists (_ == specialClazz)
  // private val specialClazz = classOf[Special]

  def selfDefinedMembers(target: Any) = target match {
    case x: Special => Some(x.tabCompletions())
    case _          => None
  }
}
