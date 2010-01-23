/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

//
// TODO, if practical:
//
// 1) Types: val s: String = x.<tab> should only show members which result in a String.
//      Possible approach: evaluate buffer as if current identifier is
// 2) Implicits: x.<tab> should show not only x's members but those of anything for which
//      there is an implicit conversion from x.
// 4) Imports: after import scala.collection.mutable._, HashMap should be among
//      my top level identifiers.
// 5) Caching: parsing the jars every startup seems wasteful, but experimentally
//      there is little to no gain from caching.

package scala.tools.nsc
package interpreter

import jline._
import java.net.URL
import java.util.{ List => JList }

// REPL completor - queries supplied interpreter for valid
// completions based on current contents of buffer.
class Completion(repl: Interpreter) extends Completor {
  private def asURLs(xs: List[String]) = xs map (x => io.File(x).toURL)
  private def classPath = (
    // compiler jars, scala-library.jar etc.
    (repl.compilerClasspath) :::
    // boot classpath, java.lang.* etc.
    (asURLs(repl.settings.bootclasspath.value split ':' toList))
  )

  // the unqualified vals/defs/etc visible in the repl
  val ids = new IdentCompletion(repl)
  // the top level packages we know about
  val pkgs = new PackageCompletion(classPath)

  // TODO - restore top level availability of scala.* java.lang.* scala.Predef
  // Old code:
  //
  // def membersOfPredef() = membersOfId("scala.Predef")
  //
  // def javaLangToHide(s: String) = (
  //   (s endsWith "Exception") ||
  //   (s endsWith "Error") ||
  //   (s endsWith "Impl") ||
  //   (s startsWith "CharacterData") ||
  //   !existsAndPublic("java.lang." + s)
  // )
  //
  // def scalaToHide(s: String) =
  //   (List("Tuple", "Product", "Function") exists (x => (x + """\d+""").r findPrefixMatchOf s isDefined)) ||
  //   (List("Exception", "Error") exists (s endsWith _))
  //
  // import reflect.Modifier.{ isPrivate, isProtected, isPublic, isStatic }
  // private def isSingleton(x: Int, isJava: Boolean) = !isJava || isStatic(x)
  // private def existsAndPublic(s: String): Boolean =
  //   (dottedPaths containsKey s) || {
  //     val clazz =
  //       try Class.forName(s)
  //       catch { case _: ClassNotFoundException | _: SecurityException => return false }
  //
  //     isPublic(clazz.getModifiers)
  //   }

  // the high level analysis
  def analyze(_buffer: String, clist: JList[String]): Int = {
    val parsed = new Parsed(_buffer)
    import parsed._

    val candidates = List(ids, pkgs) flatMap (_ completionsFor buffer)
    candidates foreach (clist add _)
    position
  }

  // chasing down results which won't parse
  def execute(line: String): Option[Any] = {
    val parsed = new Parsed(line)
    import parsed._

    if (!isQualified)
      return None

    for (topLevel <- List(ids, pkgs) ; exec <- topLevel executionFor buffer)
      return Some(exec)

    None
  }

  // override if history is available
  def lastCommand: Option[String] = None

  // For recording the buffer on the last tab hit
  private var lastTab: (String, String) = (null, null)

  // Does this represent two consecutive tabs?
  def isConsecutiveTabs(buf: String) = (buf, lastCommand orNull) == lastTab

  // This is jline's entry point for completion.
  override def complete(_buffer: String, cursor: Int, candidates: JList[String]): Int = {
    // println("_buffer = %s, cursor = %d".format(_buffer, cursor))
    val verbose = isConsecutiveTabs(_buffer)
    lastTab = (_buffer, lastCommand orNull)

    // modify the buffer in place and returns the cursor position
    analyze(_buffer, candidates)
  }
}
