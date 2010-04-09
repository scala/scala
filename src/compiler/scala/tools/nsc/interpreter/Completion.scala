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
import java.lang.reflect
import scala.tools.util.PathResolver
import io.{ Path, Directory }

object Completion {
  // methods to leave out of completion
  val excludeMethods = List("hashCode", "equals", "wait", "notify", "notifyAll")

  // strings to look for an exclude by default
  val excludeStrings = List("$$super", "MODULE$")

  def looksLikeInvocation(code: String) = (
        (code != null)
    &&  (code startsWith ".")
    && !(code == ".")
    && !(code startsWith "./")
    && !(code startsWith "..")
  )

  trait Forwarder extends CompletionAware {
    def forwardTo: Option[CompletionAware]

    override def completions() = forwardTo map (_.completions()) getOrElse Nil
    override def follow(s: String) = forwardTo flatMap (_ follow s)
  }
}
import Completion._

// REPL completor - queries supplied interpreter for valid
// completions based on current contents of buffer.
class Completion(repl: Interpreter) {
  self =>

  private lazy val classPath = repl.compilerClasspath

  // the unqualified vals/defs/etc visible in the repl
  val ids = new IdentCompletion(repl)
  // the top level packages we know about
  val pkgs = new PackageCompletion(classPath)
  // members of Predef
  val predef = new StaticCompletion(classOf[scala.Predef$]) {
    override def filterNotFunction(s: String) = (
      (s contains "2") ||
      (s startsWith "wrap") ||
      (s endsWith "Wrapper") ||
      (s endsWith "Ops")
    )
  }
  // members of scala.*
  val scalalang = new pkgs.SubCompletor("scala") with Forwarder {
    def forwardTo = pkgs follow "scala"
    val arityClasses = {
      val names = List("Tuple", "Product", "Function")
      val expanded = for (name <- names ; index <- 0 to 22 ; dollar <- List("", "$")) yield name + index + dollar

      Set(expanded: _*)
    }

    override def filterNotFunction(s: String) = {
      val simple = s.reverse takeWhile (_ != '.') reverse

      (arityClasses contains simple) ||
      (s endsWith "Exception") ||
      (s endsWith "Error")
    }
  }
  // members of java.lang.*
  val javalang = new pkgs.SubCompletor("java.lang") with Forwarder {
    def forwardTo = pkgs follow "java.lang"
    import reflect.Modifier.isPublic
    private def existsAndPublic(s: String): Boolean = {
      val name = if (s contains ".") s else "java.lang." + s
      val clazz = classForName(name) getOrElse (return false)

      isPublic(clazz.getModifiers)
    }
    override def filterNotFunction(s: String) = {
      (s endsWith "Exception") ||
      (s endsWith "Error") ||
      (s endsWith "Impl") ||
      (s startsWith "CharacterData")
    }
    override def completions() = super.completions() filter existsAndPublic
  }
  val literals = new LiteralCompletion {
    lazy val global = repl.compiler
    val parent = self
  }

  def lastResult = new Forwarder {
    def forwardTo = ids follow repl.mostRecentVar
  }

  def lastResultFor(parsed: Parsed) = {
    /** The logic is a little tortured right now because normally '.' is
     *  ignored as a delimiter, but on .<tab> it needs to be propagated.
     */
    val xs = lastResult completionsFor parsed
    if (parsed.isEmpty) xs map ("." + _) else xs
  }

  // the list of completion aware objects which should be consulted
  val topLevel: List[CompletionAware] = List(ids, pkgs, predef, scalalang, javalang, literals)

  // the first tier of top level objects (doesn't include file completion)
  def topLevelFor(parsed: Parsed) = topLevel flatMap (_ completionsFor parsed)

  // chasing down results which won't parse
  def execute(line: String): Option[Any] = {
    val parsed = Parsed(line)
    def noDotOrSlash = line forall (ch => ch != '.' && ch != '/')

    if (noDotOrSlash) None  // we defer all unqualified ids to the repl.
    else {
      (ids executionFor parsed) orElse
      (pkgs executionFor parsed) orElse
      (FileCompletion executionFor line)
    }
  }

  // override if history is available
  def lastCommand: Option[String] = None

  // jline's entry point
  lazy val jline: ArgumentCompletor =
    returning(new ArgumentCompletor(new JLineCompletion, new JLineDelimiter))(_ setStrict false)

  class JLineCompletion extends Completor {
    // For recording the buffer on the last tab hit
    private var lastTab: (String, String) = (null, null)

    // Does this represent two consecutive tabs?
    def isConsecutiveTabs(buf: String) = (buf, lastCommand orNull) == lastTab

    // verbosity goes up with consecutive tabs
    // TODO - actually implement.
    private var verbosity = 0

    // This is jline's entry point for completion.
    override def complete(buf: String, cursor: Int, candidates: JList[String]): Int = {
      // println("complete: buf = %s, cursor = %d".format(buf, cursor))
      verbosity = if (isConsecutiveTabs(buf)) verbosity + 1 else 0
      lastTab = (buf, lastCommand orNull)

      // we don't try lower priority completions unless higher ones return no results.
      def tryCompletion(p: Parsed, completionFunction: Parsed => List[String]): Option[Int] = {
        completionFunction(p) match {
          case Nil  => None
          case xs   =>
            // modify in place and return the position
            xs foreach (candidates add _)
            Some(p.position)
        }
      }

      // a single dot is special cased to completion on the previous result
      def lastResultCompletion =
        if (!looksLikeInvocation(buf)) None
        else tryCompletion(Parsed.dotted(buf drop 1, cursor), lastResultFor)

      def regularCompletion = tryCompletion(Parsed.dotted(buf, cursor), topLevelFor)
      def fileCompletion    = tryCompletion(Parsed.undelimited(buf, cursor), FileCompletion completionsFor _.buffer)

      (lastResultCompletion orElse regularCompletion orElse fileCompletion) getOrElse cursor
    }
  }
}
