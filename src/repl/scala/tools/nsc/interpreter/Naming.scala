/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package tools.nsc
package interpreter

import scala.util.Properties.lineSeparator
import scala.util.matching.Regex

/** This is for name logic which is independent of the compiler (notice there's no Global.)
 *  That includes at least generating, metaquoting, mangling, and unmangling.
 */
trait Naming {
  def unmangle(str: String): String = {
    val ESC = '\u001b'
    val cleaned = removeIWPackages(removeLineWrapper(str))
    // Looking to exclude binary data which hoses the terminal, but
    // let through the subset of it we need, like whitespace and also
    // <ESC> for ansi codes.
    val binaryChars = cleaned count (ch => ch < 32 && !ch.isWhitespace && ch != ESC)
    // Lots of binary chars - translate all supposed whitespace into spaces
    // except supposed line endings, otherwise scrubbed lines run together
    if (binaryChars > 5)  // more than one can count while holding a hamburger
      cleaned map {
        case c if lineSeparator contains c  => c
        case c if c.isWhitespace            => ' '
        case c if c < 32                    => '?'
        case c                              => c
      }
    // Not lots - preserve whitespace and ESC
    else
      cleaned map (ch => if (ch.isWhitespace || ch == ESC) ch else if (ch < 32) '?' else ch)
  }

  // The two name forms this is catching are the two sides of this assignment:
  //
  // $line3.$read.$iw.$iw.Bippy =
  //   $line3.$read$$iw$$iw$Bippy@4a6a00ca
  lazy val lineRegex = {
    val sn = sessionNames
    val members = List(sn.read, sn.eval, sn.print) map Regex.quote mkString ("(?:", "|", ")")
    debugging("lineRegex")(Regex.quote(sn.line) + """\d+[./]""" + members + """[$.]""")
  }

  private def removeLineWrapper(s: String) = s.replaceAll(lineRegex, "")
  private def removeIWPackages(s: String)  = s.replaceAll("""\$iw[$.]""", "")

  trait SessionNames {
    // All values are configurable by passing e.g. -Dscala.repl.name.read=XXX
    final def propOr(name: String): String = propOr(name, "$" + name)
    final def propOr(name: String, default: String): String =
      sys.props.getOrElse("scala.repl.name." + name, default)

    // Prefixes used in repl machinery.  Default to $line, $read, etc.
    def line   = propOr("line")
    def read   = propOr("read")
    def eval   = propOr("eval")
    def print  = propOr("print")
    def result = propOr("result")

    // The prefix for unnamed results: by default res0, res1, etc.
    def res   = propOr("res", "res")  // INTERPRETER_VAR_PREFIX
    // Internal ones
    def ires  = propOr("ires")
  }
  lazy val sessionNames: SessionNames = new SessionNames { }

  /** Generates names pre0, pre1, etc. via calls to apply method */
  class NameCreator(pre: String) {
    private var x = -1
    var mostRecent: String = ""

    def apply(): String = {
      x += 1
      mostRecent = pre + x
      mostRecent
    }
    def reset(): Unit = x = -1
    def didGenerate(name: String) =
      (name startsWith pre) && ((name drop pre.length) forall (_.isDigit))
  }

  private lazy val userVar     = new NameCreator(sessionNames.res)  // var name, like res0
  private lazy val internalVar = new NameCreator(sessionNames.ires) // internal var name, like $ires0

  def isUserVarName(name: String)     = userVar didGenerate name
  def isInternalVarName(name: String) = internalVar didGenerate name

  val freshLineId            = {
    var x = 0
    () => { x += 1 ; x }
  }
  def freshUserVarName() = userVar()
  def freshInternalVarName() = internalVar()

  def resetAllCreators() {
    userVar.reset()
    internalVar.reset()
  }

  def mostRecentVar = userVar.mostRecent
}
