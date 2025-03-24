/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter

import scala.util.matching.Regex

/** This is for name logic which is independent of the compiler (notice there's no Global.)
 *  That includes at least generating, metaquoting, mangling, and unmangling.
 */
object Naming {
  // The CSI pattern matches a subset of the following spec:
  // For CSI, or "Control Sequence Introducer" commands,
  // the ESC [ is followed by any number (including none) of "parameter bytes" in the range 0x30–0x3F (ASCII 0–9:;<=>?),
  // then by any number of "intermediate bytes" in the range 0x20–0x2F (ASCII space and !"#$%&'()*+,-./),
  // then finally by a single "final byte" in the range 0x40–0x7E (ASCII @A–Z[\]^_`a–z{|}~)
  private final val esc = "\u001b"    // "\N{escape}"
  private val csi = raw"$esc\[[0-9;]*([\x40-\x7E])"

  // Matches one of 3 alternatives:
  // group 1 is the CSI command letter, where 'm' is color rendition
  // group 2 is a sequence of chars to be rendered as `?`: anything non-printable and not some space char
  // additional groups are introduced by linePattern but not used
  private lazy val cleaner = raw"$csi|([\p{Cntrl}&&[^\p{Space}]]+)|$linePattern".r

  /** Final pass to clean up REPL output.
   *
   *  Substrings representing REPL artifacts are stripped.
   *
   *  Attempt to replace dangerous characters with '?', which might otherwise
   *  put the terminal in a bad state. Allow SGR (select graphic rendition, "m")
   *  control sequences, but restrict otherwise.
   */
  def unmangle(str: String): String = cleaner.replaceSomeIn(str, clean)

  private def clean(m: Regex.Match): Option[String] =
    if ("m" == m.group(1) ) None
    else if (m.group(1) != null || m.group(2) != null) Some("?" * (m.end - m.start))
    else Some("")

  // Uncompiled regex pattern to detect `line` package and members
  // `read`, `eval`, `print`, for purposes of filtering output and stack traces.
  //
  // The two name forms this is catching are the two sides of this assignment:
  //
  // $line3.$read.$iw.Bippy =
  //   $line3.$read$$iw$$Bippy@4a6a00ca
  //
  // This needs to be aware of LambdaMetafactory generated classnames to strip the correct number of '$' delimiters.
  // A lambda hosted in a module `$iw` (which has a module class `$iw$` is named `$iw$ $ $Lambda1234` (spaces added
  // here for clarification.) This differs from an explicitly declared inner classes named `$Foo`, which would be
  // `$iw$$Foo`.
  //
  // (\Q$line\E\d+(\Q$read\E)?|\Q$read\E(\.INSTANCE)?(\$\Q$iw\E)?|\Q$eval\E|\Q$print\E|\Q$iw\E)(\.this\.|\.|/|\$\$(?=\$Lambda)|\$|$)
  //
  private def linePattern: String = {
    import Regex.{quote => q}
    val sn        = sessionNames
    val lineN     = raw"${q(sn.line)}\d+"
    val lineNRead = raw"$lineN(${q(sn.read)})?"
    val lambda    = """(\.this\.|\.|/|\$\$(?=\$Lambda)|\$|$)"""
    raw"($lineNRead|${q(sn.read)}(\.INSTANCE)?(\$$${q(sn.iw)})?|${q(sn.eval)}|${q(sn.print)}|${q(sn.iw)})$lambda"
  }
  lazy val lineRegex: Regex = linePattern.r

  object sessionNames {
    // All values are configurable by passing e.g. -Dscala.repl.name.read=XXX
    final def propOr(name: String): String = propOr(name, "$" + name)
    final def propOr(name: String, default: String): String =
      sys.props.getOrElse("scala.repl.name." + name, default)

    // Prefixes used in repl machinery.  Default to $line, $read, etc.
    def line = propOr("line")
    def read = "$" + "read"
    def iw = "$" + "iw"
    def eval = propOr("eval")
    def print = propOr("print")
    def result = propOr("result")
    def packageName(lineId: Int) = line + lineId

    /** Create the name for the temp val used in the -Yclass-based REPL wrapper to refer to the state of a previous line. */
    final def lineReadValName(linePackageName: String) = s"${linePackageName}${read}"

    // The prefix for unnamed results: by default res0, res1, etc.
    def res = propOr("res", "res") // INTERPRETER_VAR_PREFIX
    // Internal ones
    def ires = propOr("ires")
  }

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
}

trait Naming {
  import Naming.{NameCreator, sessionNames}
  private lazy val userVar     = new NameCreator(sessionNames.res)  // var name, like res0
  private lazy val internalVar = new NameCreator(sessionNames.ires) // internal var name, like $ires0
  private var _freshLineId = 0

  def isUserVarName(name: String)     = userVar didGenerate name
  def isInternalVarName(name: String) = internalVar didGenerate name

  def freshUserVarName() = userVar()
  def freshInternalVarName() = internalVar()
  def freshLineId() = { _freshLineId += 1 ; _freshLineId}

  def resetAllCreators(): Unit = {
    userVar.reset()
    internalVar.reset()
    _freshLineId = 0
  }

  def mostRecentVar = userVar.mostRecent
}
