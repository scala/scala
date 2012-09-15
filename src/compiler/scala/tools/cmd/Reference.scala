/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd

import scala.collection.mutable.ListBuffer
import nsc.Properties.envOrNone

/** Mixes in the specification trait and uses the vals therein to
 *  side-effect private accumulators.  From this emerges formatted help,
 *  lists of unary and binary arguments, an apply which can creates
 *  instances of the specification, and etc.
 *
 *  @see    Instance
 */
trait Reference extends Spec {
  lazy val options = new Reference.Accumulators()
  import options._

  def helpMsg     = options.helpMsg
  def propertyArgs: List[String] = Nil

  def isUnaryOption(s: String)  = unary contains toOpt(s)
  def isBinaryOption(s: String) = binary contains toOpt(s)
  def isExpandOption(s: String) = expansionMap contains toOpt(s)
  def isAnyOption(s: String)    = isUnaryOption(s) || isBinaryOption(s) || isExpandOption(s)

  def expandArg(arg: String)      = expansionMap.getOrElse(fromOpt(arg), List(arg))

  protected def help(str: => String)        = addHelp(() => str)

  type ThisCommandLine <: CommandLine

  class SpecCommandLine(args: List[String]) extends CommandLine(Reference.this, args) { }
  protected def creator(args: List[String]): ThisCommandLine
  final def apply(args: String*): ThisCommandLine = creator(propertyArgs ++ args flatMap expandArg)

  type OptionMagic = Opt.Reference
  protected implicit def optionMagicAdditions(name: String) = new Opt.Reference(programInfo, options, name)
}

object Reference {
  val MaxLine = 80

  class Accumulators() {
    private var _help     = new ListBuffer[() => String]
    private var _unary   = List[String]()
    private var _binary  = List[String]()
    private var _expand  = Map[String, List[String]]()

    def helpFormatStr     = "    %-" + longestArg + "s %s"
    def defaultFormatStr  = (" " * (longestArg + 7)) + "%s"

    def addUnary(s: String)   = _unary +:= s
    def addBinary(s: String)  = _binary +:= s

    def addExpand(opt: String, expanded: List[String]) =
      _expand += (opt -> expanded)

    def mapHelp(g: String => String) = {
      val idx = _help.length - 1
      val f = _help(idx)

      _help(idx) = () => g(f())
    }

    def addHelp(f: () => String)      = _help += f
    def addHelpAlias(f: () => String) = mapHelp { s =>
      val str = "alias for '%s'" format f()
      def noHelp = (helpFormatStr.format("", "")).length == s.length
      val str2 = if (noHelp) str else " (" + str + ")"

      s + str2
    }
    def addHelpDefault(f: () => String) = mapHelp { s =>
      val str = "(default: %s)" format f()

      if (s.length + str.length < MaxLine) s + " " + str
      else defaultFormatStr.format(s, str)
    }
    def addHelpEnvDefault(name: String) = mapHelp { s =>
      val line1     = "%s (default: %s)".format(s, name)
      val envNow    = envOrNone(name) map ("'" + _ + "'") getOrElse "unset"
      val line2     = defaultFormatStr.format("Currently " + envNow)

      line1 + "\n" + line2
    }

    lazy val unary          = (_unary ++ _expand.keys).distinct
    lazy val binary         = _binary.distinct
    lazy val all            = unary ++ binary
    lazy val expansionMap   = _expand
    lazy val helpMsg        = _help map (f => f() + "\n") mkString
    lazy val longestArg     = all map (_.length) max
  }
}
