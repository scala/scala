/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package nsc.util

import scala.collection.mutable.ListBuffer

/**
 *  XXX Note this has been completely obsolesced by scala.tools.cmd.
 *  I checked it back in as part of rolling partest back a month
 *  rather than go down the rabbit hole of unravelling dependencies.
 */
case class CommandLine(
  args: List[String],
  unaryArguments: List[String],
  binaryArguments: List[String]
) {
  def this(args: List[String]) = this(args, Nil, Nil)
  def this(args: Array[String]) = this(args.toList, Nil, Nil)
  def this(line: String) = this(cmd.CommandLineParser tokenize line, Nil, Nil)

  def withUnaryArgs(xs: List[String]) = copy(unaryArguments = xs)
  def withBinaryArgs(xs: List[String]) = copy(binaryArguments = xs)

  def assumeBinary = true
  def enforceArity = true
  def onlyKnownOptions = false

  val Terminator = "--"
  val ValueForUnaryOption = "true"  // so if --opt is given, x(--opt) = true

  def mapForUnary(opt: String) = Map(opt -> ValueForUnaryOption)
  def errorFn(msg: String) = println(msg)

  /** argMap is option -> argument (or "" if it is a unary argument)
   *  residualArgs are what is left after removing the options and their args.
   */
  lazy val (argMap, residualArgs) = {
    val residualBuffer = new ListBuffer[String]

    def stripQuotes(s: String) = {
      def isQuotedBy(c: Char) = s.length > 0 && s.head == c && s.last == c
      if (List('"', '\'') exists isQuotedBy) s.tail.init else s
    }

    def isValidOption(s: String) = !onlyKnownOptions || (unaryArguments contains s) || (binaryArguments contains s)
    def isOption(s: String) = (s startsWith "-") && (isValidOption(s) || { unknownOption(s) ; false })
    def isUnary(s: String) = isOption(s) && (unaryArguments contains s)
    def isBinary(s: String) = isOption(s) && !isUnary(s) && (assumeBinary || (binaryArguments contains s))

    def unknownOption(opt: String) =
      errorFn("Option '%s' not recognized.".format(opt))
    def missingArg(opt: String, what: String) =
      errorFn("Option '%s' requires argument, found %s instead.".format(opt, what))

    def loop(args: List[String]): Map[String, String] = {
      def residual(xs: List[String]) = { residualBuffer ++= xs ; Map[String, String]() }
      if (args.isEmpty) return Map()
      val hd :: rest = args
      if (rest.isEmpty) {
        if (isBinary(hd) && enforceArity)
          missingArg(hd, "EOF")

        if (isOption(hd)) mapForUnary(hd) else residual(args)
      }
      else
        if (hd == Terminator) residual(rest)
      else {
        val hd1 :: hd2 :: rest = args

        if (hd2 == Terminator) mapForUnary(hd1) ++ residual(rest)
        else if (isUnary(hd1)) mapForUnary(hd1) ++ loop(hd2 :: rest)
        else if (isBinary(hd1)) {
          // Disabling this check so
          //  --scalacopts "-verbose" works.  We can't tell if it's quoted,
          // the shell does us in.
          //
          // if (isOption(hd2) && enforceArity)
          //   missingArg(hd1, hd2)

          Map(hd1 -> hd2) ++ loop(rest)
        }
        else { residual(List(hd1)) ++ loop(hd2 :: rest) }
      }
    }

    (loop(args), residualBuffer map stripQuotes toList)
  }

  def isSet(arg: String) = args contains arg
  def get(arg: String) = argMap get arg
  def apply(arg: String) = argMap(arg)

  override def toString() = "CommandLine(\n%s)\n" format (args map ("  " + _ + "\n") mkString)
}
