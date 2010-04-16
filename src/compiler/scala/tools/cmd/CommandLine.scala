/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package cmd

import scala.collection.mutable.ListBuffer

/** An instance of a command line, parsed according to a Spec.
 */
class CommandLine(val spec: Reference, val originalArgs: List[String]) {
  def this(spec: Reference, line: String) = this(spec, Parser tokenize line)
  def this(spec: Reference, args: Array[String]) = this(spec, args.toList)

  import spec.{ isAnyOption, isUnaryOption, isBinaryOption, isExpandOption }

  def assumeBinary  = true
  def enforceArity  = true
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

    def loop(args: List[String]): Map[String, String] = {
      def residual(xs: List[String]) = { residualBuffer ++= xs ; Map[String, String]() }

      /** Returns Some(List(args)) if this option expands to an
       *  argument list and it's not returning only the same arg.
       */
      def expand(s1: String) = {
        if (isExpandOption(s1)) {
          val s2 = spec expandArg s1
          if (s2 == List(s1)) None
          else Some(s2)
        }
        else None
      }

      /** Assumes known options have all been ruled out already. */
      def isUnknown(opt: String) =
        onlyKnownOptions && (opt startsWith "-") && {
          errorFn("Option '%s' not recognized.".format(opt))
          true
        }

      args match {
        case Nil              => Map()
        case Terminator :: xs => residual(xs)
        case x :: Nil         =>
          expand(x) foreach (exp => return loop(exp))
          if (isBinaryOption(x) && enforceArity)
            errorFn("Option '%s' requires argument, found EOF instead.".format(x))

          if (isUnaryOption(x)) mapForUnary(x)
          else if (isUnknown(x)) Map()
          else residual(args)

        case x1 :: x2 :: xs   =>
          expand(x1) foreach (exp => return loop(exp ++ args.tail))

          if (x2 == Terminator)         mapForUnary(x1) ++ residual(xs)
          else if (isUnaryOption(x1))   mapForUnary(x1) ++ loop(args.tail)
          else if (isBinaryOption(x1))  Map(x1 -> x2) ++ loop(xs)
          else if (isUnknown(x1))       loop(args.tail)
          else                          residual(List(x1)) ++ loop(args.tail)
      }
    }

    (loop(originalArgs), residualBuffer map stripQuotes toList)
  }

  def apply(arg: String)  = argMap(arg)
  def get(arg: String)    = argMap get arg
  def isSet(arg: String)  = argMap contains arg

  def getOrElse(arg: String, orElse: => String) = if (isSet(arg)) apply(arg) else orElse

  override def toString() = argMap.toString + " " + residualArgs.toString
}

object CommandLine {
  def apply(args: List[String], unary: List[String], binary: List[String]) = {
    /** XXX Temporarily assembling a fake spec so we can continue to
     *  do ad-hoc parsing based on a list of unary and binary args.
     *  Should either clean this up or do it differently.
     */
    object NoSpec extends Reference {
      unary foreach (_ --? )
      binary foreach (_ --| )

      protected def creator(args: List[String]) = error("No Spec")
      def programInfo = Spec.Names("", "")
      lazy val referenceSpec = this
    }

    new CommandLine(NoSpec, args)
  }
}

