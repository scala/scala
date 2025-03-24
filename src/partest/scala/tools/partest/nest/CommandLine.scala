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

package scala.tools.partest.nest

import scala.collection.mutable.ListBuffer
import scala.sys.process.Parser.tokenize

trait CommandLineConfig {
  def enforceArity: Boolean = true
  def onlyKnownOptions: Boolean = true
}

/** An instance of a command line, parsed according to a Spec.
 */
class CommandLine(val spec: Reference, val originalArgs: List[String]) extends CommandLineConfig {
  def this(spec: Reference, line: String) = this(spec, tokenize(line))
  def this(spec: Reference, args: Array[String]) = this(spec, args.toList)

  import spec.{ isUnaryOption, isBinaryOption, isExpandOption }

  val Terminator = "--"
  val ValueForUnaryOption = "true"  // so if --opt is given, x(--opt) = true

  def mapForUnary(opt: String) = Map(fromOpt(opt) -> ValueForUnaryOption)
  def errorFn(msg: String) = println(msg)

  /** argMap is option -> argument (or "true" if it is a unary argument)
   *  residualArgs are what is left after removing the options and their args.
   */
  lazy val (argMap, residualArgs): (Map[String, String], List[String]) = {
    val residualBuffer = new ListBuffer[String]

    def loop(args: List[String]): Map[String, String] = {
      def residual(xs: List[String]) = { residualBuffer ++= xs ; Map[String, String]() }

      /*  Returns Some(List(args)) if this option expands to an
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

      /* Assumes known options have all been ruled out already. */
      def isUnknown(opt: String) =
        onlyKnownOptions && (opt startsWith "-") && {
          errorFn(s"Option '$opt' not recognized.")
          true
        }

      args match {
        case Nil              => Map()
        case Terminator :: xs => residual(xs)
        case x :: Nil         =>
          expand(x) match {
            case Some(expanded) => loop(expanded)
            case _ =>
              if (isBinaryOption(x) && enforceArity)
                errorFn(s"Option '$x' requires argument, found EOF instead.")

              if (isUnaryOption(x)) mapForUnary(x)
              else if (isUnknown(x)) Map()
              else residual(args)
          }

        case x1 :: (tail @ (x2 :: xs))   =>
          expand(x1) match {
            case Some(expanded) => loop(expanded ++ tail)
            case _ =>
              if (x2 == Terminator)         mapForUnary(x1) ++ residual(xs)
              else if (isUnaryOption(x1))   mapForUnary(x1) ++ loop(args.tail)
              else if (isBinaryOption(x1))  Map(fromOpt(x1) -> x2) ++ loop(xs)
              else if (isUnknown(x1))       loop(args.tail)
              else                          residual(List(x1)) ++ loop(args.tail)
          }
      }
    }

    (loop(originalArgs), residualBuffer.map(stripQuotes).toList)
  }

  def apply(arg: String)  = argMap(arg)
  def get(arg: String)    = argMap get arg
  def isSet(arg: String)  = argMap contains arg

  def getOrElse(arg: String, orElse: => String) = if (isSet(arg)) apply(arg) else orElse

  override def toString() = argMap.toString + " " + residualArgs.toString
}
