// Copyright 2002-2017 LAMP/EPFL and Lightbend, Inc.

package scala.tools.nsc.interpreter

import java.io.PrintWriter

import scala.tools.nsc.reporters.Reporter

trait ReplReporter extends Reporter {
  def out: PrintWriter

  /**
    * Print message (info/warning/error).
    * By default, messages beyond a certain length are truncated (see `withoutTruncating`),
    * and internal repl wrapping is removed (see `withoutUnwrapping` and `unmangleInterpreterOutput`).
    * To suppress all output, use `suppressOutput`
    */
  def printMessage(msg: String): Unit

  /** Don't print any errors/messages/echos during the execution of `body`.
    */
  def suppressOutput[T](body: => T): T

  /** Suppress truncation during the executing of `body`.
    */
  def withoutTruncating[T](body: => T): T

  /** Do not remove interpreter wrappers ($iw etc) from all output during the execution of `body`.
    */
  def withoutUnwrapping(body: => Unit): Unit


  /** Print result (Right --> success, Left --> error)
    */
  def printResult(result: Either[String, String]): Unit

  /** Don't print result lines.
    */
  def withoutPrintingResults[T](body: => T): T

  /** Whether we're printing results (should only be used from the shell).
    */
  def printResults: Boolean

  /** Toggle whether to print results (should only be used from the shell).
    */
  def togglePrintResults(): Unit


  //// println debugging ftw
  def isDebug: Boolean
  def debug(msg: => String): Unit = if (isDebug) echo(msg)

  def isTrace: Boolean
  def trace(msg: => String): Unit = if (isTrace) echo(msg)

  //// Internal signalling from repl to shell

  /** Currently executing request (used to determine position of error in terms of user-submitted code)
    *
    * TODO: should no longer be needed if we do wrapping after type checking
    */
  def currentRequest: IMain#Request

  /** Set currently executing request.
    */
  def currentRequest_= (req: IMain#Request): Unit

}
