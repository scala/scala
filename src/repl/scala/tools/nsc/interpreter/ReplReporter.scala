/* NSC -- new Scala compiler
 * Copyright 2002-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc.interpreter

import java.io.PrintWriter

import scala.tools.nsc.reporters._


trait ReplReporter extends ConsoleReporter {
  def out: PrintWriter

  def compilerInitialized(): Unit
  def initializeComplete: Boolean

  def currentRequest: IMain#Request
  def currentRequest_= (req: IMain#Request): Unit

  def withoutTruncating[T](body: => T): T
  def printUntruncatedMessage(msg: String): Unit
  def withoutUnwrapping(body: => Unit): Unit
  def unmangleInterpreterOutput(str: String): String

  def beQuietDuring[T](body: => T): T
  def beSilentDuring[T](body: => T): T

  def printResults: Boolean
  def printResults_= (b: Boolean): Unit

  def isDebug: Boolean
  def debug(msg: => String): Unit = if (isDebug) echo(msg)

  def isTrace: Boolean
  def trace(msg: => String): Unit = if (isTrace) echo(msg)
}
