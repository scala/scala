/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.util.control.ControlThrowable
import util.Exceptional.unwrap
import util.stackTraceString

trait ReplConfig {
  lazy val replProps = new ReplProps

  class TapMaker[T](x: T) {
    def tapDebug(msg: => String): T = tap(x => repldbg(parens(x)))
    def tap[U](f: T => U): T = {
      f(x)
      x
    }
  }

  private def parens(x: Any) = "(" + x + ")"
  private def echo(msg: => String) =
    try Console println msg
    catch { case x: AssertionError => Console.println("Assertion error printing debugging output: " + x) }

  private[nsc] def repldbg(msg: => String)    = if (isReplDebug) echo(msg)
  private[nsc] def repltrace(msg: => String)  = if (isReplTrace) echo(msg)
  private[nsc] def replinfo(msg: => String)   = if (isReplInfo)  echo(msg)

  private[nsc] def logAndDiscard[T](label: String, alt: => T): PartialFunction[Throwable, T] = {
    case t: ControlThrowable => throw t
    case t: Throwable        =>
      repldbg(label + ": " + unwrap(t))
      repltrace(stackTraceString(unwrap(t)))
      alt
  }
  private[nsc] def substituteAndLog[T](label: String, alt: => T)(body: => T): T = {
    try body
    catch logAndDiscard(label, alt)
  }

  def isReplTrace: Boolean = replProps.trace
  def isReplDebug: Boolean = replProps.debug || isReplTrace
  def isReplInfo: Boolean  = replProps.info || isReplDebug
  def isReplPower: Boolean = replProps.power

  private def csv(p: String, v: String) = p split "," contains v
  def isPaged: Boolean     = replProps.format.isSet && csv(replProps.format.get, "paged")
  def isAcross: Boolean    = replProps.format.isSet && csv(replProps.format.get, "across")
}
