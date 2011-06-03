/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

trait ReplConfig {
  lazy val replProps = new ReplProps

  class TapMaker[T](x: T) {
    def tapInfo(msg: => String): T  = tap(x => replinfo(parens(x)))
    def tapDebug(msg: => String): T = tap(x => repldbg(parens(x)))
    def tapTrace(msg: => String): T = tap(x => repltrace(parens(x)))
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

  def isReplTrace: Boolean = replProps.trace
  def isReplDebug: Boolean = replProps.debug || isReplTrace
  def isReplInfo: Boolean  = replProps.info || isReplDebug
  def isReplPower: Boolean = replProps.power
}
