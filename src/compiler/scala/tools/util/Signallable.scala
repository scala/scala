/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package util

import java.security.AccessControlException

/** A class for things which are signallable.
 */
abstract class Signallable[T] private (val signal: String, val description: String) {
  private var last: Option[T] = None
  private def lastString = last match {
    case Some(())   => ""
    case Some(x)    => "" + x
    case _          => ""
  }

  /** The most recent result from the signal handler. */
  def lastResult: Option[T] = last

  /** Method to be executed when the associated signal is received. */
  def onSignal(): T

  // todo:
  // def unregister(): Boolean

  override def toString =  "  SIG(%s) => %s%s".format(
    signal, description, if (lastString == "") "" else " (" + lastString + ")"
  )
}

object Signallable {
  /** Same as the other apply, but an open signal is found for you.
   */
  def apply[T](description: String)(body: => T): Signallable[T] = wrap {
    apply(SignalManager.findOpenSignal().name, description)(body)
  }

  /** Given a signal name, a description, and a handler body, this
   *  registers a signal handler and returns the Signallable instance.
   *  The signal handler registry is thereafter available by calling
   *  SignalManager.info(), or sending SIGINFO to the manager will
   *  dump it to console.
   */
  def apply[T](signal: String, description: String)(body: => T): Signallable[T] = wrap {
    val result = create[T](signal, description, body)
    SignalManager.public(signal, description)(result.onSignal())
    result
  }

  private def wrap[T](body: => Signallable[T]): Signallable[T] =
    try body catch { case _: AccessControlException => null }

  private def create[T](signal: String, description: String, body: => T): Signallable[T] =
    new Signallable[T](signal, description) {
      def onSignal = {
        val result = body
        last = Some(result)
        result
      }
    }
}
