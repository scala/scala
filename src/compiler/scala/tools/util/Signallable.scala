/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package util

/** A class for things which are signallable.
 */
abstract class Signallable[T] private (val signal: String, val description: String) {
  private var last: Option[T] = None
  private def lastString = last filterNot (_ == ()) map (_.toString) getOrElse ""

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
  def apply[T](description: String)(body: => T): Signallable[T] =
    apply(SignalManager.findOpenSignal().name, description)(body)

  /** Given a signal name, a description, and a handler body, this
   *  registers a signal handler and returns the Signallable instance.
   *  The signal handler registry is thereafter available by calling
   *  SignalManager.info(), or sending SIGINFO to the manager will
   *  dump it to console.
   */
  def apply[T](signal: String, description: String)(body: => T): Signallable[T] = {
    val result = new Signallable[T](signal, description) {
      def onSignal(): T = {
        val result = body
        last = Some(result)
        result
      }
    }
    SignalManager.public(signal, description)(result.onSignal())
    result
  }
}

