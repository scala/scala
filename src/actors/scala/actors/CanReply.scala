/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

/**
 * Defines result-bearing message send operations.
 *
 * @author Philipp Haller
 *
 * @define actor `CanReply`
 */
trait CanReply[-T, +R] {

  type Future[+P] <: () => P

  /**
   * Sends <code>msg</code> to this $actor and
   * awaits reply (synchronous).
   *
   * @param  msg the message to be sent
   * @return     the reply
   */
  def !?(msg: T): R

  /**
   * Sends <code>msg</code> to this $actor and
   * awaits reply (synchronous) within <code>msec</code>
   * milliseconds.
   *
   * @param  msec the time span before timeout
   * @param  msg  the message to be sent
   * @return      <code>None</code> in case of timeout, otherwise
   *              <code>Some(x)</code> where <code>x</code> is the reply
   */
  def !?(msec: Long, msg: T): Option[R]

  /**
   * Sends <code>msg</code> to this $actor and
   * immediately returns a future representing the reply value.
   *
   * @param  msg the message to be sent
   * @return     the future
   */
  def !!(msg: T): Future[R]

  /**
   * Sends <code>msg</code> to this $actor and
   * immediately returns a future representing the reply value.
   * The reply is post-processed using the partial function
   * <code>handler</code>. This also allows to recover a more
   * precise type for the reply value.
   *
   * @param      msg the message to be sent
   * @param  handler the function to be applied to the response
   * @return         the future
   */
  def !![P](msg: T, handler: PartialFunction[R, P]): Future[P]

}
