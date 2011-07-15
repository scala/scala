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
   * Sends `msg` to this $actor and awaits reply (synchronous).
   *
   * @param  msg the message to be sent
   * @return     the reply
   */
  def !?(msg: T): R

  /**
   * Sends `msg` to this $actor and awaits reply (synchronous) within
   * `msec` milliseconds.
   *
   * @param  msec the time span before timeout
   * @param  msg  the message to be sent
   * @return      `None` in case of timeout, otherwise
   *              `Some(x)` where `x` is the reply
   */
  def !?(msec: Long, msg: T): Option[R]

  /**
   * Sends `msg` to this $actor and immediately returns a future representing
   * the reply value.
   *
   * @param  msg the message to be sent
   * @return     the future
   */
  def !!(msg: T): Future[R]

  /**
   * Sends `msg` to this $actor and immediately returns a future representing
   * the reply value. The reply is post-processed using the partial function
   * `handler`. This also allows to recover a more precise type for the reply
   * value.
   *
   * @param      msg the message to be sent
   * @param  handler the function to be applied to the response
   * @return         the future
   */
  def !![P](msg: T, handler: PartialFunction[R, P]): Future[P]

}
