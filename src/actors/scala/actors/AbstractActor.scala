/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * The <code>AbstractActor</code> trait.
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
trait AbstractActor extends OutputChannel[Any] {

  private[actors] var exiting = false

  private[actors] def linkTo(to: AbstractActor): Unit

  private[actors] def unlinkFrom(from: AbstractActor): Unit

  private[actors] def exit(from: AbstractActor, reason: AnyRef): Unit

  def !?(msg: Any): Any

  def !?(msec: Long, msg: Any): Option[Any]

  def !!(msg: Any): Future[Any]

  def !![A](msg: Any, f: PartialFunction[Any, A]): Future[A]

}
