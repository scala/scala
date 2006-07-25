/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Actor.scala 7955 2006-06-22 13:55:30Z michelou $

package scala.actors.threads

/**
 * The class <code>Actor</code> ...
 *
 * @author Martin Odersky
 * @version 1.0
 */
trait Actor[T] extends Thread with scala.actors.Actor[T] {
  private val in = new MailBox

  def !(msg: T): Unit =
    in.send(msg)

  def send(msg: T): Unit =
    in.send(msg)

  def receive[a](f: PartialFunction[Any, a]): a =
    if (Thread.currentThread() == this) in.receive(f)
    else error("receive called not on own process")

  def receiveWithin[a](msec: long)(f: PartialFunction[Any, a]): a =
    if (Thread.currentThread() == this) in.receiveWithin(msec)(f)
    else error("receiveWithin called not on own process")
}
