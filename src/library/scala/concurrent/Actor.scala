/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Actor.scala 9235 2006-11-13 14:59:18 +0000 (Mon, 13 Nov 2006) mihaylov $


package scala.concurrent


import java.lang.Thread

/**
 * The class <code>Actor</code> ...
 *
 * @author Martin Odersky
 * @version 1.0
 */
abstract class Actor extends Thread {
  private val in = new MailBox

  def send(msg: in.Message) =
    in.send(msg)

  def receive[a](f: PartialFunction[in.Message, a]): a =
    if (currentThread == this) in.receive(f)
    else throw new IllegalArgumentException("receive called not on own process")

  def receiveWithin[a](msec: long)(f: PartialFunction[in.Message, a]): a =
    if (currentThread == this) in.receiveWithin(msec)(f)
    else throw new IllegalArgumentException("receiveWithin called not on own process")

  private var pid: Pid = null

  def self = {
    if (pid eq null) pid = new Pid(this)
    pid
  }

  def self_= (p: Pid) = pid = p
}



