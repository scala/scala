/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.concurrent;

abstract class Actor extends Thread {
  private val in = new MailBox;

  def send(msg: in.Message) =
    in.send(msg);

  def receive[a](f: PartialFunction[in.Message, a]): a =
    if (Thread.currentThread() == this) in.receive(f);
    else error("receive called not on own process");

  def receiveWithin[a](msec: long)(f: PartialFunction[in.Message, a]): a =
    if (Thread.currentThread() == this) in.receiveWithin(msec)(f);
    else error("receiveWithin called not on own process");
}



