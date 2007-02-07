/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.concurrent


/**
 *  @author  Erik Stenman
 *  @version 1.0, 01/10/2003
 */
object NameServer {

  val names = new scala.collection.mutable.HashMap[Symbol, Process]

  /**
   *  @param name ...
   *  @param proc ...
   */
  def register(name: Symbol, proc: Process) = {
    if (names contains name) throw new IllegalArgumentException("Name:" + name + " already registred")
    names += name -> proc
  }

  def unregister(name: Symbol) =
    if (names contains name) names -= name
    else throw new IllegalArgumentException("Name:" + name + " not registred")

  /**
   *  @param name ...
   *  @return     ...
   */
  def whereis(name: Symbol): Option[Process] =
    names.get(name)

  def send(name: Symbol, msg: MailBox#Message) =
    names(name).send(msg)

}
