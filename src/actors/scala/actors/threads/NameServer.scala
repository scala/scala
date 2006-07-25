/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: NameServer.scala 5889 2006-03-05 00:33:02Z mihaylov $


package scala.actors.threads;


object NameServer {

  val names = new scala.collection.mutable.HashMap[Symbol, Process];

  def register(name: Symbol, proc: Process) = {
    if (names.contains(name)) error("Name:" + name + " already registred");
    names += name -> proc;
  }

  def unregister(name: Symbol) = {
    if (names.contains(name))
      names -= name;
    else
      error("Name:" + name + " not registred");
  }

  def whereis(name: Symbol): Option[Process] =
    names.get(name);

  def send(name: Symbol, msg: Any) =
    names(name).send(msg);

}
