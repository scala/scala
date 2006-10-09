/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.remote

object FreshNameCreator {

  protected var counter = 0
  protected val counters = new scala.collection.mutable.HashMap[String,int]

  /**
   * Create a fresh name with the given prefix. It is guaranteed
   * that the returned name has never been returned by a previous
   * call to this function (provided the prefix does not end in a digit).
   */
  def newName(prefix: String): Symbol = {
    val count = counters.get(prefix) match {
      case Some(last) => last + 1
      case None => 0
    }
    counters.update(prefix, count)
    new Symbol(prefix + count)
  }

  def newName(): Symbol = {
    counter = counter + 1
    new Symbol("$" + counter + "$")
  }
}
