/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors
package remote

object FreshNameCreator {

  protected var counter = 0
  protected val counters = new scala.collection.mutable.HashMap[String, Int]

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
    Symbol(prefix + count)
  }

  def newName(): Symbol = {
    counter += 1
    Symbol("$" + counter + "$")
  }
}
