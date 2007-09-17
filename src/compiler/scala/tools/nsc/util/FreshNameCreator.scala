/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.util

import scala.collection.mutable.HashMap

trait FreshNameCreator {
  def newName(prefix : String) : String
  def newName() : String
  def newName(pos : util.Position, prefix : String) : String
}
object FreshNameCreator {
  class Default extends FreshNameCreator {

  protected var counter = 0
  protected val counters = new HashMap[String, Int]

  /**
   * Create a fresh name with the given prefix. It is guaranteed
   * that the returned name has never been returned by a previous
   * call to this function (provided the prefix does not end in a digit).
   */
  def newName(prefix: String): String = {
    val count = counters.get(prefix) match {
      case Some(last) => last + 1
      case None => 0
    }
    counters.update(prefix, count)
    prefix + count
  }
  def newName(pos : util.Position, prefix : String) = newName(prefix)

  def newName(): String = {
    counter = counter + 1
    "$" + counter + "$"
  }
}
}