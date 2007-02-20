/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListSet.scala 9554 2007-01-04 16:30:16 +0000 (Thu, 04 Jan 2007) odersky $



package scala.collection.immutable

/** This class implements empty immutable maps
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
@serializable
class EmptySet[A] extends Set[A] {

  def empty[C]: Set[C] = new EmptySet[C]

  def size: Int = 0

  def contains(elem: A): Boolean = false

  def + (elem: A): Set[A] = new Set1(elem)

  def - (elem: A): Set[A] = this

  def elements: Iterator[A] = Iterator.empty
}



