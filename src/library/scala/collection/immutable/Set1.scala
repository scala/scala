/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$



package scala.collection.immutable

/** This class implements immutable sets with exactly one element.
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
@serializable
class Set1[A](elem1: A) extends Set[A] {

  def empty[C]: Set[C] = new EmptySet[C]

  def size: Int = 1

  def contains(elem: A): Boolean =
    elem == elem1

  def + (elem: A): Set[A] =
    if (contains(elem)) this
    else new Set2(elem1, elem)

  def - (elem: A): Set[A] =
    if (elem == elem1) empty
    else this

  def elements: Iterator[A] =
    Iterator.fromValues(elem1)
}



