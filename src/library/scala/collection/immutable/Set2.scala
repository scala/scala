/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$



package scala.collection.immutable

/** This class implements immutable sets with exactly two elements.
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
@serializable
class Set2[A](elem1: A, elem2: A) extends Set[A] {

  def empty[C]: Set[C] = new EmptySet[C]

  def size: Int = 2

  def contains(elem: A): Boolean =
    elem == elem1 || elem == elem2

  def + (elem: A): Set[A] =
    if (contains(elem)) this
    else new Set3(elem1, elem2, elem)

  def - (elem: A): Set[A] =
    if (elem == elem1) new Set1(elem2)
    else if (elem == elem2) new Set1(elem1)
    else this

  def elements: Iterator[A] =
    Iterator.fromValues(elem1, elem2)
}



