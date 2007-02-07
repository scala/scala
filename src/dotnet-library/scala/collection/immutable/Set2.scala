/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListSet.scala 9554 2007-01-04 16:30:16 +0000 (Thu, 04 Jan 2007) odersky $



package scala.collection.immutable

/** This class implements empty immutable maps
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
[serializable]
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



