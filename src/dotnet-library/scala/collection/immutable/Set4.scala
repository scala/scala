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
class Set4[A](elem1: A, elem2: A, elem3: A, elem4: A) extends Set[A] {

  def empty[C]: Set[C] = new EmptySet[C]

  def size: Int = 4

  def contains(elem: A): Boolean =
    elem == elem1 || elem == elem2 || elem == elem3 || elem == elem4

  def + (elem: A): Set[A] =
    if (contains(elem)) this
    else HashSet(elem1, elem2, elem3, elem4, elem)

  def - (elem: A): Set[A] =
    if      (elem == elem1) new Set3(elem2, elem3, elem4)
    else if (elem == elem2) new Set3(elem1, elem3, elem4)
    else if (elem == elem3) new Set3(elem1, elem2, elem4)
    else if (elem == elem4) new Set3(elem1, elem2, elem3)
    else this

  def elements: Iterator[A] =
    Iterator.fromValues(elem1, elem2, elem3, elem4)
}



