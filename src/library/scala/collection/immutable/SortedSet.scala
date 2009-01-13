/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.immutable

trait SortedSet[A] extends scala.collection.SortedSet[A] with Set[A] {
  override def ++ (elems: Iterable[A]): SortedSet[A] =
    (this /: elems) ((s, elem) => s + elem)
  override def +(elem: A): SortedSet[A]
}
