/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: EmptySet.scala 16893 2009-01-13 13:09:22Z cunei $



package scalax.collection.immutable

import collection.generic.Builder

/** This class implements empty immutable sets
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
@serializable
class EmptySet[A] extends Set[A] {

  def size: Int = 0

  def contains(elem: A): Boolean = false

  def + (elem: A): Set[A] = new Set1(elem)

  def - (elem: A): Set[A] = this

  def elements: Iterator[A] = Iterator.empty

  override def foreach(f: A => Unit): Unit = {}
}



