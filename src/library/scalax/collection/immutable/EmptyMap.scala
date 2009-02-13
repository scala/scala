/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: EmptyMap.scala 16893 2009-01-13 13:09:22Z cunei $



package scalax.collection.immutable

/** This class implements empty immutable maps
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
@serializable
class EmptyMap[A, B] extends Map[A, B] {

  def size: Int = 0

  def get(key: A): Option[B] = None

  def elements: Iterator[(A, B)] = Iterator.empty

  def update (key: A, value: B): Map[A, B] = new Map1(key, value)

  def - (key: A): Map[A, B] = this
}



