/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListMap.scala 9554 2007-01-04 16:30:16 +0000 (Thu, 04 Jan 2007) odersky $



package scala.collection.immutable

import Predef.Pair

/** This class implements empty immutable maps
 *
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
@serializable
class Map1[A, +B](key1: A, value1: B) extends Map[A, B] {

  def size = 1

  def get(key: A): Option[B] =
    if (key == key1) Some(value1) else None

  def elements = Iterator.single(Pair(key1, value1))

  def empty[B]: Map[A, B] = new EmptyMap[A, B]

  def update [B1 >: B](key: A, value: B1): Map[A, B1] =
    if (key == key1) new Map1(key1, value)
    else new Map2(key1, value1, key, value)

  def - (key: A): Map[A, B] =
    if (key == key1) empty else this
}



