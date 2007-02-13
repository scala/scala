/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ListMap.scala 9554 2007-01-04 16:30:16 +0000 (Thu, 04 Jan 2007) odersky $



package scala.collection.immutable

import Predef.Pair

/** This class implements empty immutable maps
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
[serializable]
class Map2[A, +B](key1: A, value1: B, key2: A, value2: B) extends Map[A, B] {

  def size = 2

  def get(key: A): Option[B] =
    if (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else None

  def elements = Iterator.fromValues(
    Pair(key1, value1), Pair(key2, value2))

  def empty[C]: Map[A, C] = new EmptyMap[A, C]

  def update [B1 >: B](key: A, value: B1): Map[A, B1] =
    if (key == key1) new Map2(key1, value, key2, value2)
    else if (key == key2) new Map2(key1, value1, key2, value)
    else new Map3(key1, value1, key2, value2, key, value)

  def - (key: A): Map[A, B] =
    if (key == key1) new Map1(key2, value2)
    else if (key == key2) new Map1(key1, value1)
    else this
}



