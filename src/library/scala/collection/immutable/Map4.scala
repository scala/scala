/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.immutable

import Predef._

/** This class implements immutable maps with exactly four entries
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
@serializable
class Map4[A, +B](key1: A, value1: B, key2: A, value2: B, key3: A, value3: B, key4: A, value4: B) extends Map[A, B] {

  def size = 4

  def get(key: A): Option[B] =
    if      (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else if (key == key3) Some(value3)
    else if (key == key4) Some(value4)
    else None

  def elements = Iterator.fromValues(
    (key1, value1), (key2, value2), (key3, value3), (key4, value4))

  def empty[C]: Map[A, C] = new EmptyMap[A, C]

  def update [B1 >: B](key: A, value: B1): Map[A, B1] =
    if      (key == key1) new Map4(key1, value, key2, value2, key3, value3, key4, value4)
    else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
    else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
    else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
    else HashMap(key1 -> value1, key2 -> value2, key3 -> value3, key4 -> value4, key -> value)

  def - (key: A): Map[A, B] =
    if      (key == key1) new Map3(key2, value2, key3, value3, key4, value4)
    else if (key == key2) new Map3(key1, value1, key3, value3, key4, value4)
    else if (key == key3) new Map3(key1, value1, key2, value2, key4, value4)
    else if (key == key4) new Map3(key1, value1, key2, value2, key3, value3)
    else this
}



