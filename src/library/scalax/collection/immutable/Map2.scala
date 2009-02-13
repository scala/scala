/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map1.scala 16893 2009-01-13 13:09:22Z cunei $



package scalax.collection.immutable

/** This class implements immutable maps with exactly one entry
 *
 *  @author  Martin Oderskty
 *  @version 1.0, 019/01/2007
 */
@serializable
class Map2[A, B](key1: A, value1: B, key2: A, value2: B) extends Map[A, B] {

  def size = 2

  def get(key: A): Option[B] =
    if (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else None

  def elements = Iterator((key1, value1), (key2, value2))

  def update (key: A, value: B): Map[A, B] =
    if (key == key1) new Map2(key1, value, key2, value2)
    else if (key == key2) new Map2(key1, value1, key2, value)
    else new Map3(key1, value1, key2, value2, key, value)

  def - (key: A): Map[A, B] =
    if (key == key1) new Map1(key2, value2)
    else if (key == key2) new Map1(key1, value1)
    else this
}



