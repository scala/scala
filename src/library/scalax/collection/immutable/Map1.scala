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
class Map1[A, B](key1: A, value1: B) extends Map[A, B] {

  def size = 1

  def get(key: A): Option[B] =
    if (key == key1) Some(value1) else None

  def elements = Iterator((key1, value1))

  def update (key: A, value: B): Map[A, B] =
    if (key == key1) new Map1(key1, value)
    else null // new Map2(key1, value1, key, value)

  def - (key: A): Map[A, B] =
    if (key == key1) empty else this
}



