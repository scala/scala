/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map.scala 16893 2009-01-13 13:09:22Z cunei $


package scala.collection.immutable

import generic._

/** A default implementation of immutable maps.
 *  This is implemented by specialized implementations
 *  `EmptyMap`, Map1, ..., Map4, for maps of size up to 4, and
 *  a proxy for an immutable HashMap for larger maps.
 */
trait FlexMap[A, +B] extends Map[A, B] { self =>
  override def empty: Map[A, B] = FlexMap.empty
}

/* Factory object for `FlexMap class */
object FlexMap extends ImmutableMapFactory[Map] {
  def empty[A, B]: FlexMap[A, B] = new EmptyMap[A, B]

  @serializable
  class EmptyMap[A, +B] extends FlexMap[A, B] {
    override def size: Int = 0
    def get(key: A): Option[B] = None
    def elements: Iterator[(A, B)] = Iterator.empty
    def add [B1 >: B] (key: A, value: B1): Map[A, B1] = new Map1(key, value)
    def - (key: A): Map[A, B] = this
  }

  @serializable
  class Map1[A, +B](key1: A, value1: B) extends FlexMap[A, B] {
    override def size = 1
    def get(key: A): Option[B] =
      if (key == key1) Some(value1) else None
    def elements = Iterator((key1, value1))
    def add [B1 >: B] (key: A, value: B1): Map[A, B1] =
      if (key == key1) new Map1(key1, value)
      else new Map2(key1, value1, key, value)
    def - (key: A): Map[A, B] =
      if (key == key1) empty else this
    override def foreach(f: ((A, B)) => Unit): Unit = {
      f((key1, value1))
    }
  }

  @serializable
  class Map2[A, +B](key1: A, value1: B, key2: A, value2: B) extends FlexMap[A, B] {
    override def size = 2
    def get(key: A): Option[B] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else None
    def elements = Iterator((key1, value1), (key2, value2))
    def add [B1 >: B] (key: A, value: B1): Map[A, B1] =
      if (key == key1) new Map2(key1, value, key2, value2)
      else if (key == key2) new Map2(key1, value1, key2, value)
      else new Map3(key1, value1, key2, value2, key, value)
    def - (key: A): Map[A, B] =
      if (key == key1) new Map1(key2, value2)
      else if (key == key2) new Map1(key1, value1)
      else this
    override def foreach(f: ((A, B)) => Unit): Unit = {
      f((key1, value1)); f((key2, value2))
    }
  }

  @serializable
  class Map3[A, +B](key1: A, value1: B, key2: A, value2: B, key3: A, value3: B) extends FlexMap[A, B] {
    override def size = 3
    def get(key: A): Option[B] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else None
    def elements = Iterator((key1, value1), (key2, value2), (key3, value3))
    def add [B1 >: B] (key: A, value: B1): Map[A, B1] =
      if (key == key1)      new Map3(key1, value, key2, value2, key3, value3)
      else if (key == key2) new Map3(key1, value1, key2, value, key3, value3)
      else if (key == key3) new Map3(key1, value1, key2, value2, key3, value)
      else new Map4(key1, value1, key2, value2, key3, value3, key, value)
    def - (key: A): Map[A, B] =
      if (key == key1)      new Map2(key2, value2, key3, value3)
      else if (key == key2) new Map2(key1, value1, key3, value3)
      else if (key == key3) new Map2(key1, value1, key2, value2)
      else this
    override def foreach(f: ((A, B)) => Unit): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3))
    }
  }

  @serializable
  class Map4[A, +B](key1: A, value1: B, key2: A, value2: B, key3: A, value3: B, key4: A, value4: B) extends FlexMap[A, B] {
    override def size = 4
    def get(key: A): Option[B] =
      if (key == key1) Some(value1)
      else if (key == key2) Some(value2)
      else if (key == key3) Some(value3)
      else if (key == key4) Some(value4)
      else None
    def elements = Iterator((key1, value1), (key2, value2), (key3, value3), (key4, value4))
    def add [B1 >: B] (key: A, value: B1): Map[A, B1] =
      if (key == key1)      new Map4(key1, value, key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map4(key1, value1, key2, value, key3, value3, key4, value4)
      else if (key == key3) new Map4(key1, value1, key2, value2, key3, value, key4, value4)
      else if (key == key4) new Map4(key1, value1, key2, value2, key3, value3, key4, value)
      else new HashMap + ((key1, value1), (key2, value2), (key3, value3), (key4, value4), (key, value))
    def - (key: A): Map[A, B] =
      if (key == key1)      new Map3(key2, value2, key3, value3, key4, value4)
      else if (key == key2) new Map3(key1, value1, key3, value3, key4, value4)
      else if (key == key3) new Map3(key1, value1, key2, value2, key4, value4)
      else if (key == key4) new Map3(key1, value1, key2, value2, key3, value3)
      else this
    override def foreach(f: ((A, B)) => Unit): Unit = {
      f((key1, value1)); f((key2, value2)); f((key3, value3)); f((key4, value4))
    }
  }
}


















