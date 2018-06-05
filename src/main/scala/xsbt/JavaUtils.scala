/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

private[xsbt] object JavaUtils {
  implicit class JavaForEach[T](val iterable: java.lang.Iterable[T]) extends AnyVal {

    @inline
    def foreach[U](op: T => U): Unit = {
      val iterator = iterable.iterator()
      while (iterator.hasNext) op(iterator.next())
    }
  }

  implicit class JavaMapForEach[K, V](val map: java.util.Map[K, V]) extends AnyVal {

    @inline
    def foreach[U](op: (K, V) => U): Unit = {
      val iterator = map.keySet().iterator()
      while (iterator.hasNext) {
        val key = iterator.next()
        op(key, map.get(key))
      }
    }
  }
}
