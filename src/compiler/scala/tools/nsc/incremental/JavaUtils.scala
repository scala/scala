/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
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
