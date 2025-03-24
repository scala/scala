/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
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
