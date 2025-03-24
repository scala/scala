/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package convert

import java.util.{concurrent => juc}
import java.{lang => jl, util => ju}

/** Defines `asJava` extension methods, available through [[scala.jdk.CollectionConverters]]. */
trait AsJavaExtensions {
  import scala.jdk.javaapi.{CollectionConverters => conv}

  implicit class IteratorHasAsJava[A](i: Iterator[A]) {
    /** Converts a Scala `Iterator` to a Java `Iterator`, see
      * [[AsJavaConverters.asJava[A](i:Iterator[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Iterator[A] = conv.asJava(i)

    /** Converts a Scala `Iterator` to a Java `Enumeration`, see
      * [[AsJavaConverters.asJavaEnumeration `scala.jdk.javaapi.CollectionConverters.asJavaEnumeration`]].
      */
    def asJavaEnumeration: ju.Enumeration[A] = conv.asJavaEnumeration(i)
  }

  implicit class IterableHasAsJava[A](i: Iterable[A]) {
    /** Converts a Scala `Iterable` to a Java `Iterable`, see
      * [[AsJavaConverters.asJava[A](i:Iterable[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: jl.Iterable[A] = conv.asJava(i)

    /** Converts a Scala `Iterator` to a Java `Collection`, see
      * [[AsJavaConverters.asJavaCollection `scala.jdk.javaapi.CollectionConverters.asJavaCollection`]].
      */
    def asJavaCollection: ju.Collection[A] = conv.asJavaCollection(i)
  }

  implicit class BufferHasAsJava[A](b: mutable.Buffer[A]) {
    /** Converts a Scala `Buffer` to a Java `List`, see
      * [[AsJavaConverters.asJava[A](b:scala\.collection\.mutable\.Buffer[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.List[A] = conv.asJava(b)
  }

  implicit class MutableSeqHasAsJava[A](s: mutable.Seq[A]) {
    /** Converts a Scala `Seq` to a Java `List`, see
      * [[AsJavaConverters.asJava[A](s:scala\.collection\.mutable\.Seq[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.List[A] = conv.asJava(s)
  }

  implicit class SeqHasAsJava[A](s: Seq[A]) {
    /** Converts a Scala `Seq` to a Java `List`, see
      * [[AsJavaConverters.asJava[A](s:scala\.collection\.Seq[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.List[A] = conv.asJava(s)
  }

  implicit class MutableSetHasAsJava[A](s: mutable.Set[A]) {
    /** Converts a Scala `mutable.Set` to a Java `Set`, see
      * [[AsJavaConverters.asJava[A](s:scala\.collection\.mutable\.Set[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Set[A] = conv.asJava(s)
  }

  implicit class SetHasAsJava[A](s: Set[A]) {
    /** Converts a Scala `Set` to a Java `Set`, see
      * [[AsJavaConverters.asJava[A](s:scala\.collection\.Set[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Set[A] = conv.asJava(s)
  }

  implicit class MutableMapHasAsJava[K, V](m: mutable.Map[K, V]) {
    /** Converts a Scala `mutable.Map` to a Java `Map`, see
      * [[AsJavaConverters.asJava[K,V](m:scala\.collection\.mutable\.Map[K,V])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Map[K, V] = conv.asJava(m)

    /** Converts a Scala `mutable.Map` to a Java `Map`, see
      * [[AsJavaConverters.asJavaDictionary `scala.jdk.javaapi.CollectionConverters.asJavaDictionary`]].
      */
    def asJavaDictionary: ju.Dictionary[K, V] = conv.asJavaDictionary(m)
  }

  implicit class MapHasAsJava[K, V](m: Map[K, V]) {
    /** Converts a Scala `Map` to a Java `Map`, see
      * [[AsJavaConverters.asJava[K,V](m:scala\.collection\.Map[K,V])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Map[K, V] = conv.asJava(m)
  }

  implicit class ConcurrentMapHasAsJava[K, V](m: concurrent.Map[K, V]) {
    /** Converts a Scala `concurrent.Map` to a Java `ConcurrentMap`, see
      * [[AsJavaConverters.asJava[K,V](m:scala\.collection\.concurrent\.Map[K,V])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: juc.ConcurrentMap[K, V] = conv.asJava(m)
  }
}
