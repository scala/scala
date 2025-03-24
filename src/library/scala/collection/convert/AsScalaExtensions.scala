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

/** Defines `asScala` extension methods, available through [[scala.jdk.CollectionConverters]]. */
trait AsScalaExtensions {
  import scala.jdk.javaapi.{CollectionConverters => conv}

  implicit class IteratorHasAsScala[A](i: ju.Iterator[A]) {
    /** Converts a Java `Iterator` to a Scala `Iterator`, see
      * [[AsScalaConverters.asScala[A](i:java\.util\.Iterator[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: Iterator[A] = conv.asScala(i)
  }

  implicit class EnumerationHasAsScala[A](e: ju.Enumeration[A]) {
    /** Converts a Java `Enumeration` to a Scala `Iterator`, see
      * [[AsScalaConverters.asScala[A](e:java\.util\.Enumeration[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: Iterator[A] = conv.asScala(e)
  }

  implicit class IterableHasAsScala[A](i: jl.Iterable[A]) {
    /** Converts a Java `Iterable` to a Scala `Iterable`, see
      * [[AsScalaConverters.asScala[A](i:Iterable[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: Iterable[A] = conv.asScala(i)
  }

  implicit class CollectionHasAsScala[A](c: ju.Collection[A]) {
    /** Converts a Java `Collection` to a Scala `Iterable`, see
      * [[AsScalaConverters.asScala[A](c:java\.util\.Collection[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: Iterable[A] = conv.asScala(c)
  }

  implicit class ListHasAsScala[A](l: ju.List[A]) {
    /** Converts a Java `List` to a Scala `Buffer`, see
      * [[AsScalaConverters.asScala[A](l:java\.util\.List[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Buffer[A] = conv.asScala(l)
  }

  implicit class SetHasAsScala[A](s: ju.Set[A]) {
    /** Converts a Java `Set` to a Scala `Set`, see
      * [[AsScalaConverters.asScala[A](s:java\.util\.Set[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Set[A] = conv.asScala(s)
  }

  implicit class MapHasAsScala[K, V](m: ju.Map[K, V]) {
    /** Converts a Java `Map` to a Scala `Map`, see
      * [[AsScalaConverters.asScala[A,B](m:java\.util\.Map[A,B])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Map[K, V] = conv.asScala(m)
  }

  implicit class ConcurrentMapHasAsScala[K, V](m: juc.ConcurrentMap[K, V]) {
    /** Converts a Java `ConcurrentMap` to a Scala `concurrent.Map`, see
      * [[AsScalaConverters.asScala[A,B](m:java\.util\.concurrent\.ConcurrentMap[A,B])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: concurrent.Map[K, V] = conv.asScala(m)
  }

  implicit class DictionaryHasAsScala[K, V](d: ju.Dictionary[K, V]) {
    /** Converts a Java `Dictionary` to a Scala `Map`, see
      * [[AsScalaConverters.asScala[A,B](d:java\.util\.Dictionary[A,B])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Map[K, V] = conv.asScala(d)
  }

  implicit class PropertiesHasAsScala(i: ju.Properties) {
    /** Converts a Java `Properties` to a Scala `Map`, see
      * [[AsScalaConverters.asScala(p:java\.util\.Properties)* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Map[String, String] = conv.asScala(i)
  }
}
