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

package scala.jdk.javaapi

import scala.collection.convert.{AsJavaConverters, AsScalaConverters}

/** This object contains methods that convert between Scala and Java collections.
  *
  * The explicit conversion methods defined here are intended to be used in Java code. For Scala
  * code, it is recommended to use the extension methods defined in
  * [[scala.jdk.CollectionConverters]].
  *
  * Note: to create [[java.util.stream.Stream Java Streams]] that operate on Scala collections
  * (sequentially or in parallel), use [[StreamConverters]].
  *
  * {{{
  *   // Java Code
  *   import scala.jdk.javaapi.CollectionConverters;
  *   public class A {
  *     public void t(scala.collection.immutable.List<String> l) {
  *       java.util.List<String> jl = CollectionConverters.asJava(l);
  *     }
  *   }
  * }}}
  *
  * The conversions return adapters for the corresponding API, i.e., the collections are wrapped,
  * not copied. Changes to the original collection are reflected in the view, and vice versa.
  *
  * The following conversions are supported via `asScala` and `asJava`:
  *
  * {{{
  *   scala.collection.Iterable       <=> java.lang.Iterable
  *   scala.collection.Iterator       <=> java.util.Iterator
  *   scala.collection.mutable.Buffer <=> java.util.List
  *   scala.collection.mutable.Set    <=> java.util.Set
  *   scala.collection.mutable.Map    <=> java.util.Map
  *   scala.collection.concurrent.Map <=> java.util.concurrent.ConcurrentMap
  * }}}
  *
  * The following conversions are supported via `asScala` and through
  * specially-named methods to convert to Java collections, as shown:
  *
  * {{{
  *   scala.collection.Iterable    <=> java.util.Collection   (via asJavaCollection)
  *   scala.collection.Iterator    <=> java.util.Enumeration  (via asJavaEnumeration)
  *   scala.collection.mutable.Map <=> java.util.Dictionary   (via asJavaDictionary)
  * }}}
  *
  * In addition, the following one-way conversions are provided via `asJava`:
  *
  * {{{
  *   scala.collection.Seq         => java.util.List
  *   scala.collection.mutable.Seq => java.util.List
  *   scala.collection.Set         => java.util.Set
  *   scala.collection.Map         => java.util.Map
  * }}}
  *
  * The following one way conversion is provided via `asScala`:
  *
  * {{{
  *   java.util.Properties => scala.collection.mutable.Map
  * }}}
  *
  * In all cases, converting from a source type to a target type and back
  * again will return the original source object.
  */
object CollectionConverters extends AsJavaConverters with AsScalaConverters
