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

/** The jdk package contains utilities to interact with JDK classes.
 * 
 * This packages offers a number of converters, that are able to wrap or copy
 * types from the scala library to equivalent types in the JDK class library
 * and vice versa:
 * 
 *  - [[CollectionConverters]], converting collections like [[scala.collection.Seq]],
 *    [[scala.collection.Map]], [[scala.collection.Set]],
 *    [[scala.collection.mutable.Buffer]], [[scala.collection.Iterator]]
 *    and [[scala.collection.Iterable]] to their JDK counterparts
 *  - [[OptionConverters]], converting between [[Option]] and
 *    [[java.util.Optional]] and primitive variations
 *  - [[StreamConverters]], to create JDK Streams from scala collections
 *  - [[DurationConverters]], for conversions between scala
 *    [[scala.concurrent.duration.FiniteDuration]] and [[java.time.Duration]]
 *  - [[FunctionConverters]], from scala Functions to java
 *    [[java.util.function.Function]], [[java.util.function.UnaryOperator]],
 *    [[java.util.function.Consumer]] and [[java.util.function.Predicate]], as
 *    well as primitive variations and Bi-variations.
 * 
 * By convention, converters that wrap an object to provide a different
 * interface to the same underlying data structure use .asScala and .asJava
 * extension methods, whereas converters that copy the underlying data structure
 * use .toScala and .toJava.
 * 
 * In the [[javaapi]] package, the same converters can be found with a
 * java-friendly interface that don't rely on implicit enrichments.
 * 
 * Additionally, this package offers [[Accumulator]]s, capable of efficiently
 * traversing JDK Streams.
 **/
package object jdk
