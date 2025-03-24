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

// GENERATED CODE: DO NOT EDIT.


package scala.jdk

/** This object provides extension methods that convert between Scala and Java function types.
  *
  * When writing Java code, use the explicit conversion methods defined in
  * [[javaapi.FunctionConverters]] instead.
  *
  * Using the `.asJava` extension method on a Scala function produces the most specific possible
  * Java function type:
  *
  * {{{
  *   scala> import scala.jdk.FunctionConverters._
  *   scala> val f = (x: Int) => x + 1
  *
  *   scala> val jf1 = f.asJava
  *   jf1: java.util.function.IntUnaryOperator = ...
  * }}}
  *
  * More generic Java function types can be created using the corresponding `asJavaXYZ` extension
  * method:
  *
  * {{{
  *   scala> val jf2 = f.asJavaFunction
  *   jf2: java.util.function.Function[Int,Int] = ...
  *
  *   scala> val jf3 = f.asJavaUnaryOperator
  *   jf3: java.util.function.UnaryOperator[Int] = ...
  * }}}
  *
  * Converting a Java function to Scala is done using the `asScala` extension method:
  *
  * {{{
  *   scala> List(1,2,3).map(jf2.asScala)
  *   res1: List[Int] = List(2, 3, 4)
  * }}}
  */
object FunctionConverters extends Priority0FunctionExtensions
