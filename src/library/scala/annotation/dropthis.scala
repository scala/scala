/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.annotation

/** This annotation can be used when referencing `this` in a method to indicate that there are no further references
  * to `this` later. The compiler will generate code to allow `this` to be garbage-collected.
  *
  * It is an error to use this annotation on anything other than a direct reference to the directly enclosing
  * class (`this`) or to require further references at a later point in the method.
  *
  * @since 2.13.1
  */
final class dropthis extends scala.annotation.StaticAnnotation
