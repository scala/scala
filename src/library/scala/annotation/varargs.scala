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

/** A method annotation which instructs the compiler to generate a
 *  Java varargs-style forwarder method for interop. This annotation can
 *  only be applied to methods with repeated parameters.
 *
 *  @since 2.9
 */
final class varargs extends scala.annotation.StaticAnnotation
