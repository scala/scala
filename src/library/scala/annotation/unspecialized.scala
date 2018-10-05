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

/** A method annotation which suppresses the creation of
 *  additional specialized forms based on enclosing specialized
 *  type parameters.
 *
 *  @since 2.10
 */
class unspecialized extends scala.annotation.StaticAnnotation
