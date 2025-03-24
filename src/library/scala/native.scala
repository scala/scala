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

/** Marker for native methods.
  *
  * {{{
  * @native def f(x: Int, y: List[Long]): String = ...
  * }}}
  *
  * A `@native` method is compiled to the platform's native method,
  * while discarding the method's body (if any). The body will be type checked if present.
  *
  * A method marked @native must be a member of a class, not a trait (since 2.12).
  */
@deprecatedInheritance("Scheduled for being final in the future", "2.13.0")
class native extends scala.annotation.StaticAnnotation {}
