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

package scala.reflect
package internal
package annotations

/**
 * An annotation that designates the annotated type should not be checked for violations of
 * type parameter bounds in the `refchecks` phase of the compiler. This can be used by synthesized
 * code the uses an inferred type of an expression as the type of an artifact val/def (for example,
 * a temporary value introduced by an ANF transform). See [[https://github.com/scala/bug/issues/7694]].
 */
final class uncheckedBounds extends scala.annotation.StaticAnnotation
