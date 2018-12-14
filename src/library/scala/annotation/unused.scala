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

/** An annotation that designates that the target is unused.
 *
 * This extends `@deprecated` because the compiler's unused linting ignores items annotated with
 * `@deprecated`.
 *
 * It is recommended to use this when using `-Xlint` (and/or `-Ywarn-unused`).  This is particularly
 * useful when defining the default implementation of a method without using all its parameters.
 */
final class unused extends deprecated("unused", "")
