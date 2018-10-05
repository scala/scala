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

package scala

/** This class represents uninitialized variable/value errors.
 *
 *  @author  Martin Odersky
 *  @since   2.5
 */
// TODO: remove in 2.14
@deprecated("will be removed in a future release", since = "2.12.7")
final class UninitializedError extends RuntimeException("uninitialized value")
