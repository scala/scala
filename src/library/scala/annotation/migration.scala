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

package scala.annotation

/**
 * An annotation that marks a member as having changed semantics
 * between versions.  This is intended for methods which for one
 * reason or another retain the same name and type signature,
 * but some aspect of their behavior is different.  An illustrative
 * examples is Stack.iterator, which reversed from LIFO to FIFO
 * order between Scala 2.7 and 2.8.
 *
 * @param message A message describing the change, which is emitted
 * by the compiler if the flag `-Xmigration` indicates a version
 * prior to the changedIn version.
 *
 * @param changedIn The version, in which the behaviour change was
 * introduced.
 */
private[scala] final class migration(message: String, changedIn: String) extends scala.annotation.ConstantAnnotation
