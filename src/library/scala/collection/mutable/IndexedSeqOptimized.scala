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
package collection
package mutable

/** A subtrait of scala.collection.IndexedSeq which represents sequences
 *  that can be mutated.
 *
 *  @since 2.8
 */
trait IndexedSeqOptimized[A, +Repr] extends Any with IndexedSeqLike[A, Repr] with scala.collection.IndexedSeqOptimized[A, Repr]
