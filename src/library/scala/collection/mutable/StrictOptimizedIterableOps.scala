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

trait StrictOptimizedIterableOps[+A, +CC[_], +C]
  extends collection.StrictOptimizedIterableOps[A, CC, C] {

  override def concat[B >: A](suffix: IterableOnce[B]) =
    strictOptimizedConcat(suffix, iterableFactory.newBuilder[B])

}