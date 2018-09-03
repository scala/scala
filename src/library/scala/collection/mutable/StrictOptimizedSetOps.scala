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

/**
  * Trait that overrides set operations to take advantage of strict builders.
  *
  * @tparam A  Elements type
  * @tparam CC Collection type constructor
  * @tparam C  Collection type
  */
trait StrictOptimizedSetOps[A, +CC[_], +C <: mutable.SetOps[A, CC, C]]
  extends mutable.SetOps[A, CC, C]
    with mutable.StrictOptimizedIterableOps[A, CC, C] {

  override def concat(that: IterableOnce[A]): C =
    strictOptimizedConcat(that, newSpecificBuilder)

}
