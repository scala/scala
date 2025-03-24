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

package scala.collection

/**
  * Trait that overrides set operations to take advantage of strict builders.
  *
  * @tparam A  Elements type
  * @tparam CC Collection type constructor
  * @tparam C  Collection type
  */
trait StrictOptimizedSetOps[A, +CC[_], +C <: SetOps[A, CC, C]]
  extends SetOps[A, CC, C]
    with StrictOptimizedIterableOps[A, CC, C] {

  override def concat(that: IterableOnce[A]): C =
    strictOptimizedConcat(that, newSpecificBuilder)

}
