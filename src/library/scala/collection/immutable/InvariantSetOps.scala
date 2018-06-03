package scala
package collection
package immutable

import scala.language.higherKinds

trait InvariantSetOps[A, +CC[X] <: InvariantSetOps[X, CC, _] with Set[X], +C <: InvariantSetOps[A, CC, C] with CC[A]]
  extends collection.InvariantSetOps[A, CC, C]
     with SetOps[A, Set, C] {
}
