package scala
package collection

import scala.language.higherKinds

trait InvariantSetOps[A, +CC[X] <: InvariantSetOps[X, CC, _] with Set[X], +C <: InvariantSetOps[A, CC, C] with CC[A]]
  extends SetOps[A, Set, C] {
}
