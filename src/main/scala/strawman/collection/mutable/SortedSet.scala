package strawman
package collection
package mutable

import scala.Ordering

trait SortedSet[A] extends Set[A] with collection.SortedSet[A] with SortedSetOps[A, SortedSet, SortedSet[A]]

trait SortedSetOps[A, +CC[X], +C <: Set[A]] extends SetOps[A, Set, C]
