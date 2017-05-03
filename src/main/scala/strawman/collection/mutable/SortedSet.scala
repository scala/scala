package strawman
package collection.mutable

import scala.Ordering

trait SortedSet[A]
  extends collection.SortedSet[A]
    with Set[A]
