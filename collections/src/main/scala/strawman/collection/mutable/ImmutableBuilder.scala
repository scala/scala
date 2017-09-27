package strawman
package collection
package mutable

import scala.Unit

/**
  * Reusable builder for immutable collections
  */
abstract class ImmutableBuilder[-A, C](empty: C)
  extends ReusableBuilder[A, C] {

  protected var elems: C = empty

  def clear(): Unit = { elems = empty }

  def result(): C = elems

}