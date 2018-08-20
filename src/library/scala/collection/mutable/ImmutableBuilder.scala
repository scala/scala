package scala
package collection
package mutable


/**
  * Reusable builder for immutable collections
  */
abstract class ImmutableBuilder[-A, C <: AnyRef](empty: C)
  extends ReusableBuilder[A, C] {

  protected var elems: C = empty

  def clear(): Unit = {elems = empty}

  def result(): C = elems

}
