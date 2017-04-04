package strawman
package collection.mutable

import scala.Unit

/**
  * Reusable builder for immutable collections
  */
abstract class ImmutableBuilder[-Elem, Coll](empty: Coll)
  extends ReusableBuilder[Elem, Coll] {

  protected var elems: Coll = empty

  def clear(): Unit = { elems = empty }

  def result(): Coll = elems

}
