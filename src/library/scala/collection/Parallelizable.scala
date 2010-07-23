package scala.collection



import parallel.ParIterableLike



/** This trait describes collections which can be turned into parallel collections
 *  by invoking the method `par`. Parallelizable collections may be parametrized with
 *  a target type different than their own.
 */
trait Parallelizable[+ParRepr <: Parallel] {

  /** Returns a parallel implementation of a collection.
   */
  def par: ParRepr

}



















