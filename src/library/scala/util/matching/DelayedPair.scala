package util.matching

/** A DelayedPair is similar to a pair, except that the second
 *  argument is delayed.
 *
 *  @author  Thibaud Hottelier
 *  @version 1.0, 15/12/2007
 *
 *  @param a the first element
 *  @param b the second element
 */
class DelayedPair[A, B](a: A, b: () => B) extends Product2[A, B] {
  override def _1: A = a
  override def _2: B = b()
}
