package scala.util.matching

/** Only used by BitFields. This class is similar to ArrowAssoc.
 *  It defines the --> function that creates a pair if the
 *  argument is an Int and a DelayedPair if it is a TaintedInt.
 *
 *  @author  Thibaud Hottelier
 *  @version 1.0, 15/12/2007
 */
private[matching] class DArrowAssoc[A](x: A) {

  /** Creates a DelayedPair
   *
   *  @param  y the TaintedInt
   *  @return   the DelayedPair
   */
  def --> (y: => TaintedInt): DelayedPair[A, TaintedInt] = {
    new DelayedPair(x, () => y)
  }

  /** Creates a pair
   *
   *  @param  y the Int
   *  @return   the pair
   */
  def --> (y: Int): Product2[A, TaintedInt] = {
    (x, new TaintedInt(y))
  }
}
