/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:$


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
