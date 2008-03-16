/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.matching

/** This is a wrapper around integer. Used only by BitFields,
 *  it makes it possible to differentiate fixed-size fields from
 *  variable-size ones.
 *
 *  @author  Thibaud Hottelier
 *  @version 1.0, 15/12/2007
 *
 *  @param x the wrapped integer
 */
private[matching] class TaintedInt(x: Int) {

  /** Converts to Int
   *
   *  @return The wrapped integer
   */
  def toInt = x

  /** Adds two TaintedInts
   *
   *  @param  y The second operand
   *  @return   The sum
   */
  def + (y: TaintedInt) = new TaintedInt(x + y.toInt)

  /** Subtracts two TaintedInts
   *
   *  @param  y The second operand
   *  @return   The difference
   */
  def - (y: TaintedInt) = new TaintedInt(x - y.toInt)

  /** Multiplies two TaintedInts
   *
   *  @param  y The second operand
   *  @return   The product
   */
  def * (y: TaintedInt) = new TaintedInt(x * y.toInt)

  /** Divides two TaintedInts
   *
   *  @param  y The second operand
   *  @return   The quotient
   */
  def / (y: TaintedInt) = new TaintedInt(x / y.toInt)
}
