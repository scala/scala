/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.matching

/** This class provides methods to do matching with
 *  BitFields.
 *
 *  @author  Thibaud Hottelier
 *  @version 1.0, 12/12/2007
 */
private[matching] class MatchableBigInt(a: Array[Byte]) {

  /** Returns the first match of this array of Byte with the
   *  provided BitField.
   *
   * @param f the BitField
   * @return  the first match
   */
  def =~ (f: FixedBitField): Option[MatchData[BigInt]] = Some(f.parse(a))
}
