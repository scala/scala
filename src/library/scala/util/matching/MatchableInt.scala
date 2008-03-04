package util.matching

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
