package scala.util.matching

/** This class provides methods for creating and using FixedBitFields.
 *  If you have variable size fields, see @see BitField.
 *
 *  @author  Thibaud Hottelier
 *  @version 1.0, 3/1/2008
 *
 *  @param f the list of (name, size) pairs
 */
class FixedBitField(f: List[Product2[String, Int]]) {

  /** Create a new FixedBitField.
   *
   *  @param groups the sequence of (name, size) pairs
   */
  def this(groups: (String, Int)*) = this(groups.toList)

  val fields = f.reverse

  val size = fields.foldLeft(0)((s, p) => s + p._2)

  // works around compiler bug (TODO: which one?)
  val t = (fields.zip(fields.indices)) map {p => (p._1._1, p._2 + 1)}
  val groupNames = Map() ++ t

  /** Performs the matching using masks */
  private def buildSeq(res: List[BigInt], a: BigInt, i: int, j: int): List[BigInt] = {
    if (i < 0)
      res
    else {
      var mask = BigInt(0)
      for (k <- List.range(1, fields(i)._2 + 1))
        mask = mask.setBit(j-k)
      buildSeq(((a & mask) >> (j - fields(i)._2))::res, a, i - 1, j - fields(i)._2)
    }
  }

  /** Match an Array[Byte] or an MatchData[BigInt] instance.
   *
   *  @param target the value to be matched. Has to be an Array[Byte]
   *                or a MatchData[BigInt] instance
   *  @return       the field contents
   */
  def unapplySeq(target: Any): Option[List[BigInt]] = {
    if (target.isInstanceOf[Array[Byte]])
      Some(buildSeq(Nil, BigInt(target.asInstanceOf[Array[Byte]]), fields.size - 1, size))
    else if (target.isInstanceOf[MatchData[_]])
      Some(buildSeq(Nil, target.asInstanceOf[MatchData[BigInt]](), fields.size - 1, size).reverse)
    else
      None
  }

  /** This operator is used in for-comprehension to iterate over matches.
   *
   * @param  a the data to be matched
   * @return   the result of the matching
   */
  def ~~ (a: Array[Byte]) = split(a)

  /* Builds a MatchData object from the raw matching results */
  private def splitOne(n: BigInt) = {
    new MatchData(n::buildSeq(Nil, n, fields.size - 1, size), groupNames)
  }

  /** Performs the matching.
   *
   *  @param  the array to be matched
   *  @return the contents of each field
   */
  def parse(a: Array[Byte]): MatchData[BigInt] = splitOne(BigInt(a))

  /** Matches and consumes the same input iteratively */
  private def split(input: Array[Byte]): List[MatchData[BigInt]] = {
    def split0(res: List[BigInt], in: Array[Byte]): List[BigInt] = {
      val bSize = size / 8
      if (in.length <= bSize)
        BigInt(in) :: res
      else {
        val x = in.slice(0, bSize)
        val xs = in.slice(bSize, in.length)
        split0(BigInt(x) :: res, xs)
      }
    }
    split0(Nil, input).reverse map {n => splitOne(n)}
  }
}
