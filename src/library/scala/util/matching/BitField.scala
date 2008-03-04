package util.matching

import scala.collection.immutable._

/** This class provides methods for creating and using BitFields.
 *  BitFields can easily parse binary data.
 *
 *  @author  Thibaud Hottelier
 *  @version 1.0, 22/12/2007
 *
 *  @param fields the list of pair of name and size for each field.
 */
class BitField(fields: List[Product2[String, TaintedInt]]) extends FixedBitField {
  /** Create a BitField
   *  @param vf pair of name and size for each field.
   */
  def this(vf: Product2[String, TaintedInt]*) = this(vf.toList)

  /* Get the position of the first variable-size field */
  val f = fields.zip(fields.indices) find {p => p._1.isInstanceOf[DelayedPair[_, _]]}
  val varPos = if (f.isDefined) f.get._2 else fields.size

  /* Split the fixed part and the variable part of fields */
  val fixedFields = fields.slice(0, varPos)
  val variableFields = fields.slice(varPos, fields.size)

  /* Create a BitField for the fixed part */
  val fixedPart = new FixedBitField(fixedFields.map(p => Pair(p._1, p._2.toInt)))

  /** Helper that parses binary data into a MatchData object */
  private def unapplySeq0(input: Array[Byte]): MatchData[BigInt] = {
    /* Extract the fixed length and the variable length part of the input */
    val max = Math.min(fixedPart.size / 8, input.size)
    val finput = input.subArray(0, max)
    val vinput = input.subArray(max, input.size)

    /* Parse the fixed length part */
    val fresult = fixedPart.parse(finput)
    BitField.setRes(fresult)

    if (varPos == fields.size)
      fresult
    else {
      /* Parse the variable length part */
      val variablePart = setVariableFields
      val vresult = variablePart.parse(vinput)
      new MatchData[BigInt](BigInt(input) :: vresult.getGroups.tail ::: fresult.getGroups.tail,
                            fresult.getNames ++ vresult.getNames)
    }
  }

  /** Performs the matching.
   *
   *  @param  a the array to match
   *  @return   the contents of each field
   */
  override def parse(a: Array[Byte]): MatchData[BigInt] = unapplySeq0(a)

  /** Matches an array of bytes or a MatchData[BigInt] instance.
   *
   *  @param target  the value to be matched. Has to be an Array[Byte]
   *                 or an MatchData[BigInt] instance.
   *  @return        the contents of the fields.
   */
  override def unapplySeq(target: Any): Option[List[BigInt]] = {
    if (target.isInstanceOf[Array[Byte]])
      Some(unapplySeq0(target.asInstanceOf[Array[Byte]]).getGroups.tail.reverse)
    else if (target.isInstanceOf[MatchData[_]])
      Some(unapplySeq0(target.asInstanceOf[MatchData[BigInt]]().toByteArray).getGroups.tail.reverse)
    else
      None
  }

  /** This operator is used in for-comprehensions to iterate over matches.
   *
   * @param  a the data to match
   * @return   the result of the matching
   */
  override def ~~ (a: Array[Byte]) = split(a)

  /* Helper that splits up binary data and matches each block one by one */
  private def split(input: Array[Byte]): List[MatchData[BigInt]] = {
    def split0(res: List[Array[Byte]], in: Array[Byte]): List[Array[Byte]] = {
      val fin = input.subArray(0, fixedPart.size / 8)
      val fres = fixedPart.parse(fin)
      BitField.setRes(fres)
      val vp = setVariableFields
      val totalSize = (vp.size + fixedPart.size) / 8
      if (in.length <= totalSize)
        in :: res
      else
        split0(in.slice(0, totalSize) :: res, in.slice(totalSize, in.length))
    }
    split0(Nil, input).reverse map {a => unapplySeq0(a)}
  }

  /* Returns a FixedBitField that represents the variable-size fields */
  private def setVariableFields: FixedBitField = {
    var fieldsSize: List[(String, Int)] = Nil
    variableFields.reverse foreach { e => fieldsSize = (e._1, e._2.toInt) :: fieldsSize }
    new FixedBitField(fieldsSize)
  }
}

/** Contains implicit conversions and helper functions */
object BitField {
  val fresult = new ThreadLocal[MatchData[BigInt]]

  private def setRes(fresult: MatchData[BigInt]) = this.fresult.set(fresult)

  /** Use this function to reference the contents of a field previously defined when
   *  defining the size of the current field.
   *
   *  @param label the name of field
   *  @return      the contents of the field when matching
   */
  def field(label: String): TaintedInt = {
    new TaintedInt(fresult.get.asInstanceOf[MatchData[BigInt]].apply(label).intValue)
  }

  /** Creates a new BitField
   *
   *  @param  vf (name, size) pairs for each field
   *  @return    the new BitField instance.
   */
  def apply(vf: Product2[String, TaintedInt]*) = new BitField(vf.toList)

  implicit def int2TaintedInt(x: Int): TaintedInt = new TaintedInt(x)

  implicit def any2DArrowAssoc[A](x: A): DArrowAssoc[A] = new DArrowAssoc(x)

  implicit def IntToMatchableInt(s: Array[Byte]): MatchableBigInt = new MatchableBigInt(s)

  implicit def IntoptionToBoolean(o: Option[MatchData[BigInt]]): Boolean = o match {
    case None => false
    case Some(_) => true
  }
}
