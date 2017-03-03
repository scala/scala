object Test {
  def mkNumbers(x: Int): List[AnyRef] = {
    //Use explicit AnyRef to workaround known limitation of type inference with F-Bounds
    val base = List[AnyRef](
      BigDecimal(x),
      BigInt(x),
      new java.lang.Double(x.toDouble),
      new java.lang.Float(x.toFloat),
      new java.lang.Long(x.toLong),
      new java.lang.Integer(x)
    )
    val extras = List(
      if (x >= Short.MinValue && x <= Short.MaxValue) List(new java.lang.Short(x.toShort)) else Nil,
      if (x >= Byte.MinValue && x <= Byte.MaxValue) List(new java.lang.Byte(x.toByte)) else Nil,
      if (x >= Char.MinValue && x <= Char.MaxValue) List(new java.lang.Character(x.toChar)) else Nil
    ).flatten

    base ::: extras
  }

  def mkNumbers(x: BigInt): List[AnyRef] = {
    List(
      List(BigDecimal(x, java.math.MathContext.UNLIMITED)),
      List(x),
      if (x.isValidDouble) List(new java.lang.Double(x.toDouble)) else Nil,
      if (x.isValidFloat) List(new java.lang.Float(x.toFloat)) else Nil,
      if (x.isValidLong) List(new java.lang.Long(x.toLong)) else Nil,
      if (x.isValidInt) List(new java.lang.Integer(x.toInt)) else Nil,
      if (x.isValidShort) List(new java.lang.Short(x.toShort)) else Nil,
      if (x.isValidByte) List(new java.lang.Byte(x.toByte)) else Nil,
      if (x.isValidChar) List(new java.lang.Character(x.toChar)) else Nil
    ).flatten
  }

  // Don't necessarily expect BigDecimal created from BigInt to agree with Double here.
  def isIffy(x: Any, y: Any, canSwap: Boolean = true): Boolean = x match {
    case bd: BigDecimal => y match {
      case _: Float | _: Double => bd.toString.length > 15
      case _ => false
    }
    case _ => canSwap && isIffy(y, x, false)
  }

  // Don't necessarily expect BigInt to agree with Float/Double beyond a Long
  def isIffyB(x: Any, y: Any, canSwap: Boolean = true): Boolean = x match {
    case bi: BigInt => y match {
      case _: Float | _: Double => bi < Long.MinValue || bi > Long.MaxValue
      case _ => false
    }
    case _ => canSwap && isIffyB(y, x, false)
  }

  def main(args: Array[String]): Unit = {
    val ints    = (0 to 15).toList map (Short.MinValue >> _)
    val ints2   = ints map (x => -x)
    val ints3   = ints map (_ + 1)
    val ints4   = ints2 map (_ - 1)

    val setneg1 = ints map mkNumbers
    val setneg2 = ints3 map mkNumbers
    val setpos1 = ints2 map mkNumbers
    val setpos2 = ints4 map mkNumbers
    val zero = mkNumbers(0)

    val sets = setneg1 ++ setneg2 ++ List(zero) ++ setpos1 ++ setpos2

    for (set <- sets ; x <- set ; y <- set) {
      assert(x == y, "%s/%s != %s/%s".format(x, x.getClass, y, y.getClass))
      assert(x.## == y.##, "%s != %s".format(x.getClass, y.getClass))
    }

    val bigInts  = (0 to 1024).toList map (BigInt(-1) << _)
    val bigInts2 = bigInts map (x => -x)
    val bigInts3 = bigInts map (_ + 1)
    val bigInts4 = bigInts2 map (_ - 1)

    val setneg1b = bigInts map mkNumbers
    val setneg2b = bigInts3 map mkNumbers
    val setpos1b = bigInts2 map mkNumbers
    val setpos2b = bigInts4 map mkNumbers

    val sets2 = setneg1 ++ setneg1b ++ setneg2 ++ setneg2b ++ List(zero) ++ setpos1 ++ setpos1b ++ setpos2 ++ setpos2b

    for (set <- sets2 ; x <- set ; y <- set) {
      if (!isIffy(x,y)) {
        assert(x == y, "%s/%s != %s/%s".format(x, x.getClass, y, y.getClass))
        // The following is blocked by SI-8150
        // if (!isIffyB(x,y)) assert(x.## == y.##, "%x/%s != %x/%s from %s.## and %s.##".format(x.##, x.getClass, y.##, y.getClass, x, y))
      }
    }
  }
}
