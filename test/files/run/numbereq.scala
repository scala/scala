object Test {
  def mkNumbers(x: Int): List[AnyRef] = {
    val base = List(
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
      // println("'%s' == '%s' (%s == %s) (%s == %s)".format(x, y, x.hashCode, y.hashCode, x.##, y.##))
      assert(x == y, "%s/%s != %s/%s".format(x, x.getClass, y, y.getClass))
      assert(x.## == y.##, "%s != %s".format(x.getClass, y.getClass))
    }
  }
}