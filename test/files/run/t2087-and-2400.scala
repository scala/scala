object Test
{
  def negativeCharMaker = new (Short => Char) { def apply(x: Short) = x.toChar }
  def main(args: Array[String]): Unit = {
    // throws exception if -100 gets to Character.valueOf
    val x = negativeCharMaker(-100)

    // chars are unsigned, they should never be equal to negative values
    assert((-100).toShort != (-100).toChar)
    assert((-100).toChar != (-100).toShort)
    assert((-100).toChar != (-100).toByte)
    assert((-100).toByte != (-100).toChar)

    // BoxesRunTime must agree as well
    assert(((-100).toShort: Any) != (-100).toChar)
    assert(((-100).toChar: Any) != (-100).toShort)
    assert(((-100).toChar: Any) != (-100).toByte)
    assert(((-100).toByte: Any) != (-100).toChar)
  }
}
