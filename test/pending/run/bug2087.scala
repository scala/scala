object Test {
  def main(args: Array[String]): Unit = {
    val s: Short = 0xFA99.toShort
    val c: Char = 0xFA99.toChar
    
    assert((s == c) == (c == s))
  }
}