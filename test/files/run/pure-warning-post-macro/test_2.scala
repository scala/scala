//> using options -Werror
object Test {
  def main(args: Array[String]): Unit = {
    // We don't want a "pure expression discarded" warning here as the macro will
    // eliminate the block
    val is = Macro.blockToList[Int] {
      1
      2
      3
    }
    assert(is == List(1, 2, 3))
  }
}
