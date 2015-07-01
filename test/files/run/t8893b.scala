// Testing that recursive calls in tail positions are replaced with
// jumps, even though the method contains recursive calls outside
// of the tail position.
object Test {
  def tick(i : Int): Unit =
    if (i == 0) ()
    else if (i == 42) {
      tick(0) /*not in tail position*/
      tick(i - 1)
    } else tick(i - 1)

  def main(args: Array[String]): Unit = {
    tick(1000000)
  }
}
