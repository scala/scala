/*
 * filter: It would fail on the following input
 */
object Test extends App {
  virtMatch()
  def virtMatch() = {
    List(1, 3, 4, 7) match {
      case 1 :: 3 :: 4 :: 5 :: x => println("nope")
      case 1 :: 3 :: 4 :: 6 :: x => println("nope")
      case 1 :: 3 :: 4 :: 7 :: x => println(1)
    }
  }
}
