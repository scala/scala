object Const {
  // The values will be incremented in the next compilation group.
  // We won't recompile the client.
  final val intConst = 1
  final val int: Int = 1
}
object Test {
  import Const._

  def main(args: Array[String]) {
    assert((intConst, int) == (1, 2))
  }
}
