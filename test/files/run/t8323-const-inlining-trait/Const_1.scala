trait Const {
  // The values will be incremented in the next compilation group.
  // We won't recompile the client.
  final val intConst = 1
  final val int: Int = 1
}
object Test extends Const {
  def main(args: Array[String]) {
    assert((intConst, int) == (1, 2))
    val setters = getClass.getMethods.map(_.getName).filter(_.contains("_setter_")).sorted.toList
    assert(setters == List("Const$_setter_$int_$eq"), setters)
  }
}
