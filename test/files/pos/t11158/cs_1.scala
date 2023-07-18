
// not computer science but the basis of the entire field, the character sequence
class cs extends CharSeqJava {
  private var n = 42
  override def length: Int = n
  def length_=(value: Int): Unit = n = value
}

object Resetter extends App {
  val cs = new cs
  cs.length = 0
  assert(cs.length == 0)
}
