// A is a class, so W does not conform to A in bytecode. an access (w: W).m() requires a cast to A.
trait W extends A
class C extends W

object Test {
  def main(args: Array[String]): Unit = {
    val w: W = new C
    assert(w.m() == 1)
  }
}
