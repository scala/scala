// final case class NonZeroLong(value: Long) extends AnyVal {
//   def get: Long = value
//   def isEmpty: Boolean = get == 0l
// }

class NonZeroLong(val value: Long) extends AnyVal {
  def get: Long = value
  def isEmpty: Boolean = get == 0l
}
object NonZeroLong {
  def unapply(value: Long): NonZeroLong = new NonZeroLong(value)
}


object Foo {
  def unapply(x: Int): NonZeroLong = new NonZeroLong(1L << x)
  // public long unapply(int);
  //        0: lconst_1
  //        1: iload_1
  //        2: lshl
  //        3: lreturn
}

object Test {
  def f(x: Int): Int = x match {
    case Foo(1024l) => 1
    case _          => 2
  }
  def main(args: Array[String]): Unit = {
    println(f(10))
    println(f(11))
  }
}
