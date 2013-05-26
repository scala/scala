class Klass[@specialized(Long) A]( val a: A )

class LongKlass( override val a: Long ) extends Klass[Long](a)

object Test {
  def main(args: Array[String]) {
    val lk = new LongKlass(10)
    val a = lk.a
  }
}
