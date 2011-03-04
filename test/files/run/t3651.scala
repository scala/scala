class Klass[@specialized(Long) A]( val a: A )

class LongKlass( override val a: Long ) extends Klass[Long](a)

object Main {
  def main(args: Array[String]) {
    val lk = new LongKlass(10)
    lk.a
  }
}
