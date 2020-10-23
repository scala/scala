object Test {
  sealed abstract class Base[T] {
    def v: T
  }
  class ImplI extends Base[Int] {
    def v = 42
  }
  object ImplI {
    def unapply(x: ImplI) = Some(x.v)
  }
  class ImplD extends Base[Double] {
    def v = 42.0
  }
  object ImplD {
    def unapply(x: ImplD) = Some(x.v)
  }

  def foo[T](x: Base[T], labels: Seq[T]): Unit = {
    val last: T = labels.last // ClassCastException

    x match {
      case ImplD(v) => println(v)
      case ImplI(v) => println(v)
    }
  }

  def main(args: Array[String]): Unit =
    foo(new ImplI(), Seq(1, 2, 3))
}
