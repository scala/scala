import language.higherKinds

object Test {
  trait Monad[M[_]] {
    def foo[A](ma: M[A])(f: M[A] => Any) = f(ma)
  }
  implicit def function1Covariant[T]: Monad[({type l[a] = (T => a)})#l] =
    new Monad[({type l[a] = (T => a)})#l] {}

  def main(args: Array[String]) {
    // inference of T = (=> Any) here was outlawed by SI-7899 / 8ed7099
    // but this pattern is used in Scalaz in just a few places and caused
    // a regression.
    //
    // Inference of a by-name type doesn't *always* lead to a ClassCastException,
    // it only gets there if a method in generic code accepts a parameter of
    // that type.
    //
    // We need to introduce the stricter inference rules gradually, probably
    // with a warning.
    val m = implicitly[Monad[({type f[+x] = (=> Any) => x})#f]]
    assert(m.foo[Int]((x => 0))(f => f(???)) == 0)
  }
}
