package tastytest

object HelloWorld {
  val msg1: String = "Hello, World!"
  def msg2: String = "Hello, World!"
  final val msg3 = "Hello, World!"
  val ints: List[Int] = List(1,2,3)
  val one: 1 = 1
  def inferred[A: Numeric](a: A) = the[Numeric[A]].plus(a, a)
  def the[T](implicit x: T): x.type = x
  def bounded[T >: Null <: String](a: T): String = a + a
  def higher[F[_], G[_]](fInt: F[Int])(implicit ev: F[Int] <:< G[Int]): G[Int] = ev(fInt)
  def higherBounded[F[_] >: Null <: List[_], A](f: F[A]): F[A] = f
  val func: Int => String = _.toString
  def func1[A]: A => A = x => x
  final lazy val lzy = "lazy"
  // def `<init>`: Int = 157 // broken in https://github.com/lampepfl/dotty/issues/7799
}
