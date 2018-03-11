object Test {
  def f[T](x1: Set[T]) = () => new {
    def apply(x2: Set[_ <: T]) = List(x1, x2)
  }


  class Matcher[X]
  object Problem2 {
    def allOf[X](x: Matcher[_ >: X], y: Matcher[_ >: X]) = new Matcher[X]
    def allOf[X](x: Matcher[_ >: X]*) = new Matcher[X]
  }

  def equalTo[X](x: X) = new Matcher[X]
  val a = equalTo("g")
  val b = Problem2.allOf(a)
  val c = Problem2.allOf(a,a)


  class JObserver[T]
  class JSubscriber[T] extends JObserver[T]
  class Converted
  implicit def convertSubscriber[T](s: JSubscriber[_ >: T]): Converted = ???
  implicit def convertObserver[T](s: JObserver[_ >: T]): Converted = ???
  val jSubscriber: JSubscriber[_ >: Int] = ???
  val conv: Converted = jSubscriber


  sealed trait Foo[A]
  case class Bar[A](f: A) extends Foo[A]
  object Extractor {
    def unapply[A](f: Bar[_ >: A]): Option[A] = ???
  }

  type X = Int
  val t: Foo[X] = ???
  t match {
    case Extractor(f) => f
    case _            => ???
  }


  class F[+T]
  class I[T]
  def f1[U](f: I[F[U]]) = f
  def f2[U](f: I[F[_ <: U]]) = f1(f)
}
