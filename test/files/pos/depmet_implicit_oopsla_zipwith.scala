case class Zero()
case class Succ[N](x: N)
import Stream.{cons, continually}

trait ZipWith[N, S] {
  type T

  def manyApp: N => Stream[S] => T
  def zipWith: N => S => T = n => f => manyApp(n)(continually(f))
}
object ZipWith {
  implicit def ZeroZipWith[S] = new ZipWith[Zero, S] {
    type T = Stream[S]

    def manyApp = n => xs => xs
  }

  implicit def SuccZipWith[N, S, R](implicit zw: ZipWith[N, R]) =
    new ZipWith[Succ[N],S => R] {
      type T = Stream[S] => zw.T

      def zapp[A, B](xs: Stream[A => B], ys: Stream[A]): Stream[B] = (xs, ys) match {
        case (cons(f, fs), cons(s, ss)) => cons(f(s),zapp(fs, ss))
        case (_, _) => Stream.empty
      }

      def manyApp = n => xs => ss => n match {
        case Succ(i) => zw.manyApp(i)(zapp(xs, ss))
      }
    }
}

object Test {
  def zWith[N, S](n: N, s: S)(implicit zw: ZipWith[N, S]): zw.T = zw.zipWith(n)(s)

  def zipWith0: Stream[Int] = zWith(Zero(),0)

// (Stream[A]) => java.lang.Object with ZipWith[Zero,B]{type T = Stream[B]}#T
// should normalise to: Stream[A] => Stream[B]
  def map[A, B](f: A => B) = zWith(Succ(Zero()),f)

  def zipWith3[A, B, C, D](f: A => B => C => D) = //: Stream[A] => Stream[B] => Stream[C] => Stream[D] = // BUG why do we need a return type?
    zWith(Succ(Succ(Succ(Zero()))),f)
}