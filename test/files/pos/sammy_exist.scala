// scala> typeOf[java.util.stream.Stream[_]].nonPrivateMember(TermName("map")).info
// [R](x$1: java.util.function.Function[_ >: T, _ <: R])java.util.stream.Stream[R]

// java.util.function.Function
trait Fun[A, B] { def apply(x: A): B }

// java.util.stream.Stream
class S[T](x: T) { def map[R](f: Fun[_ >: T, _ <: R]): R = f(x) }

class Bla { def foo: Bla = this }

object T {
  val aBlaSAM = (new S(new Bla)).map(_.foo) // : Bla should be inferred, when running under -Xexperimental [TODO]
  val fun: Fun[Bla, Bla] = (x: Bla) => x
  val aBlaSAMX = (new S(new Bla)).map(fun) // : Bla should be inferred, when running under -Xexperimental [TODO]
}
//
// // or, maybe by variance-cast?
// import annotation.unchecked.{uncheckedVariance => uv}
// type SFun[-A, +B] = Fun[_ >: A, _ <: B @uv]
//
// def jf[T, R](f: T => R): SFun[T, R] = (x: T) => f(x): R
//
// val aBlaSAM = (new S(new Bla)).map(jf(_.foo)) // : Bla should be inferred [type checks, but existential inferred]