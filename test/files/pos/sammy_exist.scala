// scala> typeOf[java.util.stream.Stream[_]].nonPrivateMember(TermName("map")).info
// [R](x$1: java.util.function.Function[_ >: T, _ <: R])java.util.stream.Stream[R]

// java.util.function.Function
trait Fun[A, B] { def apply(x: A): B }

// java.util.stream.Stream
class S[T](x: T) { def map[R](f: Fun[_ >: T, _ <: R]): R = f(x) }

class Bla { def foo: Bla = this }

// NOTE: inferred types show unmoored skolems, should pack them to display properly as bounded wildcards
object T {
  val aBlaSAM = (new S(new Bla)).map(_.foo)
  val fun: Fun[Bla, Bla] = (x: Bla) => x
  val aBlaSAMX = (new S(new Bla)).map(fun)
}
