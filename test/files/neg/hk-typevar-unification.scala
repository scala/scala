class A
class B
trait TC[F[_ <: A]]
class Foo[_ <: B]

object Test {

  def f[F[ _]](tc: TC[F]): Unit = ()
  def g[F[+_]](tc: TC[F]): Unit = ()

  val tcFoo: TC[Foo] = new TC[Foo] {}

  // incompatible bounds
  f(tcFoo)

  // incompatible variance
  g(tcFoo)
}
