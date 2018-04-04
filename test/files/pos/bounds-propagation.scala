import scala.language.higherKinds

object Test {
  def foo[A <: List[B], B](x: A): Option[B] = None
  val res0 = foo(List(0))
  val res1: Option[Int] = res0

  def bar[A <: F[Int], F[_]](x: A): Option[F[Boolean]] = None
  val res2 = bar(List(0))
  val res3: Option[List[Boolean]] = res2
}
