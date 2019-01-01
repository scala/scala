import scala.language.higherKinds

class Base[M[_]] { def mint: M[Int] = ??? }
final case class Sub() extends Base[Option]

object Test {
  def test[M[_]](b: Base[M]): Option[Int] = {
    bar: M[Any]
    b match {
      case Sub() =>
        bar: M[Any] // incorrect error: M does not take type parameters
        b.mint // GADT refinement: M = Option

      // Fix: change GADT refinement for HK type params to assign M the type:
      // [A]( >: Option[A] <: Option[A]),
      // rather than:
      // >: Option <: Option
    }
  }

  def bar[N[_]]: N[Any] = ???
}
