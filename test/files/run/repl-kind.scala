import scala.tools.partest.ReplTest

object Test extends ReplTest {
  override def extraSettings = "-feature -language:_"

  def code = """
:kind Int
:kind scala.Option
:k (Int, Int) => Int
:k -v Either
:k -v scala.collection.generic.ImmutableSortedMapFactory
:kind -v Predef.Pair
:kind -v Predef.Pair[Int, Int]
trait Functor[F[_]] {}
:kind -v Functor
object Bar { class Bop }
import Bar.Bop
:kind Bop
type IntTuple[+A] = (Int, A)
:kind IntTuple
:kind 5
:kind Nonexisting
  """.trim
}
