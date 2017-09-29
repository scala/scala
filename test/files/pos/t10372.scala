import scala.language.higherKinds
import scala.language.implicitConversions

object Test {
  class Expected[T, Func[_]]
  implicit def conv[T, Func[_]](i : Int) : Expected[T, Func] = ???
  type FuncId[T] = T

  object DoesNotCompile {
    class Bla {
      type Alias[T] = Expected[T, FuncId]
      def bla[T](expected : Alias[T]) : Unit = {}
    }
    (new Bla).bla(2)
  }
}
