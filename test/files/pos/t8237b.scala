import scala.language.higherKinds
import scala.reflect.runtime.universe._
object Test {

  def fTt[A,E[X]<:List[X]](a: A)(implicit tt: TypeTag[E[A]]) = a

  trait TC[A]
  implicit def TCListInt[A]: TC[A] = ???
  fTt(1)
}
