import scala.language.existentials
import scala.reflect.ClassTag

object Test extends App {
  println(implicitly[ClassTag[List[T forSome {type T <: List[T]}]]])
  println(implicitly[ClassTag[List[Any]]])
}
