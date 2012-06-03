import scala.reflect.base.{Universe => BaseUniverse}
import scala.reflect.{basis => rb}
import scala.reflect.runtime.{universe => ru}

object Test extends App {
  println(rb.typeOf[List[Int]])
  println(ru.typeOf[List[Int]])
  def foo[T: rb.TypeTag] = {
    println(rb.typeOf[T])
    println(ru.typeOf[T])
    println(implicitly[BaseUniverse#TypeTag[T]])
  }
  foo[Map[String, String]]
}