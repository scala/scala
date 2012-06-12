import scala.reflect.runtime.universe._
import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def typeTagIsnotClassTag[T: TypeTag] = {
    println(classTag[T])
  }

  typeTagIsnotClassTag[Int]
  typeTagIsnotClassTag[String]
  typeTagIsnotClassTag[Array[Int]]
}