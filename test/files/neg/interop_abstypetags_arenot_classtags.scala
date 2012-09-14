import scala.reflect.runtime.universe._
import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def weakTypeTagIsnotClassTag[T: WeakTypeTag] = {
    println(classTag[T])
  }

  weakTypeTagIsnotClassTag[Int]
  weakTypeTagIsnotClassTag[String]
  weakTypeTagIsnotClassTag[Array[Int]]
}