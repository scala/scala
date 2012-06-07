import scala.reflect.runtime.universe._
import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def absTypeTagIsnotClassTag[T: AbsTypeTag] = {
    println(classTag[T])
  }

  absTypeTagIsnotClassTag[Int]
  absTypeTagIsnotClassTag[String]
  absTypeTagIsnotClassTag[Array[Int]]
}