import scala.reflect.runtime.universe._
import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def concreteTypeTagIsnotClassTag[T: ConcreteTypeTag] = {
    println(classTag[T])
  }

  concreteTypeTagIsnotClassTag[Int]
  concreteTypeTagIsnotClassTag[String]
  concreteTypeTagIsnotClassTag[Array[Int]]
}