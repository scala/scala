// a.scala
import scala.reflect.{ClassTag, classTag}

class A {
  type AI = Array[Int]

  def f1 = classTag[Array[Int]]
  def f2 = classTag[AI]
}