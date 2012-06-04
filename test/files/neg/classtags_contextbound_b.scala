import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def mkArray[T: ClassTag] = Array[T]()
  def foo[T] = mkArray[T]
  println(foo[Int].getClass)
}