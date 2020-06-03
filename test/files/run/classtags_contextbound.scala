import scala.reflect.ClassTag

object Test extends App {
  def mkArray[T: ClassTag] = Array[T]()
  def foo[T: ClassTag] = mkArray[T]
  println(foo[Int].getClass)
}
