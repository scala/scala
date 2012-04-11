import scala.reflect.mirror._

object Test extends App {
  def fooNoTypeTag[T] = {
    println(implicitly[GroundTypeTag[T]])
    println(implicitly[GroundTypeTag[List[T]]])
  }
  fooNoTypeTag[Int]
}