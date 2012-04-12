import scala.reflect.mirror._

object Test extends App {
  reify {
    List(1, 2, 3) match {
      case foo :: bar :: _ => println(foo * bar)
      case _ => println("this is getting out of hand!")
    }
  }.eval
}
