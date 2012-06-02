import scala.reflect.{ClassTag, classTag}
import java.lang.Integer

object Tester {
  def main(args: Array[String]) = {
    val map = Map("John" -> 1, "Josh" -> 2)
    new Tester().toJavaMap(map)
  }
}

class Tester {
  private final def toJavaMap[T, V](map: Map[T, V])(implicit m1: ClassTag[T], m2: ClassTag[V]): java.util.Map[_, _] = {
    map match {
      case m0: Map[Int, Int] => new java.util.HashMap[Integer, Integer]
      case m1: Map[Int, V] => new java.util.HashMap[Integer, V]
      case m2: Map[T, Int] => new java.util.HashMap[T, Integer]
      case _ => new java.util.HashMap[T, V]
    }
  }
}