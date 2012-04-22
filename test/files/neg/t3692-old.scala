import java.lang.Integer

object ManifestTester {
  def main(args: Array[String]) = {
    val map = Map("John" -> 1, "Josh" -> 2)
    new ManifestTester().toJavaMap(map)
  }
}

class ManifestTester {
  private final def toJavaMap[T, V](map: Map[T, V])(implicit m1: Manifest[T], m2: Manifest[V]): java.util.Map[_, _] = {
    map match {
      case m0: Map[Int, Int] => new java.util.HashMap[Integer, Integer]
      case m1: Map[Int, V] => new java.util.HashMap[Integer, V]
      case m2: Map[T, Int] => new java.util.HashMap[T, Integer]
      case _ => new java.util.HashMap[T, V]
    }
  }
}