case class Elem[T](x: T, y: T)

object Test {
  def unrolled[T](x: Any, y: Any, z: Any) = (x, y, z) match {
    case (el: Elem[_], el.x, el.y) => true
    case _                         => false
  }

  def main(args: Array[String]): Unit = {
    println(unrolled(Elem("bippy", 5), "bippy", 6))
    println(unrolled(Elem("bippy", 5), "bippy", 5))
  }
}
