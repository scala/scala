abstract class RedBlack[A] extends Serializable {
  abstract class Tree[+B] extends Serializable
  case object Empty extends Tree[Nothing]
}

object Test {
  def main(args: Array[String]): Unit = {
    val r = classOf[RedBlack[_]].getMethod("Empty").getGenericReturnType.toString
    // Output changed in JDK 1.8.0_172: https://github.com/scala/bug/issues/10835
    assert(r == "RedBlack<A>.Empty$" || r == "RedBlack<A>$Empty$", r)
  }
}
