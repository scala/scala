abstract class RedBlack[A] extends Serializable {
  abstract class Tree[+B] extends Serializable
  case object Empty extends Tree[Nothing]
}

object Test {
  def main(args: Array[String]): Unit = {
    println(classOf[RedBlack[_]].getMethod("Empty").getGenericReturnType)
  }
}
