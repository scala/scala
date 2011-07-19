object Test {
  def main(args: Array[String]): Unit = {
    println(classOf[scala.collection.immutable.RedBlack[_]].getMethod("Empty").getGenericReturnType)
  }
}
