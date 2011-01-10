object Test {
  def main(args: Array[String]): Unit = {
    classOf[scala.collection.immutable.RedBlack[_]].getMethod("Empty").getGenericReturnType
  }
}
