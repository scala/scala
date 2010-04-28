object Test {
  def main(args: Array[String]): Unit = {
    Iterator.iterate(1 to 5 toList)(_.tail).takeWhile(_.nonEmpty).map(_.head).toList
  }
}
