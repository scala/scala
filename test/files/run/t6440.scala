object Test {

  def main(args: Array[String]): Unit = {
    println(Stream.continually(()).filterNot(_ => false).take(2))
  }

}
