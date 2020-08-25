// scalac: -Ydelambdafy:method-ref
object Test {
  def main(args: Array[String]): Unit = {
    List(Option("a")).map(_.map(_.toUpperCase))
  }
}
