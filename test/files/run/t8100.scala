object Test {
  import scala.util.Try

  def main(args: Array[String]): Unit = {
    def stream = Stream.from(0).take(100000).map(n => None)
    println(Try(stream.flatten.length))
  }
}
