trait RepeatedSink { def accept(a: Any*): Unit }

object Test {
  def main(args: Array[String]): Unit = {
    val f: RepeatedSink = (a) => println(a)
    f.accept(1)
  }
}