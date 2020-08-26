// scalac: -Ydelambdafy:method-ref
object Test {
  def main(args: Array[String]): Unit = {
    val I_value: demo.I => String = x => x.value()
  }
}
