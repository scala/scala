object Test {
  def main(args: Array[String]) {
    val seq: Seq[String] = List("one", "two")
    println(java.util.Arrays.asList(seq: _*))
    println(java.util.Arrays.asList(Seq(1,2,3): _*))
  }
}
