object Test {
  def main(args: Array[String]): Unit = {
    @annotation.unused
    val x = Seq(Set(1,2,3),Set(4,5,6),Set(7,8,9)).transpose

    ()
  }
}
