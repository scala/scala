object Test {
  val x = Seq(1, 2, 3).contains("wat")
  val l = List("foo").contains(Option("foo"))
  val y = Seq(Option(1)).contains(None) // this is ok
}
