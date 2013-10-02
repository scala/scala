object Test {
  val sw: Seq[Char] = "ab"
  val sw2: Seq[Char] = Array('a', 'b')
  val sw3 = Seq('a', 'b')
  val sw4 = "ab".toList
  val all = List(sw, sw2, sw3, sw4)

  def main(args: Array[String]): Unit = {
    for (s1 <- all ; s2 <- all) {
      assert(s1 == s2, s1 + " != " + s2)
      assert(s1.## == s2.##, s1 + ".## != " + s2 + ".##")
    }
  }
}
