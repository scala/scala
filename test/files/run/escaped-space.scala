object Test {
  def main(args: Array[String]): Unit = {
    println("a\sb")
    println(raw"a\sb")
    println(f"a\sb")
    print(
      """heh  \s
        |bi   \s
        |""".stripMargin)
    print(
      s"""heh  \s
         |bi   \s
         |""".stripMargin)
  }
}
