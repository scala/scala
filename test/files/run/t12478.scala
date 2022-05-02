object Test {
  val oks = "ab\u202edc\u202c"
  val okc = '\u202e'

  def שרה = "Sarah"

  def main(args: Array[String]): Unit = {
    println(oks)
    println(s"ab${okc}dc\u202c")
    println(שרה)
  }
}
