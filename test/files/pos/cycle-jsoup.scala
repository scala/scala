// scalac: -Ybreak-cycles
object Test {
  def main(args : Array[String]): Unit = {
    org.jsoup.Jsoup.parse(null: java.net.URL, 3000)
  }
}
