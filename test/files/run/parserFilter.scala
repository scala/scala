object Test extends scala.util.parsing.combinator.RegexParsers {
    val keywords = Set("if", "false")
    def word: Parser[String] = "\\w+".r

    def keyword: Parser[String] = word filter (keywords.contains)
    def ident: Parser[String] = word filter(!keywords.contains(_))

    def test = keyword ~ ident

    def main(args: Array[String]) {
      println(parseAll(test, "if false"))
      println(parseAll(test, "not true"))
      println(parseAll(test, "if true"))
    }
}
