object Test extends scala.util.parsing.combinator.RegexParsers {
  def word: Parser[String] = "\\w+".r

  def twoWords = for {
    (a ~ b) <- word ~ word
  } yield (b, a)

  def main(args: Array[String]) {
    println(parseAll(twoWords, "first second"))
  }
}

