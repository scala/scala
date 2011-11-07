import util.parsing.combinator.{PackratParsers, RegexParsers}

object MyParser extends RegexParsers with PackratParsers {
}

object Test {
  class ParsedAs(a: String) (implicit pattern: MyParser.Parser[_]) {
    def parsedAs[T](v: T) = MyParser.parse(pattern, a).get someOperation v 
  }
}
