object MyParser {
  implicit def literal(s: String): Parser[String] = ???
  trait Parser[+T]
  def parse[T](p: Parser[T], in: java.lang.CharSequence): Option[T] = ???
}
object Test {
  class ParsedAs(a: String) (implicit pattern: MyParser.Parser[_]) {
    def parsedAs[T](v: T) = MyParser.parse(pattern, a).get someOperation v
  }
}
