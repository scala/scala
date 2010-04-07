trait ResponseCommon extends Parser {
  private[this] var paramsParser: Parser = null
  def withParamsParser(parser: Parser) = {paramsParser = parser; this}

  class Foo {
    println(paramsParser)
  }

  override abstract def parse(t: Any): Unit = t match {
    case ("params", value: List[_]) => value.foreach {paramsParser.parse(_)}
    case _ => super.parse(t)
  }
}

