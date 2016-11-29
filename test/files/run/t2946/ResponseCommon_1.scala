class Parser {
  def parse(t: Any): Unit = {}
}

trait ResponseCommon extends Parser {
  private[this] var paramsParser: Parser = null
  def withParamsParser(parser: Parser) = {paramsParser = parser; this}

  override abstract def parse(t: Any): Unit = t match {
    case ("params", value: List[_]) => value.foreach {paramsParser.parse(_)}
    case _ => super.parse(t)
  }
}
