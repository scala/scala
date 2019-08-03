// scalac: -Xsource:2.14

object Literals214 {
  def inTripleQuoted = """\u000A"""
  def inRawInterpolation = raw"\u000A"
  def inRawTripleQuoted = raw"""\u000A"""
}

object Test {

  def main(args: Array[String]): Unit = {
    val asList = List('\\', 'u', '0', '0', '0', 'A')
    assert(asList == Literals214.inTripleQuoted.toList)
    assert(asList == Literals214.inRawInterpolation.toList)
    assert(asList == Literals214.inRawTripleQuoted.toList)
  }
}