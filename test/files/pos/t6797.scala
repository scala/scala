object Test extends App /* workaround: don't extend App */ {
  private class Matcher(aParam: Option[String] = None)
  private val stringMatcher = new Matcher
}
