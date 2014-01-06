object Test extends App {

  "foo" match {
    case Matcher(result) => println(result)
  }

  object Matcher{
    def unapply(s: String)(implicit secondParam: Option[String] = None) = Some("Got: " + s + " and " + secondParam)
  }
}
