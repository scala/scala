import scala.util.matching.Regex

object Test extends App {
  val input = "CURRENCY 5.80"
  println("CURRENCY".r.replaceAllIn(input, Regex quoteReplacement "US$"))
}

