package tastytest

class Lexeme private (private val str: String) extends AnyVal with Ordered[Lexeme] {
  def compare(that: Lexeme) = str.compareTo(that.str)
}

object Lexeme {

  val empty: Lexeme = new Lexeme("")

  def apply(str: String): Option[Lexeme] = str match {
    case str: String => Some(new Lexeme(str))
    case null        => None
  }
}
