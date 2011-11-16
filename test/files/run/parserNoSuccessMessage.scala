object Test extends scala.util.parsing.combinator.RegexParsers {
  def sign = "-"
  def number = "\\d+".r
  def p = sign.? ~ number withErrorMessage  "Number expected!"
  def q = sign.? ~! number withErrorMessage  "Number expected!"

  def main(args: Array[String]) {
    println(parseAll(p, "-x"))
    println(parseAll(p, "x"))
    println(parseAll(p, "-5"))
    println(parseAll(p, "5"))
    println(parseAll(q, "-x"))
    println(parseAll(q, "x"))
    println(parseAll(q, "-5"))
    println(parseAll(q, "5"))
  }
}


