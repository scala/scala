package examples.parsing

import scala.util.parsing.combinator1.syntactical.StandardTokenParsers

object ListParsers extends StandardTokenParsers {   
  lexical.delimiters ++= List("(", ")", ",")

  def expr: Parser[Any] = "(" ~ exprs ~ ")" | ident | numericLit
  def exprs: Parser[Any] = expr ~ rep ("," ~ expr)

  def main(args: Array[String]) {
    val tokens = new lexical.Scanner(args(0))
    println(args(0))
    println(phrase(expr)(tokens))
  }
}

object ListParsers1 extends StandardTokenParsers {   
  lexical.delimiters ++= List("(", ")", ",")

  def expr: Parser[Any] = "(" ~> exprs <~ ")" | ident | numericLit

  def exprs: Parser[List[Any]] = expr ~ rep ("," ~> expr) ^^ { case x ~ y => x :: y }

  def main(args: Array[String]) {
    val tokens = new lexical.Scanner(args(0))
    println(args(0))
    println(phrase(expr)(tokens))
  }
}
