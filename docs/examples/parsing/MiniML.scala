package examples.parsing

import scala.util.parsing.combinator1.syntactical.StandardTokenParsers
import scala.util.parsing.combinator1.syntactical.StandardTokenParsers

object MiniML extends StandardTokenParsers {   
  lexical.delimiters += ("(", ")", ".", "=")
  lexical.reserved += ("lambda", "let", "in")

  def expr: Parser[Any] = (
    "let" ~ ident ~ "=" ~ expr ~ "in" ~ expr
  | "lambda" ~ ident ~ "." ~ expr
  | simpleExpr ~ rep(expr)
  )
  def simpleExpr: Parser[Any] = (
    ident
  | "(" ~ expr ~ ")"
  )

  def main(args: Array[String]) {
    val tokens = new lexical.Scanner(args(0))
    println(args(0))
    println(phrase(expr)(tokens))
  }
}

class Expr
case class Let(x: String, expr: Expr, body: Expr) extends Expr
case class Lambda(x: String, expr: Expr) extends Expr
case class Apply(fun: Expr, arg: Expr) extends Expr
case class Var(x: String) extends Expr

object MiniML1 extends StandardTokenParsers {   
  lexical.delimiters += ("(", ")", ".", "=")
  lexical.reserved += ("lambda", "let", "in")

  def expr: Parser[Expr] = (
    "let" ~ ident ~ "=" ~ expr ~ "in" ~ expr ^^ { case "let" ~ x ~ "=" ~ e ~ "in" ~ b => Let(x, e, b) }
  | "lambda" ~ ident ~ "." ~ expr            ^^ { case "lambda" ~ x ~ "." ~ e => Lambda(x, e) }
  | simpleExpr ~ rep(expr)                   ^^ { case f ~ as => (f /: as) (Apply) }
  )
  def simpleExpr: Parser[Expr] = (
    ident                                    ^^ { Var }
  | "(" ~> expr <~ ")"                       
  )

  def main(args: Array[String]) {
    val tokens = new lexical.Scanner(args(0))
    println(args(0))
    println(phrase(expr)(tokens))
  }
}
