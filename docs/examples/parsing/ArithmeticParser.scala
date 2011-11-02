/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package examples.parsing

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

/** Parse and evaluate a numeric expression as a sequence of terms, separated by + or -
 *   a term is a sequence of factors, separated by * or /
 *   a factor is a parenthesized expression or a number
 *
 * @author Adriaan Moors 
 */ 
object arithmeticParser extends StdTokenParsers {   
  type Tokens = StdLexical ; val lexical = new StdLexical
  lexical.delimiters ++= List("(", ")", "+", "-", "*", "/")

  lazy val expr =   term*("+" ^^^ {(x: int, y: int) => x + y} | "-" ^^^ {(x: int, y: int) => x - y})
  lazy val term = factor*("*" ^^^ {(x: int, y: int) => x * y} | "/" ^^^ {(x: int, y: int) => x / y})
  lazy val factor: Parser[int] = "(" ~> expr <~ ")" | numericLit ^^ (_.toInt)
  
  def main(args: Array[String]) {
    println(
      if (args.length == 1) {
        expr(new lexical.Scanner(args(0)))
      }
      else
        "usage: scala examples.parsing.arithmeticParser <expr-string>"
    )
  }
}


object arithmeticParserDesugared extends StdTokenParsers {   
  type Tokens = StdLexical ; val lexical = new StdLexical
  lexical.delimiters ++= List("(", ")", "+", "-", "*", "/")

  lazy val expr = chainl1(term, (keyword("+").^^^{(x: int, y: int) => x + y}).|(keyword("-").^^^{(x: int, y: int) => x - y}))
  lazy val term = chainl1(factor, (keyword("*").^^^{(x: int, y: int) => x * y}).|(keyword("/").^^^{(x: int, y: int) => x / y}))
  lazy val factor: Parser[int] = keyword("(").~>(expr.<~(keyword(")"))).|(numericLit.^^(x => x.toInt))   
  
  def main(args: Array[String]) {
    println(
      if (args.length == 1) {
        expr(new lexical.Scanner(args(0)))
      }
      else
        "usage: scala examples.parsing.arithmeticParser <expr-string>"
    )
  }
}
