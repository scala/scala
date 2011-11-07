import scala.util.parsing.combinator._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

import scala.collection.mutable.HashMap

object Test extends App{
  import grammars._

  val head = phrase(term)

  println(extractResult(head(new lexical.Scanner("1"))))
  println(extractResult(head(new lexical.Scanner("1+2"))))
  println(extractResult(head(new lexical.Scanner("9-4"))))
  println(extractResult(head(new lexical.Scanner("9*9"))))
  println(extractResult(head(new lexical.Scanner("8/2"))))
  println(extractResult(head(new lexical.Scanner("4*9-0/7+9-8*1"))))
  println(extractResult(head(new lexical.Scanner("(1+2)*3"))))
}

object grammars extends StandardTokenParsers with PackratParsers{
  
  def extractResult(r : ParseResult[_]) = r match {
    case Success(a,_) => a
    case NoSuccess(a,_) => a
  }
  
  lexical.delimiters ++= List("+","-","*","/","(",")")
  lexical.reserved ++= List("Hello","World")
  
  /****
   * term = term + fact | term - fact | fact
   * fact = fact * num  | fact / num  | num
   */


 val term: PackratParser[Int] = (term~("+"~>fact) ^^ {case x~y => x+y}
           |term~("-"~>fact) ^^ {case x~y => x-y}
           |fact)
  
 val fact: PackratParser[Int] = (fact~("*"~>numericLit) ^^ {case x~y => x*y.toInt}
           |fact~("/"~>numericLit) ^^ {case x~y => x/y.toInt}
           |"("~>term<~")"
           |numericLit ^^ {_.toInt})
 }
