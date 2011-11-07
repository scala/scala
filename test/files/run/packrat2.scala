import scala.util.parsing.combinator._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

import scala.collection.mutable.HashMap

object Test extends App{
  import grammars2._

  val head = phrase(exp)

  println(extractResult(head(new lexical.Scanner("1"))))
  println(extractResult(head(new lexical.Scanner("1+2"))))
  println(extractResult(head(new lexical.Scanner("9*9"))))
  println(extractResult(head(new lexical.Scanner("4*9+7"))))
  println(extractResult(head(new lexical.Scanner("4*9+7*2+3*3"))))
  println(extractResult(head(new lexical.Scanner("4*9+7*2+3*3+9*5+7*6*2"))))
  println(extractResult(head(new lexical.Scanner("4*(9+7)*(2+3)*3"))))

}

object grammars2 extends StandardTokenParsers with PackratParsers{
  
  def extractResult(r : ParseResult[_]) = r match{
    case Success(a,_) => a
    case NoSuccess(a,_) => a
  }
  
  lexical.delimiters ++= List("+","-","*","/","(",")")
  lexical.reserved ++= List("Hello","World")
  
  /*
   * exp = sum | prod | num
   * sum = exp ~ "+" ~ num
   * prod = exp ~ "*" ~ num
   */

  val exp : PackratParser[Int] = sum | prod | numericLit ^^{_.toInt} | "("~>exp<~")"
  val sum : PackratParser[Int] = exp~("+"~>exp) ^^ {case x~y => x+y}
  val prod: PackratParser[Int] = exp~("*"~>(numericLit ^^{_.toInt} | exp)) ^^ {case x~y => x*y}
  
   
 /* lexical.reserved ++= List("a","b", "c")
  val a : PackratParser[Any] = numericLit^^{x => primeFactors(x.toInt)}
  val b : PackratParser[Any] = memo("b")
  val c : PackratParser[Any] = memo("c")
  val AnBnCn : PackratParser[Any] = 
    parseButDontEat(repMany1(a,b))~not(b)~>rep1(a)~repMany1(b,c)// ^^{case x~y => x:::y}
  //val c : PackratParser[Any] = parseButDontEat(a)~a~a
  //println(c((new PackratReader(new lexical.Scanner("45 24")))))
  val r = new PackratReader(new lexical.Scanner("45 b c"))
  println(AnBnCn(r))
  println(r.getCache.size)
*/ 
}
