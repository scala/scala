import scala.util.parsing.combinator._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.syntax._

import scala.collection.mutable.HashMap

object Test {
  def main(args: Array[String]): Unit = {
    import grammars3._

    val head = phrase(AnBnCn)

    println(extractResult(head(new lexical.Scanner("a b c"))))
    println(extractResult(head(new lexical.Scanner("a a b b c c"))))
    println(extractResult(head(new lexical.Scanner("a a a b b b c c c"))))
    println(extractResult(head(new lexical.Scanner("a a a a b b b b c c c c"))))

    println(extractResult(AnBnCn(new PackratReader(new lexical.Scanner("a a a b b b b c c c c")))))
    println(extractResult(AnBnCn(new PackratReader(new lexical.Scanner("a a a a b b b c c c c")))))
    println(extractResult(AnBnCn(new PackratReader(new lexical.Scanner("a a a a b b b b c c c")))))
  }
}

object grammars3 extends StandardTokenParsers with PackratParsers {
  
  def extractResult(r: ParseResult[_]) = r match {
    case Success(a,_) => a
    case NoSuccess(a,_) => a
  }
  

  lexical.reserved ++= List("a","b", "c")
  val a: PackratParser[Any] = memo("a")
  val b: PackratParser[Any] = memo("b")
  val c: PackratParser[Any] = memo("c")

  val AnBnCn: PackratParser[Any] = 
    guard(repMany1(a,b) ~ not(b)) ~ rep1(a) ~ repMany1(b,c)// ^^{case x~y => x:::y}


  private def repMany[T](p: => Parser[T], q: => Parser[T]): Parser[List[T]] = 
  ( p~repMany(p,q)~q ^^ {case x~xs~y => x::xs:::(y::Nil)}
   | success(Nil)
  )

  def repMany1[T](p: => Parser[T], q: => Parser[T]): Parser[List[T]] = 
   p~opt(repMany(p,q))~q ^^ {case x~Some(xs)~y => x::xs:::(y::Nil)}

} 
