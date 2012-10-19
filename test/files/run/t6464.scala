import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.RegexParsers

object Test extends RegexParsers {
  val ok: Parser[Any] =
    ( "<%" ~! rep(' ') ~  "\\w+".r  ~ rep(' ') ~ "%>"
    | "<%" ~! error("should not fail here, because of ~!") )

  val buggy: Parser[Any] =
    ( "<%" ~! rep(' ') ~> "\\w+".r <~ rep(' ') ~ "%>"
    | "<%" ~! error("should not fail here, because of ~!") )

  def main(args:Array[String]) {
    println( phrase(ok)   (new CharSequenceReader("<% hi %>")))  // [1.9] parsed: ((((<%~List( ))~hi)~List( ))~%>)
    println( phrase(ok)   (new CharSequenceReader("<%    %>")))  // [1.7] error: string matching regex `\w+' expected but `%' found
    println( phrase(buggy)(new CharSequenceReader("<% hi %>")))  // [1.9] parsed: hi
    println( phrase(buggy)(new CharSequenceReader("<%    %>")))  // java.lang.RuntimeException: should not fail here, because of ~!
  }
}