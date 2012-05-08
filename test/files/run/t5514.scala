


import scala.io.Source
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position



class DemoReader(n: Int) extends Reader[String] {
  def atEnd = n == 0
  def first = if (n >= 0) "s" + n else throw new IllegalArgumentException("No more input.")
  def rest = new DemoReader(n - 1)
  def pos = new Position {
    def line = 0
    def column = 0
    def lineContents = first
  }
  println("constructed reader: " + n)
}


object Test extends App with Parsers {
  type Elem = String
  def startsWith(prefix: String) = acceptIf(_ startsWith prefix)("Error: " + _)
  
  val resrep = startsWith("s").*(new DemoReader(10))
  Console println resrep
  
  val resrep5 = repN(5, startsWith("s"))(new DemoReader(10))
  Console println resrep5
}


