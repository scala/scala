


import scala.io.Source
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position



object DemoApp extends App {
  val parsers = new DemoParsers
  val reader = new DemoReader(10)
  val result = parsers.startsWith("s").*(reader)
  Console println result
}


class DemoReader(n: Int) extends Reader[String] {
  def atEnd = n == 0
  def first = "s" + n
  def rest = new DemoReader(n - 1)
  def pos = new Position {
    def line = 0
    def column = 0
    def lineContents = first
  }
  println("reader: " + n)
}


class DemoParsers extends Parsers {
  type Elem = String
  def startsWith(prefix: String) = acceptIf(_ startsWith prefix)("Error: " + _)
}
