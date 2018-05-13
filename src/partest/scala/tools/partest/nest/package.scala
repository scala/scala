package scala.tools.partest

package object nest {
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }

  // make some language features in this package compile without warning
  implicit def implicitConversions = scala.language.implicitConversions
  implicit def postfixOps = scala.language.postfixOps

  def runAndExit(body: => Unit): Nothing = {
    body
    System.exit(0)
    ???
  }

  def toOpt(s: String): String             = if (s startsWith "--") s else "--" + s
  def fromOpt(s: String): String           = s stripPrefix "--"

  def stripQuotes(s: String): String = {
    def isQuotedBy(c: Char) = s.length > 0 && s.head == c && s.last == c
    if (List('"', '\'') exists isQuotedBy) s.tail.init else s
  }
}
