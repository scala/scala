/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools

package object cmd {
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }

  private[cmd] def debug(msg: String) = println(msg)

  def runAndExit(body: => Unit): Nothing = {
    body
    sys.exit(0)
  }

  def toOpt(s: String)              = if (s startsWith "--") s else "--" + s
  def fromOpt(s: String)            = s stripPrefix "--"
  def toArgs(line: String)          = Parser tokenize line
  def fromArgs(args: List[String])  = args mkString " "

  def stripQuotes(s: String) = {
    def isQuotedBy(c: Char) = s.length > 0 && s.head == c && s.last == c
    if (List('"', '\'') exists isQuotedBy) s.tail.init else s
  }
}
