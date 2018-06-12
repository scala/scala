/* NEST (New Scala Test)
 * Copyright 2007-2018 LAMP/EPFL
 * @author Paul Phillips
 */
package scala.tools.cmd

import scala.annotation.tailrec

/** A simple enough command line parser.
 */
object CommandLineParser {
  private final val DQ = '"'
  private final val SQ = '\''

  /** Split the line into tokens separated by whitespace or quotes.
   *
   *  @return either an error message or reverse list of tokens
   */
  private def tokens(in: String) = {
    import Character.isWhitespace
    import java.lang.{StringBuilder => Builder}
    import collection.mutable.ArrayBuffer

    var accum: List[String] = Nil
    var pos = 0
    var start = 0
    val qpos = new ArrayBuffer[Int](16)    // positions of paired quotes

    def cur: Int  = if (done) -1 else in.charAt(pos)
    def bump() = pos += 1
    def done   = pos >= in.length

    def skipToQuote(q: Int) = {
      var escaped = false
      def terminal = in.charAt(pos) match {
        case _ if escaped => escaped = false ; false
        case '\\'         => escaped = true ; false
        case `q`          => true
        case _            => false
      }
      while (!done && !terminal) pos += 1
      !done
    }
    def skipToDelim(): Boolean =
      cur match {
        case q @ (DQ | SQ)        => { qpos.append(pos); bump(); skipToQuote(q) } && { qpos.append(pos); bump(); skipToDelim() }
        case -1                   => true
        case c if isWhitespace(c) => true
        case _                    => bump(); skipToDelim()
      }
    def skipWhitespace() = while (isWhitespace(cur)) pos += 1
    def copyText() = {
      val buf = new Builder
      var p = start
      var i = 0
      while (p < pos) {
        if (i >= qpos.size) {
          buf.append(in, p, pos)
          p = pos
        } else if (p == qpos(i)) {
          buf.append(in, qpos(i)+1, qpos(i+1))
          p = qpos(i+1)+1
          i += 2
        } else {
          buf.append(in, p, qpos(i))
          p = qpos(i)
        }
      }
      buf.toString
    }
    def text() = {
      val res =
        if (qpos.isEmpty) in.substring(start, pos)
        else if (qpos(0) == start && qpos(1) == pos) in.substring(start+1, pos-1)
        else copyText()
      qpos.clear()
      res
    }
    def badquote = Left("Unmatched quote")

    @tailrec def loop(): Either[String, List[String]] = {
      skipWhitespace()
      start = pos
      if (done) Right(accum)
      else if (!skipToDelim()) badquote
      else {
        accum = text() :: accum
        loop()
      }
    }
    loop()
  }

  class ParseException(msg: String) extends RuntimeException(msg)

  def tokenize(line: String, errorFn: String => Unit): List[String] =
    tokens(line) match {
      case Right(args) => args.reverse
      case Left(msg)   => errorFn(msg) ; Nil
    }

  def tokenize(line: String): List[String] = tokenize(line, x => throw new ParseException(x))
}
