/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.sys.process

import scala.annotation.tailrec

/** A simple enough command line parser using shell quote conventions.
 */
private[scala] object Parser {
  private final val DQ = '"'
  private final val SQ = '\''
  private final val EOF = -1

  /** Split the line into tokens separated by whitespace or quotes.
   *
   *  @return either an error message or reverse list of tokens
   */
  def tokenize(line: String, errorFn: String => Unit): List[String] = {
    import Character.isWhitespace
    import java.lang.{StringBuilder => Builder}
    import collection.mutable.ArrayBuffer

    var accum: List[String] = Nil
    var pos = 0
    var start = 0
    val qpos = new ArrayBuffer[Int](16)    // positions of paired quotes

    def cur: Int  = if (done) EOF else line.charAt(pos)
    def bump() = pos += 1
    def done   = pos >= line.length

    // Skip to the next quote as given.
    def skipToQuote(q: Int): Boolean = {
      var escaped = false
      def terminal: Boolean = cur match {
        case _ if escaped => escaped = false ; false
        case '\\'         => escaped = true ; false
        case `q` | EOF    => true
        case _            => false
      }
      while (!terminal) bump()
      !done
    }
    // Skip to a word boundary, where words can be quoted and quotes can be escaped
    def skipToDelim(): Boolean = {
      var escaped = false
      def quote() = { qpos += pos ; bump() }
      @tailrec def advance(): Boolean = cur match {
        case _ if escaped         => escaped = false ; bump() ; advance()
        case '\\'                 => escaped = true ; bump() ; advance()
        case q @ (DQ | SQ)        => { quote() ; skipToQuote(q) } && { quote() ; advance() }
        case EOF                  => true
        case c if isWhitespace(c) => true
        case _                    => bump(); advance()
      }
      advance()
    }
    def skipWhitespace() = while (isWhitespace(cur)) bump()
    def copyText() = {
      val buf = new Builder
      var p = start
      var i = 0
      while (p < pos) {
        if (i >= qpos.size) {
          buf.append(line, p, pos)
          p = pos
        } else if (p == qpos(i)) {
          buf.append(line, qpos(i)+1, qpos(i+1))
          p = qpos(i+1)+1
          i += 2
        } else {
          buf.append(line, p, qpos(i))
          p = qpos(i)
        }
      }
      buf.toString
    }
    def text() = {
      val res =
        if (qpos.isEmpty) line.substring(start, pos)
        else if (qpos(0) == start && qpos(1) == pos) line.substring(start+1, pos-1)
        else copyText()
      qpos.clear()
      res
    }
    def badquote() = errorFn(s"Unmatched quote [${qpos.last}](${line.charAt(qpos.last)})")

    @tailrec def loop(): List[String] = {
      skipWhitespace()
      start = pos
      if (done) accum.reverse
      else if (!skipToDelim()) { badquote() ; Nil }
      else {
        accum ::= text()
        loop()
      }
    }
    loop()
  }

  class ParseException(msg: String) extends RuntimeException(msg)

  def tokenize(line: String): List[String] = tokenize(line, x => throw new ParseException(x))
}
