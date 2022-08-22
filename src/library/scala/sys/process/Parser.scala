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
import collection.mutable.ListBuffer
import Character.isWhitespace

/** A simple enough command line parser using shell quote conventions.
 */
private[scala] object Parser {
  private final val DQ = '"'
  private final val SQ = '\''
  private final val EOF = -1

  /** Split the line into tokens separated by whitespace.
   *
   *  Tokens may be surrounded by quotes and may contain whitespace or escaped quotes.
   *
   *  @return list of tokens
   */
  def tokenize(line: String, errorFn: String => Unit): List[String] = {
    val accum = ListBuffer.empty[String]
    val buf   = new java.lang.StringBuilder
    var pos   = 0

    def cur: Int = if (done) EOF else line.charAt(pos)
    def bump()   = pos += 1
    def put()    = { buf.append(cur.toChar); bump() }
    def done     = pos >= line.length

    def skipWhitespace() = while (isWhitespace(cur)) bump()

    // Collect to end of word, handling quotes. False for missing end quote.
    def word(): Boolean = {
      var escaped = false
      var Q = EOF
      var lastQ = 0
      def inQuote = Q != EOF
      def badquote() = errorFn(s"Unmatched quote [${lastQ}](${line.charAt(lastQ)})")
      def finish(): Boolean = if (!inQuote) !escaped else { badquote(); false}
      @tailrec def advance(): Boolean = cur match {
        case EOF                              => finish()
        case _ if escaped                     => escaped = false; put(); advance()
        case '\\'                             => escaped = true; bump(); advance()
        case q if q == Q                      => Q = EOF; bump(); advance()
        case q @ (DQ | SQ) if !inQuote        => Q = q; lastQ = pos; bump(); advance()
        case c if isWhitespace(c) && !inQuote => finish()
        case _                                => put(); advance()
      }
      advance()
    }
    def text() = {
      val res = buf.toString
      buf.setLength(0)
      res
    }
    @tailrec def loop(): List[String] = {
      skipWhitespace()
      if (done) accum.toList
      else if (!word()) Nil
      else {
        accum += text()
        loop()
      }
    }
    loop()
  }

  class ParseException(msg: String) extends RuntimeException(msg)

  def tokenize(line: String): List[String] = tokenize(line, x => throw new ParseException(x))
}
