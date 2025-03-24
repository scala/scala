/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter.shell

/** One instance of a command buffer.
 */
class Parsed private (
  val buffer: String,
  val cursor: Int,
  val delimited: Char => Boolean) {
  /** Break String into args based on delimiting function.
    */
  protected def toArgs(s: String): List[String] =
    if (s == "") Nil
    else (s indexWhere isDelimiterChar) match {
      case -1   => List(s)
      case idx  => (s take idx) :: toArgs(s drop (idx + 1))
    }

  def isDelimiterChar(ch: Char) = delimited(ch)
  def isEscapeChar(ch: Char): Boolean = ch == '\\'

  def isEmpty       = args.isEmpty
  def isUnqualified = args.size == 1
  def isAtStart     = cursor <= 0

  private var _verbosity = 0

  def verbosity = _verbosity
  def withVerbosity(v: Int): this.type = { _verbosity = v ; this }

  def args = toArgs(buffer take cursor).toList
  def bufferHead = args.head
  def headLength = bufferHead.length + 1
  def bufferTail = new Parsed(buffer drop headLength, cursor - headLength, delimited) withVerbosity verbosity

  def prev = new Parsed(buffer, cursor - 1, delimited) withVerbosity verbosity
  def currentChar = buffer(cursor)
  def currentArg = args.last
  def position =
    if (isEmpty) 0
    else if (isLastDelimiter) cursor
    else cursor - currentArg.length

  def isFirstDelimiter  = !isEmpty && isDelimiterChar(buffer.head)
  def isLastDelimiter   = !isEmpty && isDelimiterChar(buffer.last)

  def isQuoted = false // TODO
  def isEscaped = !isAtStart && isEscapeChar(currentChar) && !isEscapeChar(prev.currentChar)
  def isDelimiter = !isQuoted && !isEscaped && isDelimiterChar(currentChar)

  override def toString = "Parsed(%s / %d)".format(buffer, cursor)
}

object Parsed {
  val DefaultDelimiters = "[]{},`; \t".toSet

  private def onull(s: String) = if (s == null) "" else s

  def apply(s: String, cursor: Int): Parsed = apply(onull(s), cursor, DefaultDelimiters)
  def apply(s: String, cursor: Int, delimited: Char => Boolean): Parsed =
    new Parsed(onull(s), cursor, delimited)

  def dotted(s: String, cursor: Int): Parsed = new Parsed(onull(s), cursor, _ == '.')

  // a leading dot plus something, but not ".." or "./", ignoring leading whitespace
  private val dotlike = """\s*\.[^./].*""".r
  def looksLikeInvocation(code: String) = code match {
    case null      => false   // insurance
    case dotlike() => true
    case _         => false
  }
}
