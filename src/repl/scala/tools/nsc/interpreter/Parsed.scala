/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import util.returning

trait Delimited {
  self: Parsed =>

  def delimited: Char => Boolean
  def escapeChars: List[Char] = List('\\')

  /** Break String into args based on delimiting function.
   */
  protected def toArgs(s: String): List[String] =
    if (s == "") Nil
    else (s indexWhere isDelimiterChar) match {
      case -1   => List(s)
      case idx  => (s take idx) :: toArgs(s drop (idx + 1))
    }

  def isDelimiterChar(ch: Char) = delimited(ch)
  def isEscapeChar(ch: Char): Boolean = escapeChars contains ch
}

/** One instance of a command buffer.
 */
class Parsed private (
  val buffer: String,
  val cursor: Int,
  val delimited: Char => Boolean
) extends Delimited {
  def isEmpty       = args.isEmpty
  def isUnqualified = args.size == 1
  def isAtStart     = cursor <= 0

  private var _verbosity = 0

  def verbosity = _verbosity
  def withVerbosity(v: Int): this.type = returning[this.type](this)(_ => _verbosity = v)

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
}
