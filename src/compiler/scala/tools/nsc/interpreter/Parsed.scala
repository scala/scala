/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

/** One instance of a command buffer.
 */
class Parsed(_buf: String) {
  val buffer = if (_buf == null) "" else _buf
  val segments = (buffer split '.').toList filterNot (_ == "")
  lazy val hd :: tl = segments
  def stub = firstDot + hd + "."
  def remainder = buffer stripPrefix stub
  def unqualifiedPart = segments.last

  def isEmpty = segments.size == 0
  def isUnqualified = segments.size == 1
  def isQualified = segments.size > 1

  def isFirstCharDot = buffer startsWith "."
  def isLastCharDot = buffer endsWith "."
  def firstDot = if (isFirstCharDot) "." else ""
  def lastDot = if (isLastCharDot) "." else ""

  // sneakily, that is 0 when there is no dot, which is what we want
  def position = (buffer lastIndexOf '.') + 1
}

