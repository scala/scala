/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003-2006, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
*/

// $Id: CodeWriter.scala 5837 2006-02-23 17:37:25 +0000 (Thu, 23 Feb 2006) michelou $

package scala.tools.scalap

import java.io._


class CodeWriter(writer: Writer) {

  private val nl = System.getProperty("line.separator")
  private var step = "  "
  private var level = 0
  private var align = false
  private var space = false
  private var line = false

  def getWriter = writer

  def getIndentLevel = level

  def setIndentLevel(level: Int): CodeWriter = {
    this.level = level
    this
  }

  def getIndentWidth = if (step == null) -1 else step.length()

  def setIndentWidth(width: Int): CodeWriter = {
    val buffer = new StringBuffer(width)
    var i = 0
    while (i < width)
      buffer.append(' ')
    setIndentString(buffer.toString())
  }

  def getIndentString = step;

  def setIndentString(step: String): CodeWriter = {
    this.step = step
    this
  }

  def indent: CodeWriter = {
    level = level + 1
    this
  }

  def undent: CodeWriter = {
    level = level - 1
    this
  }

  def newline: CodeWriter = {
    if (step == null)
      newspace
    else if (!line) {
      try {
        writer.write(nl)
      } catch {
        case e => error("IO error")
      }
      line = align
      align = true
      space = false
      this
    } else
      this
  }

  def newspace: CodeWriter = {
    space = !align
    this
  }

  def * : Unit = {}

  def println: CodeWriter = newline

  def println(value: Boolean): CodeWriter = print(value).newline

  def println(value: Byte): CodeWriter = print(value).newline

  def println(value: Short): CodeWriter = print(value).newline

  def println(value: Char): CodeWriter = print(value).newline

  def println(value: Int): CodeWriter = print(value).newline

  def println(value: Long): CodeWriter = print(value).newline

  def println(value: Float): CodeWriter = print(value).newline

  def println(value: Double): CodeWriter = print(value).newline

  def println(value: String): CodeWriter = print(value).newline

  def print(value: Boolean): CodeWriter = print(String.valueOf(value))

  def print(value: Byte): CodeWriter = print(String.valueOf(value))

  def print(value: Short): CodeWriter = print(String.valueOf(value))

  def print(value: Char): CodeWriter = print(String.valueOf(value))

  def print(value: Int): CodeWriter = print(String.valueOf(value))

  def print(value: Long): CodeWriter = print(String.valueOf(value))

  def print(value: Float): CodeWriter = print(String.valueOf(value))

  def print(value: Double): CodeWriter = print(String.valueOf(value))

  def print(value: String): CodeWriter = try {
    if (align) {
      var i = 0
      while (i < level) {
        writer.write(step)
        i = i + 1
      }
    }
    if (space)
      writer.write(" ")
    writer.write(value)
    align = false
    space = false
    line = false
    this
  } catch {
    case e => error("IO error")
  }

  override def toString(): String = writer.toString()
}
