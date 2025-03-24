/*
 * Scala classfile decoder (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools.scalap

import java.io._


class CodeWriter(writer: Writer) {
  import java.lang.System.{lineSeparator => nl}

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

  def setIndentWidth(width: Int): CodeWriter =
    setIndentString(List.fill(width)(' ').mkString)

  def getIndentString = step

  def setIndentString(step: String): CodeWriter = {
    this.step = step
    this
  }

  def indent: CodeWriter = {
    level += 1
    this
  }

  def undent: CodeWriter = {
    level -= 1
    this
  }

  def newline: CodeWriter = {
    if (step == null)
      newspace
    else if (!line) {
      writer.write(nl)
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

  def *() = {}

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

  def print(value: Byte): CodeWriter = print(String.valueOf(value.toInt))

  def print(value: Short): CodeWriter = print(String.valueOf(value.toInt))

  def print(value: Char): CodeWriter = print(String.valueOf(value))

  def print(value: Int): CodeWriter = print(String.valueOf(value))

  def print(value: Long): CodeWriter = print(String.valueOf(value))

  def print(value: Float): CodeWriter = print(String.valueOf(value))

  def print(value: Double): CodeWriter = print(String.valueOf(value))

  def print(value: String): CodeWriter = {
    if (align) {
      var i = 0
      while (i < level) {
        writer.write(step)
        i += 1
      }
    }
    if (space)
      writer.write(" ")
    writer.write(value)
    align = false
    space = false
    line = false
    this
  }

  override def toString(): String = writer.toString()
}
