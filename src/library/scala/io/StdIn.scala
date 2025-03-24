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

package scala
package io

import java.text.MessageFormat

/** private[scala] because this is not functionality we should be providing
 *  in the standard library, at least not in this idiosyncratic form.
 *  Factored into trait because it is better code structure regardless.
 */
private[scala] trait StdIn {
  import scala.Console._

  /** Read a full line from the default input.  Returns `null` if the end of the
   * input stream has been reached.
   *
   * @return the string read from the terminal or null if the end of stream was reached.
   */
  def readLine(): String = in.readLine()

  /** Print and flush formatted text to the default output, and read a full line from the default input.
   *  Returns `null` if the end of the input stream has been reached.
   *
   *  @param text the format of the text to print out, as in `printf`.
   *  @param args the parameters used to instantiate the format, as in `printf`.
   *  @return the string read from the default input
   */
  def readLine(text: String, args: Any*): String = {
    printf(text, args: _*)
    out.flush()
    readLine()
  }

  /** Reads a boolean value from an entire line of the default input.
   *  Has a fairly liberal interpretation of the input.
   *
   *  @return the boolean value read, or false if it couldn't be converted to a boolean
   *  @throws java.io.EOFException if the end of the input stream has been reached.
   */
  def readBoolean(): Boolean = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toLowerCase() match {
        case "true" => true
        case "t" => true
        case "yes" => true
        case "y" => true
        case _ => false
      }
  }

  /** Reads a byte value from an entire line of the default input.
   *
   *  @return the Byte that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Byte
   */
  def readByte(): Byte = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toByte
  }

  /** Reads a short value from an entire line of the default input.
   *
   *  @return the short that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Short
   */
  def readShort(): Short = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toShort
  }

  /** Reads a char value from an entire line of the default input.
   *
   *  @return the Char that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.StringIndexOutOfBoundsException if the line read from default input was empty
   */
  def readChar(): Char = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s charAt 0
  }

  /** Reads an int value from an entire line of the default input.
   *
   *  @return the Int that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to an Int
   */
  def readInt(): Int = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toInt
  }

  /** Reads an long value from an entire line of the default input.
   *
   *  @return the Long that was read
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Long
   */
  def readLong(): Long = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toLong
  }

  /** Reads a float value from an entire line of the default input.
   *  @return the Float that was read.
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Float
   *
   */
  def readFloat(): Float = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toFloat
  }

  /** Reads a double value from an entire line of the default input.
   *
   *  @return the Double that was read.
   *  @throws java.io.EOFException if the end of the
   *  input stream has been reached.
   *  @throws java.lang.NumberFormatException if the value couldn't be converted to a Float
   */
  def readDouble(): Double = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      s.toDouble
  }

  /** Reads in some structured input (from the default input), specified by
   *  a format specifier. See class `java.text.MessageFormat` for details of
   *  the format specification.
   *
   *  @param format the format of the input.
   *  @return a list of all extracted values.
   *  @throws java.io.EOFException if the end of the input stream has been
   *          reached.
   */
  def readf(format: String): List[Any] = {
    val s = readLine()
    if (s == null)
      throw new java.io.EOFException("Console has reached end of input")
    else
      textComponents(new MessageFormat(format).parse(s))
  }

  /** Reads in some structured input (from the default input), specified by
   *  a format specifier, returning only the first value extracted, according
   *  to the format specification.
   *
   *  @param format format string, as accepted by `readf`.
   *  @return The first value that was extracted from the input
   */
  def readf1(format: String): Any = readf(format).head

  /** Reads in some structured input (from the default input), specified
   *  by a format specifier, returning only the first two values extracted,
   *  according to the format specification.
   *
   *  @param format format string, as accepted by `readf`.
   *  @return A [[scala.Tuple2]] containing the first two values extracted
   */
  def readf2(format: String): (Any, Any) = {
    val res = readf(format)
    (res.head, res.tail.head)
  }

  /** Reads in some structured input (from the default input), specified
   *  by a format specifier, returning only the first three values extracted,
   *  according to the format specification.
   *
   *  @param format format string, as accepted by `readf`.
   *  @return A [[scala.Tuple3]] containing the first three values extracted
   */
  def readf3(format: String): (Any, Any, Any) = {
    val res = readf(format)
    (res.head, res.tail.head, res.tail.tail.head)
  }

  private def textComponents(a: Array[AnyRef]): List[Any] = {
    var i: Int = a.length - 1
    var res: List[Any] = Nil
    while (i >= 0) {
      res = (a(i) match {
        case x: java.lang.Boolean   => x.booleanValue()
        case x: java.lang.Byte      => x.byteValue()
        case x: java.lang.Short     => x.shortValue()
        case x: java.lang.Character => x.charValue()
        case x: java.lang.Integer   => x.intValue()
        case x: java.lang.Long      => x.longValue()
        case x: java.lang.Float     => x.floatValue()
        case x: java.lang.Double    => x.doubleValue()
        case x => x
      }) :: res
      i -= 1
    }
    res
  }
}

object StdIn extends StdIn
