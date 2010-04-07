/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools
package partest
package io

import java.io.{ Writer, PrintWriter, OutputStream, OutputStreamWriter }

object ANSIWriter {
  val NONE = 0
  val SOME = 1
  val MANY = 2

  def apply(isAnsi: Boolean) = if (isAnsi) MANY else NONE
}
import ANSIWriter._

class ANSIWriter(writer: Writer) extends PrintWriter(writer, true) {
  def this(out: OutputStream) = this(new OutputStreamWriter(out))
  def colorful: Int = NONE

  protected val manyColors = List(
    Console.BOLD + Console.BLACK,
    Console.BOLD + Console.GREEN,
    Console.BOLD + Console.RED,
    Console.BOLD + Console.YELLOW,
    Console.RESET
  )
  protected val someColors = List(
    Console.BOLD + Console.BLACK,
    Console.RESET,
    Console.BOLD + Console.BLACK,
    Console.BOLD + Console.BLACK,
    Console.RESET
  )
  protected val noColors = List("", "", "", "", "")

  lazy val List(_outline, _success, _failure, _warning, _default) = colorful match {
    case NONE => noColors
    case SOME => someColors
    case MANY => manyColors
    case _    => noColors
  }

  private def wrprint(msg: String): Unit = synchronized {
    print(msg)
    flush()
  }

  def outline(msg: String) = wrprint(_outline + msg + _default)
  def success(msg: String) = wrprint(_success + msg + _default)
  def failure(msg: String) = wrprint(_failure + msg + _default)
  def warning(msg: String) = wrprint(_warning + msg + _default)
  def normal(msg: String) = wrprint(_default + msg)
}
