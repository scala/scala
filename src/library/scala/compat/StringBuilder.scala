/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat

import java.lang.StringBuffer

final class StringBuilder(str: StringBuffer) {

  def this() =          this(new StringBuffer())
  def this(n: Int) =    this(new StringBuffer(n))
  def this(s: String) = this(new StringBuffer(s))

  def charAt(i: Int): Char = str.charAt(i)

  def setCharAt(index: Int, c: Char): Unit = str.setCharAt(index, c)

  def append(x: Any): StringBuilder = { str.append(x); this }
  def append(x: Boolean): StringBuilder = { str.append(x); this }
  def append(x: Byte):    StringBuilder = { str.append(x); this }
  def append(x: Short):   StringBuilder = { str.append(x); this }
  def append(x: Char):    StringBuilder = { str.append(x); this }
  def append(x: Int):     StringBuilder = { str.append(x); this }
  def append(x: Long):    StringBuilder = { str.append(x); this }
  def append(x: Float):   StringBuilder = { str.append(x); this }
  def append(x: Double):  StringBuilder = { str.append(x); this }
  def append(x: String):  StringBuilder = { str.append(x); this }
  def append(x: Array[Char]): StringBuilder = { str.append(x); this }
  def append(x: Array[Char], start: Int, length: Int): StringBuilder =
    { str.append(x, start, length); this }

  def insert(at: Int, x: Any):     StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Boolean): StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Byte):    StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Short):   StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Char):    StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Int):     StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Long):    StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Float):   StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Double):  StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: String):  StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Array[Char]):  StringBuilder = { str.insert(at, x); this }
  def insert(at: Int, x: Array[Char], start: Int, length: Int): StringBuilder =
    { str.insert(at, x, start, length); this }

  def length(): Int = str.length()

  def setLength(i: Int) = str.setLength(i)

  override def toString() = str.toString()
}
