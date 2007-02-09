/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat


import System.Text.{StringBuilder => StringBuffer}

final class StringBuilder(str: StringBuffer) {

  def this() =          this(new StringBuffer())
  def this(n: Int) =    this(new StringBuffer(n))
  def this(s: String) = this(new StringBuffer(s))

  def charAt(i: Int): Char = str(i)
  def setCharAt(index: Int, c: Char): Unit = str(index) = c

  def append(x: Any): StringBuilder = { str.Append(x); this }
  def append(x: Boolean): StringBuilder = { str.Append(x); this }
  def append(x: Byte):    StringBuilder = { str.Append(x); this }
  def append(x: Short):   StringBuilder = { str.Append(x); this }
  def append(x: Char):    StringBuilder = { str.Append(x); this }
  def append(x: Int):     StringBuilder = { str.Append(x); this }
  def append(x: Long):    StringBuilder = { str.Append(x); this }
  def append(x: Float):   StringBuilder = { str.Append(x); this }
  def append(x: Double):  StringBuilder = { str.Append(x); this }
  def append(x: String):  StringBuilder = { str.Append(x); this }
  def append(x: Array[Char]): StringBuilder = { str.Append(x); this }
  def append(x: Array[Char], start: Int, length: Int): StringBuilder =
    { str.Append(x, start, length); this }

  def insert(at: Int, x: Any):     StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Boolean): StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Byte):    StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Short):   StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Char):    StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Int):     StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Long):    StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Float):   StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Double):  StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: String):  StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Array[Char]):  StringBuilder = { str.Insert(at, x); this }
  def insert(at: Int, x: Array[Char], start: Int, length: Int): StringBuilder =
    { str.Insert(at, x, start, length); this }

  def length(): Int = str.Length

  def setLength(i: Int) = str.Capacity = i

  override def toString() = str.toString()
}
