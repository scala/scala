/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat


import System.Text.{StringBuilder => StringBuffer}


/** Consult the documentation of <code>java.lang.StringBuffer</code>
 *  for more details.
 */
final class StringBuilder(val self: StringBuffer) extends (Int => Char) with Proxy {

  def this() =          this(new StringBuffer())
  def this(n: Int) =    this(new StringBuffer(n))
  def this(s: String) = this(new StringBuffer(s))

  def length: Int = self.Length
  def length_=(n: Int) { self.Length = n }
  def setLength(n: Int) { self.Length = n }

  def capacity: Int = self.Capacity
  def capacity_=(n: Int) { self.Capacity = n }
  def ensureCapacity(n: Int) { self.Capacity = n }

  def charAt(i: Int): Char = self(i)
  def apply(i: Int): Char = self(i)
  def deleteCharAt(index: Int) = self.Remove(index, 1)

  def setCharAt(i: Int, c: Char) { self(i) = c }
  def update(i: Int, c: Char) { self(i) = c }

  def substring(i: Int): String = self.ToString(i, length)
  def substring(i: Int, j: Int): String = self.ToString(i, j)

  def append(x: Any): StringBuilder = { self.Append(x); this }
  def append(x: Boolean): StringBuilder = { self.Append(x); this }
//   def append(x: Byte):    StringBuilder = { self.Append(x); this }
  def append(x: Short):   StringBuilder = { self.Append(x); this }
  def append(x: Char):    StringBuilder = { self.Append(x); this }
  def append(x: Int):     StringBuilder = { self.Append(x); this }
  def append(x: Long):    StringBuilder = { self.Append(x); this }
  def append(x: Float):   StringBuilder = { self.Append(x); this }
  def append(x: Double):  StringBuilder = { self.Append(x); this }
  def append(x: String):  StringBuilder = { self.Append(x); this }
  def append(x: Array[Char]): StringBuilder = { self.Append(x); this }
  def append(x: Array[Char], start: Int, length: Int): StringBuilder =
    { self.Append(x, start, length); this }

  def insert(at: Int, x: Any):     StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: Boolean): StringBuilder = { self.Insert(at, x); this }
//   def insert(at: Int, x: Byte):    StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: Short):   StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: Char):    StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: Int):     StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: Long):    StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: Float):   StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: Double):  StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: String):  StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: Array[Char]):  StringBuilder = { self.Insert(at, x); this }
  def insert(at: Int, x: Array[Char], start: Int, length: Int): StringBuilder =
    { self.Insert(at, x, start, length); this }

}
