/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat


import java.lang.{String, StringBuffer}


/** Consult the documentation of <code>java.lang.StringBuffer</code> for more
 *  details (see http://java.sun.com/javame/reference/apis/jsr030/).
 */
final class StringBuilder(val self: StringBuffer) extends (Int => Char) with Proxy {

  def this() =          this(new StringBuffer())
  def this(n: Int) =    this(new StringBuffer(n))
  def this(s: String) = this(new StringBuffer(s))

  def length: Int = self.length()
  def length_=(n: Int) { self.setLength(n) }
  def setLength(n: Int) { self.setLength(n) }

  def capacity: Int = self.capacity
  def capacity_=(n: Int) { self.ensureCapacity(n) }
  def ensureCapacity(n: Int) { self.ensureCapacity(n) }

  def charAt(i: Int): Char = self.charAt(i)
  def apply(i: Int): Char = self.charAt(i)
  def deleteCharAt(index: Int) = self.deleteCharAt(index)

  def setCharAt(index: Int, c: Char) { self.setCharAt(index, c) }
  def update(i: Int, c: Char) { self.setCharAt(i, c)}

  def substring(i: Int): String = self.toString.substring(i)
  def substring(i: Int, j: Int): String = self.toString.substring(i, j)

  def append(x: Any): StringBuilder = { self.append(x); this }
  def append(x: Boolean): StringBuilder = { self.append(x); this }
  def append(x: Byte):    StringBuilder = { self.append(x); this }
  def append(x: Short):   StringBuilder = { self.append(x); this }
  def append(x: Char):    StringBuilder = { self.append(x); this }
  def append(x: Int):     StringBuilder = { self.append(x); this }
  def append(x: Long):    StringBuilder = { self.append(x); this }
  def append(x: String):  StringBuilder = { self.append(x); this }
  def append(x: Array[Char]): StringBuilder = { self.append(x); this }
  def append(x: Array[Char], start: Int, length: Int): StringBuilder =
    { self.append(x, start, length); this }

  def insert(at: Int, x: Any):     StringBuilder = { self.insert(at, x); this }
  def insert(at: Int, x: Boolean): StringBuilder = { self.insert(at, x); this }
  def insert(at: Int, x: Byte):    StringBuilder = { self.insert(at, x); this }
  def insert(at: Int, x: Short):   StringBuilder = { self.insert(at, x); this }
  def insert(at: Int, x: Char):    StringBuilder = { self.insert(at, x); this }
  def insert(at: Int, x: Int):     StringBuilder = { self.insert(at, x); this }
  def insert(at: Int, x: Long):    StringBuilder = { self.insert(at, x); this }
  def insert(at: Int, x: String):  StringBuilder = { self.insert(at, x); this }
  def insert(at: Int, x: Array[Char]):  StringBuilder = { self.insert(at, x); this }
  def insert(at: Int, x: Array[Char], start: Int, length: Int): StringBuilder =
    { self.insert(at, x.slice(start, length)); this }

}
