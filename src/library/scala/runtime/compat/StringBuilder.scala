/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime.compat

class StringBuilder {
  val str = new StringBuffer()

  def this(s: String) = { this(); str.append(s) }

  def charAt(i: Int): Char = str.charAt(i)

  def append(x: Any): StringBuilder = {
    str.append(x)
    this
  }

  def append(x: Char): StringBuilder = {
    str.append(x)
    this
  }

  def append(x: String): StringBuilder = {
    str.append(x)
    this
  }

  def length(): Int = str.length()

  def setLength(i: Int) = str.setLength(i)

  override def toString() = str.toString()
}
