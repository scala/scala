/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime.compat;

class StringBuilder {
  val str = new StringBuffer();
  def append(x: Any): StringBuilder = {
    str.append(x);
    this
  }
  def length(): Int = str.length();
  override def toString() = str.toString();
}
