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
  val str = new System.Text.StringBuilder();
  def append(x: Any): StringBuilder = {
    str.Append(x);
    this
  }
  def length(): Int = str.Length;
  override def toString() = str.toString();
}
