/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime.compat;

object Platform {
  def getClass(obj: AnyRef) = obj.getClass();
  def getClassName(obj: AnyRef) = obj.getClass().getName();
  def printStackTrace(exc: java.lang.Throwable) = exc.printStackTrace();
  def getMessage(exc: java.lang.Throwable) = exc.getMessage();
  def split(str: String, separator: Char): Array[String] = {
    str.split(separator.toString());
  }
  def parseFloat(s: String): Float = java.lang.Float.parseFloat(s);
  def parseDouble(s: String): Double = java.lang.Double.parseDouble(s);
}
