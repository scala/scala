/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime.compat;

object Platform {
  def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit =
    System.arraycopy(src, srcPos, dest, destPos, length);
  def getClass(obj: AnyRef) = obj.getClass();
  def getClassName(obj: AnyRef) = obj.getClass().getName();
  def printStackTrace(exc: java.lang.Throwable) = exc.printStackTrace();
  def getMessage(exc: java.lang.Throwable) = exc.getMessage();
  def split(str: String, separator: Char): Array[String] = {
    str.split(separator.toString());
  }

  def currentThread = java.lang.Thread.currentThread();

  def parseByte(s: String): Byte = java.lang.Byte.parseByte(s);
  def parseShort(s: String): Short = java.lang.Short.parseShort(s);
  def parseInt(s: String): Int = java.lang.Integer.parseInt(s);
  def parseLong(s: String): Long = java.lang.Long.parseLong(s);
  def parseFloat(s: String): Float = java.lang.Float.parseFloat(s);
  def parseDouble(s: String): Double = java.lang.Double.parseDouble(s);

  def isDigit(c: Char): Boolean = java.lang.Character.isDigit(c);
}
