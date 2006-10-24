/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat;


object Platform {

  type Class = java.lang.Class;

  type StackOverflowError = java.lang.StackOverflowError;
  type ClassCastException = java.lang.ClassCastException;
  type RuntimeException = java.lang.RuntimeException;
  type IndexOutOfBoundsException = java.lang.IndexOutOfBoundsException;
  type UnsupportedOperationException = java.lang.UnsupportedOperationException
  type IllegalArgumentException = java.lang.IllegalArgumentException
  type NoSuchElementException = java.util.NoSuchElementException

  def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit =
    Array.copy(src, srcPos, dest, destPos, length)

  /** create array of the same type as arrayInstance with the given length */
  def createArray(elemClass: Class, length: Int): Object =
    java.lang.reflect.Array.newInstance(elemClass, length);

  def getClass(obj: AnyRef) = obj.getClass();
  def getClassName(obj: AnyRef) = obj.getClass().getName();
  def getName(cls: Class) = cls.getName();
  def getElementClass(obj: AnyRef) = obj.getClass().getComponentType();

  def printStackTrace(exc: java.lang.Throwable) = exc.printStackTrace();
  def getMessage(exc: java.lang.Throwable) = exc.getMessage();

  private val eol = System.getProperty("line.separator", "\n")
  def getStackTrace(exc: java.lang.Throwable): String = {
    val s = new StringBuilder()
    for (val trElem <- exc.getStackTrace()) {
      s.append(trElem.toString())
      s.append(eol)
    }
    s.toString()
  }

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
