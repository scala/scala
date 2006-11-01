/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat;


import java.lang.System
import Predef.Class

object Platform {

  type StackOverflowError = java.lang.StackOverflowError;

  def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit =
    System.arraycopy(src, srcPos, dest, destPos, length)

  /** create array of the same type as arrayInstance with the given length */
  def createArray(elemClass: Class, length: Int): AnyRef =
    java.lang.reflect.Array.newInstance(elemClass, length);

  def getClass(obj: AnyRef) = obj.getClass();
  def getClassName(obj: AnyRef) = obj.getClass().getName();
  def getName(cls: Class) = cls.getName();
  def getElementClass(obj: AnyRef) = obj.getClass().getComponentType();
  def getClassForName(name: String): Class = java.lang.Class.forName(name);

  def printStackTrace(exc: java.lang.Throwable) = exc.printStackTrace();
  def getMessage(exc: java.lang.Throwable) = exc.getMessage();

  val EOL = System.getProperty("line.separator", "\n")

  def getStackTrace(exc: java.lang.Throwable): String = {
    val s = new StringBuilder()
    for (val trElem <- exc.getStackTrace()) {
      s.append(trElem.toString())
      s.append(EOL)
    }
    s.toString()
  }

  def split(str: String, separator: Char): Array[String] = {
    str.split(separator.toString());
  }

  def currentTime: Long = System.currentTimeMillis()
  def collectGarbage: Unit = System.gc()

}
