/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat


import java.lang.System
import Predef.Class

object Platform {

  type StackOverflowError = java.lang.StackOverflowError

  /**
   *  @param src     ..
   *  @param srcPos  ..
   *  @param dest    ..
   *  @param destPos ..
   *  @param length  ..
   */
  def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit =
    System.arraycopy(src, srcPos, dest, destPos, length)

  /** Create array of the same type as arrayInstance with the given
   *  length.
   *
   *  @param elemClass ..
   *  @param length    ..
   *  @return          ..
   */
  def createArray(elemClass: Class, length: Int): AnyRef =
    java.lang.reflect.Array.newInstance(elemClass, length)

  def getClassForName(name: String): Class = java.lang.Class.forName(name)

  val EOL = System.getProperty("line.separator", "\n")

  def currentTime: Long = System.currentTimeMillis()

  def collectGarbage: Unit = System.gc()

}

