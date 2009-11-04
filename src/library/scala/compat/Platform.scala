/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat

import java.lang.System

object Platform {

  type StackOverflowError = java.lang.StackOverflowError
  type ConcurrentModificationException = java.util.ConcurrentModificationException

  /**
   *  @param src     ..
   *  @param srcPos  ..
   *  @param dest    ..
   *  @param destPos ..
   *  @param length  ..
   */
  @inline
  def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int) {
    System.arraycopy(src, srcPos, dest, destPos, length)
  }

  /** Create array of the same type as arrayInstance with the given
   *  length.
   *
   *  @param elemClass ..
   *  @param length    ..
   *  @return          ..
   */
  @inline
  def createArray(elemClass: Class[_], length: Int): AnyRef =
    java.lang.reflect.Array.newInstance(elemClass, length)

  @inline
  def arrayclear(arr: Array[Int]) { java.util.Arrays.fill(arr, 0) }

  @inline
  def getClassForName(name: String): Class[_] = java.lang.Class.forName(name)

  val EOL = System.getProperty("line.separator", "\n")

  @inline
  def currentTime: Long = System.currentTimeMillis()

  @inline
  def collectGarbage: Unit = System.gc()

  /** The name of the default character set encoding as a string */
  @inline
  def defaultCharsetName: String = java.nio.charset.Charset.defaultCharset.name
}
