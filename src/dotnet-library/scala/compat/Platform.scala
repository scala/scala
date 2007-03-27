/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.compat


import Predef._

object Platform {

  type StackOverflowError = System.StackOverflowException

  /**
   *  @param src     ..
   *  @param srcPos  ..
   *  @param dest    ..
   *  @param destPos ..
   *  @param length  ..
   */
  def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = {
    if (!src.isInstanceOf[System.Array]) throw new Exception("src for arraycopy is not an Array; use scala.Array.copy for boxed arrays");
    if (!dest.isInstanceOf[System.Array]) throw new Exception("dest for arraycopy is not an Array; use scala.Array.copy for boxed arrays");
    System.Array.Copy(src.asInstanceOf[System.Array], srcPos, dest.asInstanceOf[System.Array], destPos, length)
  }

  /** Create array of the same type as arrayInstance with the given
   *  length.
   *
   *  @param elemClass ..
   *  @param length    ..
   *  @return          ..
   */
  def createArray(elemClass: Class, length: Int): AnyRef =
    System.Array.CreateInstance(elemClass, length);

  def getClassForName(name: String): Class = System.Type.GetType(name)

  val EOL = System.Environment.NewLine

  def currentTime: Long = 0L

  def collectGarbage: Unit = System.GC.Collect()

}

