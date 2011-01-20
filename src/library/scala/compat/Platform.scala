/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.compat

import java.lang.System

object Platform {

  type StackOverflowError = java.lang.StackOverflowError
  type ConcurrentModificationException = java.util.ConcurrentModificationException

  /** Copies `length` elements of array `src` starting at position `srcPos` to the
    * array `dest` starting at position `destPos`. If `src eq dest`, the copying will
    * behave as if the elements copied from `src` were first copied to a temporary
    * array before being copied back into the array at the destination positions.
    * @param src     A non-null array as source for the copy.
    * @param srcPos  The starting index in the source array.
    * @param dest    A non-null array as destination for the copy.
    * @param destPos The starting index in the destination array.
    * @param length  The number of elements to be copied.
    * @throws java.lang.NullPointerException If either `src` or `dest` are `null`.
    * @throws java.lang.ArrayStoreException If either `src` or `dest` are not of type
    *                [java.lang.Array]; or if the element type of `src` is not
    *                compatible with that of `dest`.
    * @throws java.lang.IndexOutOfBoundsException If either srcPos` or `destPos` are
    *                outside of the bounds of their respective arrays; or if `length`
    *                is negative; or if there are less than `length` elements available
    *                after `srcPos` or `destPos` in `src` and `dest` respectively. */
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

  val EOL = util.Properties.lineSeparator

  /** The current time in milliseconds. The time is counted since 1 January 1970
    * UTC.
    *
    * Note that the operating system timer used to obtain this value may be less
    * precise than a millisecond. */
  @inline
  def currentTime: Long = System.currentTimeMillis()

  @inline
  def collectGarbage: Unit = System.gc()

  /** The name of the default character set encoding as a string */
  @inline
  def defaultCharsetName: String = java.nio.charset.Charset.defaultCharset.name
}
