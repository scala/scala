/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package compat

import java.lang.System

object Platform {

  /** Thrown when a stack overflow occurs because a method or function recurses too deeply.
    *
    * On the JVM, this is a type alias for `java.lang.StackOverflowError`, which itself extends `java.lang.Error`.
    * The same rules apply to catching a `java.lang.Error` as for Java, that it indicates a serious problem that a reasonable application should not try and catch.
    */
  type StackOverflowError = java.lang.StackOverflowError

  /** This is a type alias for `java.util.ConcurrentModificationException`,
    * which may be thrown by methods that detect an invalid modification of an object.
    * For example, many common collection types do not allow modifying a collection
    * while it is being iterated over.
    */
  type ConcurrentModificationException = java.util.ConcurrentModificationException

  /** Copies `length` elements of array `src` starting at position `srcPos` to the
    * array `dest` starting at position `destPos`. If `src`==`dest`, the copying will
    * behave as if the elements copied from `src` were first copied to a temporary
    * array before being copied back into the array at the destination positions.
    *
    * @param src     A non-null array as source for the copy.
    * @param srcPos  The starting index in the source array.
    * @param dest    A non-null array as destination for the copy.
    * @param destPos The starting index in the destination array.
    * @param length  The number of elements to be copied.
    * @throws java.lang.NullPointerException If either `src` or `dest` are `null`.
    * @throws java.lang.ArrayStoreException If either `src` or `dest` are not of type
    *                [java.lang.Array]; or if the element type of `src` is not
    *                compatible with that of `dest`.
    * @throws java.lang.IndexOutOfBoundsException If either `srcPos` or `destPos` are
    *                outside of the bounds of their respective arrays; or if `length`
    *                is negative; or if there are less than `length` elements available
    *                after `srcPos` or `destPos` in `src` and `dest` respectively.
    */
  @inline
  def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int) {
    System.arraycopy(src, srcPos, dest, destPos, length)
  }

  /** Creates a new array of the specified type and given length.
   *
   *  Note that if `elemClass` is a subclass of [[scala.AnyVal]] then the returned value is an Array of the corresponding java primitive type.
   *  For example, the following code `scala.compat.Platform.createArray(classOf[Int], 4)` returns an array of the java primitive type `int`.
   *
   *  For a [[scala.AnyVal]] array, the values of the array are set to 0 for ''numeric value types'' ([[scala.Double]], [[scala.Float]], [[scala.Long]], [[scala.Int]], [[scala.Char]],
   *  [[scala.Short]], and [[scala.Byte]]), and `false` for [[scala.Boolean]]. Creation of an array of type [[scala.Unit]] is not possible.
   *
   *  For subclasses of [[scala.AnyRef]], the values of the array are set to `null`.
   *
   *  The caller must cast the returned value to the correct type.
   *
   *  @example {{{
   *  val a = scala.compat.Platform.createArray(classOf[Int], 4).asInstanceOf[Array[Int]] // returns Array[Int](0, 0, 0, 0)
   *  }}}
   *
   *  @param elemClass the `Class` object of the component type of the array
   *  @param length    the length of the new array.
   *  @return          an array of the given component type as an `AnyRef`.
   *  @throws java.lang.NullPointerException If `elemClass` is `null`.
   *  @throws java.lang.IllegalArgumentException if componentType is [[scala.Unit]] or `java.lang.Void.TYPE`
   *  @throws java.lang.NegativeArraySizeException if the specified length is negative
   */
  @inline
  def createArray(elemClass: Class[_], length: Int): AnyRef =
    java.lang.reflect.Array.newInstance(elemClass, length)

  /** Assigns the value of 0 to each element in the array.
    * @param arr     A non-null Array[Int].
    * @throws java.lang.NullPointerException If `arr` is `null`.
    */
  @inline
  def arrayclear(arr: Array[Int]) { java.util.Arrays.fill(arr, 0) }

  /** Returns the `Class` object associated with the class or interface with the given string name using the current `ClassLoader`.
   *  On the JVM, invoking this method is equivalent to: `java.lang.Class.forName(name)`
   *
   *  For more information, please see the Java documentation for [[java.lang.Class]].
   *
   * @param    name the fully qualified name of the desired class.
   * @return   the `Class` object for the class with the specified name.
   * @throws java.lang.LinkageError if the linkage fails
   * @throws java.lang.ExceptionInInitializerError if the initialization provoked by this method fails
   * @throws java.lang.ClassNotFoundException if the class cannot be located
   *  @example {{{
   *  val a = scala.compat.Platform.getClassForName("java.lang.Integer")  // returns the Class[_] for java.lang.Integer
   *  }}}
   */
  @inline
  def getClassForName(name: String): Class[_] = java.lang.Class.forName(name)

  /** The default line separator.
   *
   * On the JVM, this is equivalent to calling the method:
   * `System.getProperty("line.separator")`
   * with a default value of "\n".
   */
  val EOL = scala.util.Properties.lineSeparator

  /** The current time in milliseconds. The time is counted since 1 January 1970
    * UTC.
    *
    * Note that the operating system timer used to obtain this value may be less
    * precise than a millisecond.
    */
  @inline
  def currentTime: Long = System.currentTimeMillis()

  /** Runs the garbage collector.
   *
   * This is a request that the underlying JVM runs the garbage collector.
   * The results of this call depends heavily on the JVM used.
   * The underlying JVM is free to ignore this request.
   */
  @inline
  def collectGarbage(): Unit = System.gc()

  /** The name of the default character set encoding as a string */
  @inline
  def defaultCharsetName: String = java.nio.charset.Charset.defaultCharset.name
}
