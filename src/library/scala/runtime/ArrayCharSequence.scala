/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package runtime

// Still need this one since the implicit class ArrayCharSequence only converts
// a single argument.
final class ArrayCharSequence(val xs: Array[Char], start: Int, end: Int) extends CharSequence {
  // yikes
  // java.lang.VerifyError: (class: scala/runtime/ArrayCharSequence, method: <init> signature: ([C)V)
  //   Constructor must call super() or this()
  //
  // def this(xs: Array[Char]) = this(xs, 0, xs.length)

  def length: Int = math.max(0, end - start)
  def charAt(index: Int): Char = {
    if (0 <= index && index < length)
      xs(start + index)
    else throw new ArrayIndexOutOfBoundsException(s"$index is out of bounds (min 0, max ${xs.length - 1})")
  }
  def subSequence(start0: Int, end0: Int): CharSequence = {
    if (start0 < 0) throw new ArrayIndexOutOfBoundsException(s"$start0 is out of bounds (min 0, max ${length -1})")
    else if (end0 > length) throw new ArrayIndexOutOfBoundsException(s"$end0 is out of bounds (min 0, max ${xs.length -1})")
    else if (end0 <= start0) new ArrayCharSequence(xs, 0, 0)
    else {
      val newlen = end0 - start0
      val start1 = start + start0
      new ArrayCharSequence(xs, start1, start1 + newlen)
    }
  }
  override def toString = {
    val start = math.max(this.start, 0)
    val end = math.min(xs.length, start + length)

    if (start >= end) "" else new String(xs, start, end - start)
  }
}
