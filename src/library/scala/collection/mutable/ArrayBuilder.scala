/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ArrayBuffer.scala 18387 2009-07-24 15:28:37Z odersky $


package scala.collection.mutable

import scala.collection.generic._
import scala.reflect.Manifest
import scala.runtime.BoxedArray

/** A builder class for arrays */
class ArrayBuilder[A](manifest: Manifest[A]) extends Builder[A, BoxedArray[A]] {

  private var elems: BoxedArray[A] = _
  private var capacity: Int = 0
  private var size: Int = 0

  private def mkArray(size: Int): BoxedArray[A] = {
    val newelems = manifest.newArray(size)
    if (this.size > 0) Array.copy(elems.value, 0, newelems.value, 0, this.size)
    newelems
  }

  private def resize(size: Int) {
    elems = mkArray(size)
    capacity = size
  }

  override def sizeHint(size: Int) {
    if (capacity < size) resize(size)
  }

  private def ensureSize(size: Int) {
    if (capacity < size) {
      var newsize = if (capacity == 0) 16 else capacity * 2
      while (newsize < size) newsize *= 2
      resize(newsize)
    }
  }

  def +=(elem: A): this.type = {
    ensureSize(size + 1)
    elems(size) = elem
    size += 1
    this
  }

  def clear() {
    size = 0
  }

  def result() = {
    if (capacity != 0 && capacity == size) elems
    else mkArray(size)
  }

  // todo: add ++=
}
