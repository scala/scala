/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._
import scala.reflect.ClassManifest

/** A builder class for arrays */
abstract class ArrayBuilder[T] extends Builder[T, Array[T]]

object ArrayBuilder {

  def make[T: ClassManifest](): ArrayBuilder[T] =
    implicitly[ClassManifest[T]].newArrayBuilder()

  class ofRef[T <: AnyRef : ClassManifest] extends ArrayBuilder[T] {

    private var elems: Array[T] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[T] = {
      val newelems = new Array[T](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: T): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[T]): this.type = (xs: AnyRef) match {
      case xs: WrappedArray.ofRef[_] =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }

  class ofByte extends ArrayBuilder[Byte] {

    private var elems: Array[Byte] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Byte] = {
      val newelems = new Array[Byte](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Byte): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[Byte]): this.type = xs match {
      case xs: WrappedArray.ofByte =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }

  class ofShort extends ArrayBuilder[Short] {

    private var elems: Array[Short] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Short] = {
      val newelems = new Array[Short](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Short): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[Short]): this.type = xs match {
      case xs: WrappedArray.ofShort =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }

  class ofChar extends ArrayBuilder[Char] {

    private var elems: Array[Char] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Char] = {
      val newelems = new Array[Char](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Char): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[Char]): this.type = xs match {
      case xs: WrappedArray.ofChar =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }

  class ofInt extends ArrayBuilder[Int] {

    private var elems: Array[Int] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Int] = {
      val newelems = new Array[Int](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Int): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[Int]): this.type = xs match {
      case xs: WrappedArray.ofInt =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }

  class ofLong extends ArrayBuilder[Long] {

    private var elems: Array[Long] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Long] = {
      val newelems = new Array[Long](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Long): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[Long]): this.type = xs match {
      case xs: WrappedArray.ofLong =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }

  class ofFloat extends ArrayBuilder[Float] {

    private var elems: Array[Float] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Float] = {
      val newelems = new Array[Float](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Float): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[Float]): this.type = xs match {
      case xs: WrappedArray.ofFloat =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }

  class ofDouble extends ArrayBuilder[Double] {

    private var elems: Array[Double] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Double] = {
      val newelems = new Array[Double](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Double): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[Double]): this.type = xs match {
      case xs: WrappedArray.ofDouble =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }

  class ofBoolean extends ArrayBuilder[Boolean] {

    private var elems: Array[Boolean] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Boolean] = {
      val newelems = new Array[Boolean](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Boolean): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[Boolean]): this.type = xs match {
      case xs: WrappedArray.ofBoolean =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }

  class ofUnit extends ArrayBuilder[Unit] {

    private var elems: Array[Unit] = _
    private var capacity: Int = 0
    private var size: Int = 0

    private def mkArray(size: Int): Array[Unit] = {
      val newelems = new Array[Unit](size)
      if (this.size > 0) Array.copy(elems, 0, newelems, 0, this.size)
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
      if (capacity == 0) resize(16)
      if (capacity < size) {
        var newsize = capacity * 2
        while (newsize < size) newsize *= 2
        resize(newsize)
      }
    }

    def +=(elem: Unit): this.type = {
      ensureSize(size + 1)
      elems(size) = elem
      size += 1
      this
    }

    override def ++=(xs: scala.collection.Traversable[Unit]): this.type = xs match {
      case xs: WrappedArray.ofUnit =>
        ensureSize(this.size + xs.length)
        Array.copy(xs.array, 0, elems, this.size, xs.length)
        size += xs.length
        this
      case _ =>
        super.++=(xs)
    }

    def clear() {
      size = 0
    }

    def result() = {
      if (capacity != 0 && capacity == size) elems
      else mkArray(size)
    }
  }
}
