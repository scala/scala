package org.scalacheck.util

import java.util
import java.util.ArrayList

import scala.collection.mutable

private[scalacheck] class ArrayListBuilder[T]
  extends mutable.Builder[T, util.ArrayList[T]] {
  val al = new ArrayList[T]
  def addOne(x: T) = {
    al.add(x)
    this
  }
  def clear() = al.clear()
  def result() = al
}
