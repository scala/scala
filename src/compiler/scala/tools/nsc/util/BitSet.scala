/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: Set.scala 12005 2007-06-13 12:28:07Z michelou $

package scala.tools.nsc.util

object BitSet {

  final val WordSize = 64
  private final val WordMask = WordSize - 1

  val empty: BitSet = new Single(0)
  def apply(elems: Int*): BitSet = empty ++ elems

  private def wordIterator(elems: Long, adjust: Int) = new Iterator[Int] {
    private var rest = elems
    private var i = 0
    def hasNext = rest != 0
    def next() = {
      if (rest == 0)
        throw new NoSuchElementException("next on empty iterator")
      while ((rest & 1) == 0) {
        rest >>>= 1
        i += 1
      }
      rest >>>= 1
      i += 1
      i - 1 + adjust
    }
  }

  private class Single(elems: Long) extends BitSet {

    def contains(i: Int): Boolean =
      0 <= i && i < WordSize && (elems & (1L << i)) != 0

    def elements: Iterator[Int] =
      wordIterator(elems, 0)

    def +(i: Int): BitSet = {
      require(0 <= i)
      if (i < WordSize) new Single(elems | (1L << i))
      else if (i < 2 * WordSize) new Double(elems, 1L << i)
      else new Multiple(Array(elems)) + i
    }

    def -(i: Int): BitSet = {
      require(0 <= i)
      if (i < WordSize) new Single(elems & ~(1L << i))
      else this
    }
  }

  private class Double(elems1: Long, elems2: Long) extends BitSet {

    def contains(i: Int): Boolean =
      0 <= i &&
      (i < WordSize && (elems1 & (1L << i)) != 0 ||
       i < 2 * WordSize && (elems2 & (1L << i)) != 0)

    def elements: Iterator[Int] =
      wordIterator(elems1, 0) ++ wordIterator(elems2, WordSize)

    def +(i: Int): BitSet = {
      require(0 <= i)
      if (i < WordSize) new Double(elems1 | (1L << i), elems2)
      else if (i < 2 * WordSize) new Double(elems1, elems2 | (1L << i))
      else new Multiple(Array(elems1, elems2)) + i
    }

    def -(i: Int): BitSet = {
      require(0 <= i)
      if (i < WordSize) new Double(elems1 & ~(1L << i), elems2)
      else if (i < 2 * WordSize) new Double(elems1, elems2 & ~(1L << i))
      else this
    }
  }

  private class Multiple(elems: Array[Long]) extends BitSet {

    def contains(i: Int): Boolean = {
      val index = i / WordSize
      0 <= i && index < elems.length  && (elems(index) & (1L << i)) != 0
    }

    def elements: Iterator[Int] =
      Iterator.range(0, elems.length) flatMap (i => wordIterator(elems(i), i * WordSize))

    def +(i: Int): BitSet = {
      require(0 <= i)
      val index = i / WordSize
      val elems1 = new Array[Long](elems.length max (index + 1))
      Array.copy(elems, 0, elems1, 0, elems.length)
      elems1(index) = elems1(index) | (1L << i)
      new Multiple(elems1)
    }

    def -(i: Int): BitSet = {
      require(0 <= i)
      val index = i / WordSize
      if (index < elems.length && (elems(index) & (1L << i)) != 0) {
        val elems1 = new Array[Long](elems.length)
        Array.copy(elems, 0, elems1, 0, elems.length)
        elems1(index) = elems1(index) & ~(1L << i)
        new Multiple(elems1)
      } else this
    }
  }
}

import BitSet._

abstract class BitSet extends (Int => Boolean) {

  def contains(i: Int): Boolean

  def apply(i: Int): Boolean = contains(i)

  def elements: Iterator[Int]

  def +(i: Int): BitSet

  def -(i: Int): BitSet

  def + (elem1: Int, elem2: Int, elems: Int*): BitSet =
    this + elem1 + elem2 ++ elems

  def ++(elems: Iterator[Int]): BitSet =
    (this /: elems) (_ + _)

  def ++ (elems: Iterable[Int]): BitSet =
    this ++ elems.elements

  def - (elem1: Int, elem2: Int, elems: Int*): BitSet =
    this - elem1 - elem2 -- elems

  def -- (elems: Iterator[Int]): BitSet =
    (this /: elems) (_ - _)

  def -- (elems: Iterable[Int]): BitSet =
    this -- elems.elements

  def ** (that: BitSet): BitSet = filter(that.contains)

  def map(f: Int => Int): BitSet =
    (empty /: (elements map f)) (_ + _)

  def flatMap(f: Int => Iterable[Int]): BitSet =
    (empty /: (elements map f)) (_ ++ _)

  def filter(p: Int => Boolean): BitSet =
    (this /: elements) ((set, elem) => if (p(elem)) set else set - elem)

  override def toString: String = elements.mkString("BitSet(", ", ", ")")
}

