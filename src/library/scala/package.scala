/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package object scala {
  type Iterable[+A] = scala.collection.Iterable[A]
  val Iterable = scala.collection.Iterable

  @deprecated("use Iterable instead") type Collection[+A] = Iterable[A]
  @deprecated("use Iterable instead") val Collection = Iterable

  type Seq[+A] = scala.collection.Seq[A]
  val Seq = scala.collection.Seq

  @deprecated("use Seq instead") type Sequence[+A] = scala.collection.Seq[A]

  type RandomAccessSeq[+A] = scala.collection.Vector[A]
  val RandomAccessSeq = scala.collection.Vector

  type Iterator[+A] = scala.collection.Iterator[A]
  val Iterator = scala.collection.Iterator

  type BufferedIterator[+A] = scala.collection.BufferedIterator[A]

  type List[+A] = scala.collection.immutable.List[A]
  val List = scala.collection.immutable.List

  val Nil = scala.collection.immutable.Nil

  type ::[A] = scala.collection.immutable.::[A]
  val :: = scala.collection.immutable.::

  type Stream[+A] = scala.collection.immutable.Stream[A]
  val Stream = scala.collection.immutable.Stream

  type StringBuilder = scala.collection.mutable.StringBuilder
  val StringBuilder = scala.collection.mutable.StringBuilder
}
