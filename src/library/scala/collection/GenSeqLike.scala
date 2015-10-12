/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import generic._

/** A template trait for all sequences which may be traversed
 *  in parallel.
 *
 *  @define Coll GenSeq
 *  @define coll general sequence
 *  @define mayNotTerminateInf
 *
 *    Note: may not terminate for infinite-sized collections.
 *  @define willNotTerminateInf
 *
 *    Note: will not terminate for infinite-sized collections.
 *
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 *  @define seqInfo
 *  Sequences are special cases of iterable collections of class `Iterable`.
 *  Unlike iterables, sequences always have a defined order of elements.
 */
trait GenSeqLike[+A, +Repr] extends Any with GenIterableLike[A, Repr] with Equals with Parallelizable[A, parallel.ParSeq[A]] {
  def seq: Seq[A]

  /** Selects an element by its index in the $coll.
   *
   * Example:
   *
   * {{{
   *    scala> val x = List(1, 2, 3, 4, 5)
   *    x: List[Int] = List(1, 2, 3, 4, 5)
   *
   *    scala> x(3)
   *    res1: Int = 4
   * }}}
   *
   *  @param  idx  The index to select.
   *  @return the element of this $coll at index `idx`, where `0` indicates the first element.
   *  @throws IndexOutOfBoundsException if `idx` does not satisfy `0 <= idx < length`.
   */
  def apply(idx: Int): A

  /** The length of the $coll.
   *
   *  $willNotTerminateInf
   *
   *  Note: `xs.length` and `xs.size` yield the same result.
   *
   *  @return     the number of elements in this $coll.
   */
  def length: Int

  /** Tests whether this $coll contains given index.
   *
   *  The implementations of methods `apply` and `isDefinedAt` turn a `Seq[A]` into
   *  a `PartialFunction[Int, A]`.
   *
   * @param    idx     the index to test
   * @return   `true` if this $coll contains an element at position `idx`, `false` otherwise.
   */
  def isDefinedAt(idx: Int): Boolean = (idx >= 0) && (idx < length)

  /** Computes length of longest segment whose elements all satisfy some predicate.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @param   from  the index where the search starts.
   *  @return  the length of the longest segment of this $coll starting from index `from`
   *           such that every element of the segment satisfies the predicate `p`.
   */
  def segmentLength(p: A => Boolean, from: Int): Int

  /** Returns the length of the longest prefix whose elements all satisfy some predicate.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the length of the longest prefix of this $coll
   *           such that every element of the segment satisfies the predicate `p`.
   */
  def prefixLength(p: A => Boolean): Int = segmentLength(p, 0)

  /** Finds index of the first element satisfying some predicate after or at some start index.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def indexWhere(p: A => Boolean, from: Int): Int

  /** Finds index of first element satisfying some predicate.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index of the first element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def indexWhere(p: A => Boolean): Int = indexWhere(p, 0)

  /** Finds index of first occurrence of some value in this $coll.
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index of the first element of this $coll that is equal (as determined by `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def indexOf(elem: A): Int
   *    @inheritdoc
   *
   *    $mayNotTerminateInf
   *
   */
  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  /** Finds index of first occurrence of some value in this $coll after or at some start index.
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this $coll that is equal (as determined by `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def indexOf(elem: A, from: Int): Int
   *    @inheritdoc
   *
   *    $mayNotTerminateInf
   *
   */
  def indexOf[B >: A](elem: B, from: Int): Int = indexWhere(elem == _, from)

  /** Finds index of last occurrence of some value in this $coll.
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index of the last element of this $coll that is equal (as determined by `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def lastIndexOf(elem: A): Int
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   */
  def lastIndexOf[B >: A](elem: B): Int = lastIndexWhere(elem == _)

  /** Finds index of last occurrence of some value in this $coll before or at a given end index.
   *
   *  @param   elem   the element value to search for.
   *  @param   end    the end index.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index `<= end` of the last element of this $coll that is equal (as determined by `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def lastIndexOf(elem: A, end: Int): Int
   *    @inheritdoc
   */
  def lastIndexOf[B >: A](elem: B, end: Int): Int = lastIndexWhere(elem == _, end)

  /** Finds index of last element satisfying some predicate.
   *
   *  $willNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index of the last element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean): Int = lastIndexWhere(p, length - 1)

  /** Finds index of last element satisfying some predicate before or at given end index.
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean, end: Int): Int

  /** Returns new $coll with elements in reversed order.
   *
   *  $willNotTerminateInf
   *
   *  @return A new $coll with all elements of this $coll in reversed order.
   */
  def reverse: Repr

  /**
   *  Builds a new collection by applying a function to all elements of this $coll and
   *  collecting the results in reversed order.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results in reversed order.
   *
   *  @usecase def reverseMap[B](f: A => B): $Coll[B]
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   *    Note: `xs.reverseMap(f)` is the same as `xs.reverse.map(f)` but might be more efficient.
   *
   *    @return       a new $coll resulting from applying the given function
   *                  `f` to each element of this $coll and collecting the results in reversed order.
   */
  def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** Tests whether this $coll starts with the given sequence.
   *
   * @param  that    the sequence to test
   * @return `true` if this collection has `that` as a prefix, `false` otherwise.
   */
  def startsWith[B](that: GenSeq[B]): Boolean = startsWith(that, 0)

  /** Tests whether this $coll contains the given sequence at a given index.
   *
   * '''Note''': If the both the receiver object `this` and the argument
   * `that` are infinite sequences this method may not terminate.
   *
   * @param  that    the sequence to test
   * @param  offset  the index where the sequence is searched.
   * @return `true` if the sequence `that` is contained in this $coll at
   *         index `offset`, otherwise `false`.
   */
  def startsWith[B](that: GenSeq[B], offset: Int): Boolean

  /** Tests whether this $coll ends with the given sequence.
   *  $willNotTerminateInf
   *  @param  that    the sequence to test
   *  @return `true` if this $coll has `that` as a suffix, `false` otherwise.
   */
  def endsWith[B](that: GenSeq[B]): Boolean

  /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
   *
   *  @param  from     the index of the first replaced element
   *  @param  patch    the replacement sequence
   *  @param  replaced the number of elements to drop in the original $coll
   *  @tparam B        the element type of the returned $coll.
   *  @tparam That     $thatinfo
   *  @param bf        $bfinfo
   *  @return          a new $coll consisting of all elements of this $coll
   *                   except that `replaced` elements starting from `from` are replaced
   *                   by `patch`.
   *
   *  @usecase def patch(from: Int, that: GenSeq[A], replaced: Int): $Coll[A]
   *    @inheritdoc
   *
   *    @return          a new $coll consisting of all elements of this $coll
   *                     except that `replaced` elements starting from `from` are replaced
   *                     by `patch`.
   */
  def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** A copy of this $coll with one single replaced element.
   *  @param  index  the position of the replacement
   *  @param  elem   the replacing element
   *  @tparam B        the element type of the returned $coll.
   *  @tparam That     $thatinfo
   *  @param bf        $bfinfo
   *  @return a new $coll which is a copy of this $coll with the element at position `index` replaced by `elem`.
   *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
   *
   *  @usecase def updated(index: Int, elem: A): $Coll[A]
   *    @inheritdoc
   *
   *    @return a copy of this $coll with the element at position `index` replaced by `elem`.
   */
  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** A copy of the $coll with an element prepended.
   *
   *  @param  elem   the prepended element
   *  @tparam B      the element type of the returned $coll.
   *  @tparam That   $thatinfo
   *  @param bf      $bfinfo
   *  @return a new collection of type `That` consisting of `elem` followed
   *          by all elements of this $coll.
   *
   *  @usecase def +:(elem: A): $Coll[A]
   *    @inheritdoc
   *
   *    Note that :-ending operators are right associative (see example).
   *    A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   *
   *    Also, the original $coll is not modified, so you will want to capture the result.
   *
   *    Example:
   *    {{{
   *      scala> val x = List(1)
   *      x: List[Int] = List(1)
   *
   *      scala> val y = 2 +: x
   *      y: List[Int] = List(2, 1)
   *
   *      scala> println(x)
   *      List(1)
   *    }}}
   *
   *    @return a new $coll consisting of `elem` followed
   *            by all elements of this $coll.
   */
  def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** A copy of this $coll with an element appended.
   *
   *  A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   *
   *  @param  elem   the appended element
   *  @tparam B      the element type of the returned $coll.
   *  @tparam That   $thatinfo
   *  @param bf      $bfinfo
   *  @return a new collection of type `That` consisting of
   *          all elements of this $coll followed by `elem`.
   *
   *  @usecase def :+(elem: A): $Coll[A]
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   *    Example:
   *    {{{
   *       scala> val a = List(1)
   *       a: List[Int] = List(1)
   *       
   *       scala> val b = a :+ 2
   *       b: List[Int] = List(1, 2)
   *       
   *       scala> println(a)
   *       List(1)
   *    }}}
   *
   *    @return a new $coll consisting of
   *            all elements of this $coll followed by `elem`.
   */
  def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** A copy of this $coll with an element value appended until a given target length is reached.
   *
   *  @param   len   the target length
   *  @param   elem  the padding value
   *  @tparam B      the element type of the returned $coll.
   *  @tparam That   $thatinfo
   *  @param bf      $bfinfo
   *  @return a new collection of type `That` consisting of
   *          all elements of this $coll followed by the minimal number of occurrences of `elem` so
   *          that the resulting collection has a length of at least `len`.
   *  @usecase def padTo(len: Int, elem: A): $Coll[A]
   *    @inheritdoc
   *
   *    @return a new $coll consisting of
   *            all elements of this $coll followed by the minimal number of occurrences of `elem` so
   *            that the resulting $coll has a length of at least `len`.
   */
  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Repr, B, That]): That

  /** Tests whether every element of this $coll relates to the
   *  corresponding element of another sequence by satisfying a test predicate.
   *
   *  @param   that  the other sequence
   *  @param   p     the test predicate, which relates elements from both sequences
   *  @tparam  B     the type of the elements of `that`
   *  @return  `true` if both sequences have the same length and
   *                  `p(x, y)` is `true` for all corresponding elements `x` of this $coll
   *                  and `y` of `that`, otherwise `false`.
   */
  def corresponds[B](that: GenSeq[B])(p: (A, B) => Boolean): Boolean

  def toSeq: GenSeq[A]

  /** Produces a new sequence which contains all elements of this $coll and also all elements of
   *  a given sequence. `xs union ys`  is equivalent to `xs ++ ys`.
   *
   *  @param that   the sequence to add.
   *  @tparam B     the element type of the returned $coll.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                followed by all elements of `that`.
   *
   *  @usecase def union(that: GenSeq[A]): $Coll[A]
   *    @inheritdoc
   *
   *    Another way to express this
   *    is that `xs union ys` computes the order-preserving multi-set union of `xs` and `ys`.
   *    `union` is hence a counter-part of `diff` and `intersect` which also work on multi-sets.
   *
   *    $willNotTerminateInf
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  followed by all elements of `that`.
   */
  def union[B >: A, That](that: GenSeq[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = this ++ that

  /** Computes the multiset difference between this $coll and another sequence.
   *
   *  @param that   the sequence of elements to remove
   *  @tparam B     the element type of the returned $coll.
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                except some of occurrences of elements that also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                part of the result, but any following occurrences will.
   *
   *  @usecase def diff(that: GenSeq[A]): $Coll[A]
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  except some of occurrences of elements that also appear in `that`.
   *                  If an element value `x` appears
   *                  ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
   *                  part of the result, but any following occurrences will.
   */
  def diff[B >: A](that: GenSeq[B]): Repr

  /** Computes the multiset intersection between this $coll and another sequence.
   *
   *  @param that   the sequence of elements to intersect with.
   *  @tparam B     the element type of the returned $coll.
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                which also appear in `that`.
   *                If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                in the result, but any following occurrences will be omitted.
   *
   *  @usecase def intersect(that: GenSeq[A]): $Coll[A]
   *    @inheritdoc
   *
   *    $mayNotTerminateInf
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  which also appear in `that`.
   *                  If an element value `x` appears
   *                  ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
   *                  in the result, but any following occurrences will be omitted.
   */
  def intersect[B >: A](that: GenSeq[B]): Repr

  /** Builds a new $coll from this $coll without any duplicate elements.
   *  $willNotTerminateInf
   *
   *  @return  A new $coll which contains the first occurrence of every element of this $coll.
   */
  def distinct: Repr

  /** Hashcodes for $Coll produce a value from the hashcodes of all the
   *  elements of the $coll.
   */
  override def hashCode()= scala.util.hashing.MurmurHash3.seqHash(seq)

  /** The equals method for arbitrary sequences. Compares this sequence to
   *  some other object.
   *  @param    that  The object to compare the sequence to
   *  @return   `true` if `that` is a sequence that has the same elements as
   *            this sequence in the same order, `false` otherwise
   */
  override def equals(that: Any): Boolean = that match {
    case that: GenSeq[_] => (that canEqual this) && (this sameElements that)
    case _               => false
  }

}
