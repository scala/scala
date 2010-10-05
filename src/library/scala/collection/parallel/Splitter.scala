package scala.collection.parallel


import scala.collection.Seq


/** A splitter (or a split iterator) can be split into more splitters that traverse over
 *  disjoint subsets of elements.
 *
 *  @tparam T    type of the elements this parallel iterator traverses
 *
 *  @since 2.8.1
 *  @author prokopec
 */
trait Splitter[+T] extends Iterator[T] {

  /** Splits the iterator into a sequence of disjunct views.
   *
   *  Returns a sequence of split iterators, each iterating over some subset of the
   *  elements in the collection. These subsets are disjoint and should be approximately
   *  equal in size. These subsets are not empty, unless the iterator is empty in which
   *  case this method returns a sequence with a single empty iterator. If the iterator has
   *  more than two elements, this method will return two or more iterators.
   *
   *  Implementors are advised to keep this partition relatively small - two iterators are
   *  already enough when partitioning the collection, although there may be a few more.
   *
   *  '''Note:''' this method actually invalidates the current iterator.
   *
   *  @return a sequence of disjunct iterators of the collection
   */
  def split: Seq[Splitter[T]]

}


object Splitter {
  def empty[T]: Splitter[T] = new Splitter[T] {
    def hasNext = false
    def next = Iterator.empty.next
    def split = Seq(this)
  }
}


/** A precise splitter (or a precise split iterator) can be split into arbitrary number of splitters
 *  that traverse disjoint subsets of arbitrary sizes.
 *
 *  Implementors might want to override the parameterless `split` method for efficiency.
 *
 *  @tparam T     type of the elements this parallel iterator traverses
 *
 *  @since 2.8.1
 *  @author prokopec
 */
trait PreciseSplitter[+T] extends Splitter[T] {

  /** Splits the iterator into disjunct views.
   *
   *  This overloaded version of the `split` method is specific to precise parallel iterators.
   *  It returns a sequence of parallel iterators, each iterating some subset of the
   *  elements in this iterator. The sizes of the subiterators in the partition is equal to
   *  the size in the corresponding argument, as long as there are enough elements in this
   *  iterator to split it that way.
   *
   *  If there aren't enough elements, a zero element iterator is appended for each additional argument.
   *  If there are additional elements, an additional iterator is appended at the end to compensate.
   *
   *  For example, say we have a parallel iterator `ps` with 100 elements. Invoking:
   *  {{{
   *    ps.split(50, 25, 25, 10, 5)
   *  }}}
   *  will return a sequence of five iterators, last two views being empty. On the other hand, calling:
   *  {{{
   *    ps.split(50, 40)
   *  }}}
   *  will return a sequence of three iterators, last of them containing ten elements.
   *
   *  '''Note:''' this method actually invalidates the current iterator.
   *
   *  Unlike the case with `split` found in parallel iterable iterators, views returned by this method can be empty.
   *
   *  @param sizes   the sizes used to split this split iterator into iterators that traverse disjunct subsets
   *  @return        a sequence of disjunct subsequence iterators of this parallel iterator
   */
  def psplit(sizes: Int*): Seq[PreciseSplitter[T]]

  def split: Seq[PreciseSplitter[T]]

}





