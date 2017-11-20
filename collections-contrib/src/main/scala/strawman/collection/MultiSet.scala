package strawman
package collection

/**
  * A multiset is a set that can contain multiple occurrences of a same value.
  *
  * @tparam A the element type of the collection
  */
trait MultiSet[A]
  extends Iterable[A]
    with MultiSetOps[A, MultiSet, MultiSet[A]]
    with Equals {

  def canEqual(that: Any): Boolean = true

  override def equals(o: Any): Boolean = o match {
    case that: MultiSet[A] =>
      (this eq that) ||
        (that canEqual this) &&
          (this.size == that.size) && {
          try {
            occurrences forall { case (elem, n) => that.get(elem) == n }
          } catch {
            case _: ClassCastException => false
          }
        }
    case _ => false
  }

  override def hashCode(): Int = collection.Set.unorderedHash(occurrences, "MultiSet".##)

}

trait MultiSetOps[A, +CC[X] <: MultiSet[X], +C]
  extends IterableOps[A, CC, C] {

  protected[this] def fromOccurrences[E](it: Iterable[(E, Int)]): CC[E] =
    // Note new MultiSet(it.to(Map)) would be more efficient but would also loose duplicates
    fromIterable(it.flatMap { case (e, n) => View.Fill(n)(e) })

  /**
    * @return All the elements contained in this multiset and their number of occurrences
    */
  def occurrences: Map[A, Int]

  def iterator(): Iterator[A] =
    occurrences.iterator().flatMap { case (elem, n) => View.Fill(n)(elem) }

  /**
    * @return The number of occurrences of `elem` in this multiset
    * @param elem Element to look up
    */
  def get(elem: A): Int = occurrences.getOrElse(elem, 0)

  /**
    * @return Whether `elem` has at least one occurrence in this multiset or not
    * @param elem the element to test
    */
  def contains(elem: A): Boolean = occurrences.contains(elem)

  /**
    * @return a new multiset summing the occurrences of this multiset
    *         with the elements of `that`
    *
    * @param that the collection of elements to add to this multiset
    */
  def concat(that: Iterable[A]): CC[A] =
    fromIterable(View.Concat(toIterable, that))

  /**
    * @return a new multiset summing the occurrences of this multiset
    *         and `that` collection of occurrences
    *
    * @param that the collection of occurrences to add to this multiset
    */
  def concatOccurrences(that: Iterable[(A, Int)]): CC[A] =
    fromOccurrences(View.Concat(occurrences, that))

  /**
    * @return a new multiset resulting from applying the given function `f`
    *         to each pair of element and its number of occurrences of this
    *         multiset and collecting the results
    * @param f the function to apply
    * @tparam B the element type of the returned collection
    */
  def mapOccurrences[B](f: ((A, Int)) => (B, Int)): CC[B] =
    fromOccurrences(View.Map(occurrences, f))

  /**
    * @return a new multiset resulting from applying the given function `f`
    *         to each pair of element and its number of occurrences of this
    *         multiset and concatenating the results
    * @param f the function to apply
    * @tparam B the element type of the returned collection
    */
  def flatMapOccurrences[B](f: ((A, Int)) => IterableOnce[(B, Int)]): CC[B] =
    fromOccurrences(View.FlatMap(occurrences, f))

  // TODO Add more multiset operations like union and intersection

}

object MultiSet extends IterableFactory.Delegate(immutable.MultiSet)