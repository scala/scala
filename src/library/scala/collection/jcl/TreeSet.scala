package scala.collection.jcl;

/** Creates a sorted set that is backed by an underlying Java tree set. Elements
  * of the sorted set are ordered with respect to the ordered view bound of A.
  *
  * @author Sean McDirmid
  */
class TreeSet[A <% Ordered[A]] extends ConcreteWrapper[A] with SortedSetWrapper[A] {
  val underlying = new java.util.TreeSet(new Comparator[A]);
}
