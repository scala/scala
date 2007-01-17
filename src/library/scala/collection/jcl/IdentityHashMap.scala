package scala.collection.jcl;

/** A map that is backed by a Java identity hash map, which compares keys by their reference-based identity
   * as opposed to using equals and hashCode. An identity hash map will often perform better than traditional hash map because it can utilize linear probing.
   * @author Sean McDirmid
   */
class IdentityHashMap[K,E](override val underlying : java.util.IdentityHashMap) extends ConcreteMapWrapper[K,E] {
  def this() = this(new java.util.IdentityHashMap);
}
