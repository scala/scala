package scala.collection.jcl;

/** A set that is backed by a Java linked hash set, which fixes iteration order in terms of insertion order.
   * @author Sean McDirmid
   */
class LinkedHashSet[A](override val underlying : java.util.LinkedHashSet) extends ConcreteWrapper[A] with SetWrapper[A] {
  def this() = this(new java.util.LinkedHashSet);
}
