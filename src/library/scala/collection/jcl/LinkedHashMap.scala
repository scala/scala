package scala.collection.jcl;

/** A map that is backed by a Java linked hash map, which fixes iteration order in terms of insertion order.
   * @author Sean McDirmid
   */
class LinkedHashMap[K,E](override val underlying : java.util.LinkedHashMap) extends ConcreteMapWrapper[K,E] {
  def this() = this(new java.util.LinkedHashMap);
}
