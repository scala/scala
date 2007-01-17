package scala.collection.jcl;

/** A sorted map that is backed by a Java tree map.
  * @author Sean McDirmid
  */
class TreeMap[K <% Ordered[K],E] extends ConcreteMapWrapper[K,E] with SortedMapWrapper[K,E] {
  val underlying = (new java.util.TreeMap(new Comparator[K]));
}
