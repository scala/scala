package scala.collection.jcl;

/** A map that is backed by a Java weak hash map, whose keys are maintained as weak references.
 *  Because keys are weak references, the garbage collector can collect them if they are not referred to elsewhere.
 *  Useful for implementing caches.
 *
 *  @author Sean McDirmid
 */
class WeakHashMap[K,E](override val underlying : java.util.WeakHashMap) extends ConcreteMapWrapper[K,E] {
  def this() = this(new java.util.WeakHashMap);
}
