package scala.collection.jcl;

/** A map that is backed by a Java hash map.
  * @author Sean McDirmid
  */
class HashMap[K,E](override val underlying : java.util.HashMap) extends ConcreteMapWrapper[K,E] {
  def this() = this(new java.util.HashMap);
}
