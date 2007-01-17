package scala.collection.jcl;

/** A hash set that is backed by a Java hash set.
 **
 ** @author Sean McDirmid
 **/
class HashSet[A](override val underlying : java.util.HashSet) extends ConcreteWrapper[A] with SetWrapper[A] {
  /** Creates an underlying Java hash set. */
  def this() = this(new java.util.HashSet);
}
