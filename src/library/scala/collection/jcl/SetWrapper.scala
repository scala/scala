package scala.collection.jcl;

/** Used to wrap Java sets.
 **
 ** @author Sean McDirmid
 **/
trait SetWrapper[A] extends CollectionWrapper[A] with Set[A] {
  protected def underlying : java.util.Set;
  override def isEmpty = super[Set].isEmpty;
}
