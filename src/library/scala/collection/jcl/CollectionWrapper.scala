package scala.collection.jcl;

/** Used to wrap Java collections in Scala.
 **
 ** @author Sean McDirmid
 **/
trait CollectionWrapper[A] extends Collection[A] with IterableWrapper[A] {
  /** Override to specify the collection being accessed through this wrapper.
   ** Collection operations are then routed through the wrapped Java collection.
   **/
  protected def underlying : java.util.Collection;
  private[jcl] final def underlying0 = underlying;
  override def has(a : A) = underlying.contains(a);
  override def elements : MutableIterator[A] = super.elements;

  override def hasAll(that : Iterable[A]) = that match {
  case that : CollectionWrapper[_] => underlying.containsAll(that.underlying);
  case _ => super.hasAll(that);
  }
  override def add(a : A) = underlying.add(a);
  override def addAll(that : Iterable[A]) = that match {
  case that : CollectionWrapper[_] => underlying.addAll(that.underlying);
  case _ => super.addAll(that);
  }
}
