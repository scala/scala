package scala.collection.immutable;

/** A set that uses TreeMap.
*/

class TreeSet[A](order: Order[A]) with Set[A] {

  protected val map = new TreeMap[ A, boolean ]( order );

  /** Returns the number of elements in this set.
  *
  *  @return number of set elements.
  */
  def size: Int = map.size;

  /** Checks if this set contains element <code>elem</code>.
  *
  *  @param  elem    the element to check for membership.
  *  @return true, iff <code>elem</code> is contained in this set.
  */
  def contains(elem: A): Boolean = map.get(elem) match {
    case Some(_) => true;
    case _ => false;
  }

  /** This method creates a new set with an additional element.
  */
  def +(elem: A): TreeSet[A] = new TreeSet(order) {
    override val map = TreeSet.this.map.update( elem, true );
  }

  /** <code>-</code> can be used to remove a single element from
  *  a set.
  */
  def -(elem: A): TreeSet[A] = new TreeSet(order) {
    override val map = TreeSet.this.map - elem ;
  }

  /** Creates a new iterator over all elements contained in this
  *  object.
  *
  *  @return the new iterator
  */
  def elements: Iterator[A] = map.elements.map {
    x:Pair[A,boolean] => x._1
  };

  /** Transform this set into a list of all elements.
  *
  *  @return  a list which enumerates all elements of this set.
  */
  override def toList: List[A] = elements.toList;

  /** Compares two sets for equality.
  *   Two set are equal iff they contain the same elements.
  */
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[scala.collection.Set[A]]) {
      val that = obj.asInstanceOf[scala.collection.Set[A]];
      if (size != that.size) false else toList.forall(that.contains);
    } else
      false;

  override def hashCode(): Int = map.hashCode();

}
