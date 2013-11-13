class Order[t](less:(t,t) => Boolean,equal:(t,t) => Boolean) {}

trait Map[A, B] extends scala.collection.Map[A, B] {
  val factory:MapFactory[A]
}
abstract class MapFactory[A] {
  def Empty[B]:Map[A,B];
}

class TreeMapFactory[KEY](newOrder:Order[KEY]) extends MapFactory[KEY] {
  val order = newOrder;
  def Empty[V] = new TreeMap[KEY,V](new TreeMapFactory[KEY](order));
}

class Tree[KEY,Entry](order:Order[KEY]) {
  def size =0;
}

class TreeMap[KEY,VALUE](_factory:TreeMapFactory[KEY]) extends Tree[KEY,Tuple2[KEY,VALUE]](_factory.order) with scala.collection.DefaultMap[KEY, VALUE] with Map[KEY, VALUE] {
  val factory = _factory
  val order = _factory.order;
  def this(newOrder:Order[KEY]) = this(new TreeMapFactory[KEY](newOrder));
  def get(key:KEY) = null;
  def iterator:Iterator[Tuple2[KEY,VALUE]] = null;
  override def size = super[Tree].size
}
