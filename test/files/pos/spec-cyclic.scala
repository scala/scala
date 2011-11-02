trait AbsFun[@specialized -A, @specialized +B] {
  def apply(x: A): B
}

trait MyPartialFunction[-A, +B] extends AnyRef with AbsFun[A, B]

trait ColMap[A, +B] extends MyPartialFunction[A, B] /*with Collection[(A, B)] */

trait ColSorted[K,+A] extends ColRanged[K,A] 

trait ColSortedMap[K,+E] extends ColMap[K,E] with ColSorted[K,Tuple2[K,E]] 

trait MutMap[A, B] extends AnyRef
      with ColMap[A, B]

trait ColRanged[K, +A] //extends Iterable[A] 

trait JclRanged[K,A] extends ColRanged[K,A] //with MutableIterable[A] {

trait JclMap[K,E] extends /*collection.jcl.MutableIterable[Tuple2[K,E]] with*/ MutMap[K,E] 

trait JclSorted[K,A] extends ColSorted[K,A] with JclRanged[K,A]

trait JclSortedMap[K,E] extends ColSortedMap[K,E] with JclMap[K,E] with JclSorted[K,Tuple2[K,E]]

class Foo[A, B] extends JclSortedMap[A, B] {
  def apply(x: A): B = error("NYI")
}

class Bar {
  val x: Foo[Int, Int] = new Foo[Int, Int]
  x.apply(0)
}
