package scala.collection.immutable

import scala.collection.mutable.ReusableBuilder
import scala.reflect.{ClassTag, classTag}
/**
  * A builder specialised for immutable Maps
  *
  * Note - this builder can be serially reused - it is NOT required to call `clear` after `result`
  */
class MapBuilder [K, V, CC[K,V] <: Map[K,V]] private[immutable](empty: CC[K,V])
  extends ReusableBuilder[(K,V), CC[K,V]] {

  protected var elems: CC[K,V] = empty

  def clear(): Unit = { elems = empty }

  def result(): CC[K,V] = elems

  final def size: Int = elems.size

  final def isEmpty: Boolean = elems.isEmpty

  def addOne(elem: (K, V)): this.type = {
    elems = (elems + elem).asInstanceOf[CC[K,V]]
    this
  }

  /**
    * Similar functionallity to [[addOne()]] without requiring the creation of tuples
    */
  def addKeyValue(key: K, value: V): this.type = {
    elems = (elems.updated[V](key,value)).asInstanceOf[CC[K,V]]
    this
  }

  override def addAll(xs: IterableOnce[(K, V)]): this.type = {
    //TODO optimise in children for the special cases if it cant be doe here ??
    // elems = (elems concat xs).asInstanceOf[CC[K,V]]
    super.addAll(xs)
    this
  }
}
