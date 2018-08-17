package scala.collection.immutable

import scala.collection.mutable.ReusableBuilder
import scala.reflect.{ClassTag, classTag}
/**
  * A builder specialised for immutable Maps
  *
  * Note - this builder can be serially reused - it is NOT required to call `clear` after `result`
  */
class MapBuilder [K, V, C <: Map[K, V] : ClassTag ] private[immutable](empty: C)
  extends ReusableBuilder[(K,V), C] {

  protected var elems: C = empty

  def clear(): Unit = { elems = empty }

  def result(): C = elems

  final def size: Int = elems.size

  final def isEmpty: Boolean = elems.isEmpty

  /**
    * are the types implementationally compatible
    */
  protected def isCompatibleType(value: IterableOnce[(K, V)]): Boolean =
    classTag[C].runtimeClass.isInstance(value)

  def addOne(elem: (K, V)): this.type = {
    elems = (elems + elem).asInstanceOf[C]; this
  }

  /**
    * Similar functionallity to [[addOne()]] without requiring the creation of tuples
    */
  def addKeyValue(key: K, value: V): this.type = {
    elems = (elems.updated(key,value)).asInstanceOf[C]; this
  }

  override def addAll(xs: IterableOnce[(K, V)]): this.type = {
    if ((elems eq empty) && isCompatibleType(xs)) {
      elems = xs.asInstanceOf[C]
      this
    } else super.addAll(xs)
  }
}
