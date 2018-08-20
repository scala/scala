package scala.collection.immutable

import scala.collection.mutable.ReusableBuilder
import scala.reflect.{ClassTag, classTag}

/**
  * A builder specialised for immutable Sets
  *
  * Note - this builder can be serially reused - it is NOT required to call `clear` after `result`
  */
class SetBuilder [T, CC[T] <: Set[T]] private[immutable](empty: CC[T])
  extends ReusableBuilder[T, CC[T]] {

  protected var elems: CC[T] = empty

  def clear(): Unit = { elems = empty }

  def result(): CC[T] = elems

  final def size: Int = elems.size

  final def isEmpty: Boolean = elems.isEmpty

  def addOne(element: T): this.type = {
    elems = (elems + element).asInstanceOf[CC[T]]
    this
  }

  override def addAll(xs: IterableOnce[T]): this.type = {
    elems = (elems concat xs).asInstanceOf[CC[T]]
    this
  }
}
