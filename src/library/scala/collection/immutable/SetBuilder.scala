package scala.collection.immutable

import scala.collection.mutable.ReusableBuilder

/**
  * A builder specialised for immutable Sets
  *
  * Note - this builder can be serially reused - it is NOT required to call `clear` after `result`
  */
abstract class SetBuilder [T, C <: Set[T]] private[immutable](empty: C)
  extends ReusableBuilder[T, C] {

  protected var elems: C = empty

  def clear(): Unit = { elems = empty }

  def result(): C = elems

  final def size: Int = elems.size
  final def isEmpty: Boolean = elems.isEmpty

  /**
    * are the types implementationally compatible, i.e value isInstanceOf[C]
    */
  protected def isCompatibleType(value: IterableOnce[T]) : Boolean

  override def addAll(xs: IterableOnce[T]): this.type = {
    if ((elems eq empty) && isCompatibleType(xs)) {
      elems = xs.asInstanceOf[C]
      this
    } else super.addAll(xs)
  }
}
