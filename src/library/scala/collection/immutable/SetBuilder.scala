package scala.collection.immutable

import scala.collection.mutable.ReusableBuilder
import scala.reflect.{ClassTag, classTag}

/**
  * A builder specialised for immutable Sets
  *
  * Note - this builder can be serially reused - it is NOT required to call `clear` after `result`
  */
class SetBuilder [T, C <: Set[T] : ClassTag] private[immutable](empty: C)
  extends ReusableBuilder[T, C] {

  protected var elems: C = empty

  def clear(): Unit = { elems = empty }

  def result(): C = elems

  final def size: Int = elems.size

  final def isEmpty: Boolean = elems.isEmpty

  /**
    * are the types implementationally compatible,
    */
  protected def isCompatibleType(value: IterableOnce[T]) : Boolean =
    classTag[C].runtimeClass.isInstance(value)

  def addOne(element: T): this.type = {
    elems = (elems + element).asInstanceOf[C]
    this
  }

  override def addAll(xs: IterableOnce[T]): this.type = {
    if ((elems eq empty) && isCompatibleType(xs)) {
      elems = xs.asInstanceOf[C]
      this
    } else super.addAll(xs)
  }
}
