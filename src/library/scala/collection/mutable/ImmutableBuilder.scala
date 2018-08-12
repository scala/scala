package scala
package collection
package mutable

import scala.reflect.{ClassTag, classTag}


/**
  * Reusable builder for immutable collections
  */
abstract class ImmutableBuilder[-A, C <: AnyRef : ClassTag](empty: C)
  extends ReusableBuilder[A, C] {

  protected var elems: C = empty

  def clear(): Unit = {elems = empty}

  def result(): C = elems

  protected def isCompatibleType(xs: IterableOnce[A]) =
    classTag[C].runtimeClass.isInstance(xs)

  override def addAll(xs: IterableOnce[A]): this.type = {
    if ((elems eq empty) && isCompatibleType(xs)) {
      elems = xs.asInstanceOf[C]
      this
    } else super.addAll(xs)
  }

}
