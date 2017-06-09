package strawman
package collection.mutable

import scala.Unit

/** The canonical builder for collections that are growable, i.e. that support an
  * efficient `+=` method which adds an element to the collection.
  *
  * GrowableBuilders can produce only a single instance of the collection they are growing.
  *
  * @author Paul Phillips
  * @version 2.8
  * @since 2.8
  *
  * @define Coll `GrowingBuilder`
  * @define coll growing builder
  */
class GrowableBuilder[Elem, To <: Growable[Elem]](elems: To) extends Builder[Elem, To] {
  def clear(): Unit = elems.clear()
  def result(): To = elems
  def add(elem: Elem): this.type = { elems += elem; this }
}
