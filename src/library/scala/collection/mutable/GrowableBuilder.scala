package scala
package collection.mutable


/** The canonical builder for collections that are growable, i.e. that support an
  * efficient `+=` method which adds an element to the collection.
  *
  * GrowableBuilders can produce only a single instance of the collection they are growing.
  *
  * @author Paul Phillips
  * @since 2.8
  *
  * @define Coll `GrowingBuilder`
  * @define coll growing builder
  */
class GrowableBuilder[Elem, To <: Growable[Elem]](protected val elems: To)
  extends Builder[Elem, To] {

  def clear(): Unit = elems.clear()

  def result(): To = elems

  def addOne(elem: Elem): this.type = { elems += elem; this }

}
