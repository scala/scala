package scala.collection
package generic

import mutable.{Builder, ArrayBuffer}
import scala.specialized

/** A factory class for rows -- flat arrays of value classes that use the unboxed representation
 *  of the element type.
 *  
 */
abstract class RowFactory[@specialized Unboxed: ClassManifest, Boxed <: scala.Boxed[Unboxed]] extends (Unboxed => Boxed) { box =>
  
  /** Convert to boxed representation
   */
  def apply(x: Unboxed): Boxed

  class Row(elems: Array[Unboxed]) extends mutable.IndexedSeq[Boxed] with mutable.IndexedSeqLike[Boxed, Row] {

    override protected[this] def newBuilder: Builder[Boxed, Row] = Row.newBuilder
    
    def apply(idx: Int): Boxed = box(elems(idx))
    
    def update(idx: Int, elem: Boxed): Unit = elems(idx) = elem.unbox
    
    def length = elems.length
    
    override def foreach[U](f: Boxed => U): Unit = elems foreach (elem => f(box(elem)))

  }

  object Row {
    def fromSeq(elems: Seq[Boxed]): Row = {
      val xs: Array[Unboxed] = new Array[Unboxed](elems.length)
      var i = 0
      for (elem <- elems) { xs(i) = elem.unbox; i += 1 }
      new Row(xs)
    }
      
    def apply(elems: Boxed*) = fromSeq(elems)
    
    def newBuilder: Builder[Boxed, Row] = new ArrayBuffer mapResult fromSeq
    
    implicit def canBuildFrom: CanBuildFrom[Row, Boxed, Row] = 
      new CanBuildFrom[Row, Boxed, Row] {
        def apply(): Builder[Boxed, Row] = newBuilder
        def apply(from: Row): Builder[Boxed, Row] = newBuilder
      }
  }
}
