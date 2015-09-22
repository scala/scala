import scala.annotation._
import scala.annotation.unchecked._
import scala.collection._


package object buffer {
  val broken = new ArrayVec2() // commenting out this line causes the file to compile.

  val works = Class.forName("buffer.ArrayVec2").newInstance().asInstanceOf[ArrayVec2]
}

package buffer {
  object Main {
    // ArrayVec2 can be compiled, instantiated and used.
    def main(args: Array[String]) { println(works) }
  }

  trait ElemType { type Element; type Component <: ElemType }
  trait Float1 extends ElemType { type Element = Float; type Component = Float1}
  class Vec2 extends ElemType { type Element = Vec2;  type Component = Float1 }

  abstract class BaseSeq[T <: ElemType, E]
  extends IndexedSeq[E] with IndexedSeqOptimized[E, IndexedSeq[E]] {
    def length = 1
    def apply(i: Int) :E
  }

  abstract class GenericSeq[T <: ElemType] extends BaseSeq[T, T#Element]
  trait DataArray[T <: ElemType] extends BaseSeq[T, T#Element]
  trait DataView[T <: ElemType] extends BaseSeq[T, T#Element]
  abstract class BaseFloat1 extends BaseSeq[Float1, Float]

  class ArrayFloat1 extends BaseFloat1 with DataArray[Float1] {
    def apply(i: Int) :Float = 0f
  }

  class ViewFloat1 extends BaseFloat1 with DataView[Float1] {
    def apply(i: Int) :Float = 0f
  }

  class ArrayVec2(val backingSeq: ArrayFloat1)
  extends GenericSeq[Vec2] with DataArray[Vec2] {
    def this() = this(new ArrayFloat1)
    def apply(i: Int) :Vec2 = null
  }
}
