package strawman.collection.mutable

import scala.{Array, Int, Boolean, Unit, AnyRef}
import scala.Predef.intWrapper
import strawman.collection.{IndexedView, Iterable, IterableFactory, IterableOnce, Seq, SeqLike}

/** Concrete collection type: ArrayBuffer */
class ArrayBuffer[A] private (initElems: Array[AnyRef], initLength: Int)
  extends Seq[A]
    with SeqLike[A, ArrayBuffer]
    with Buildable[A, ArrayBuffer[A]]
    with Builder[A, ArrayBuffer[A]] {

  def this() = this(new Array[AnyRef](16), 0)

  private var elems: Array[AnyRef] = initElems
  private var start = 0
  private var end = initLength

  def apply(n: Int) = elems(start + n).asInstanceOf[A]

  def length = end - start
  override def knownSize = length

  override def view = new ArrayBufferView(elems, start, end)

  def iterator() = view.iterator()

  def fromIterable[B](it: Iterable[B]): ArrayBuffer[B] =
    ArrayBuffer.fromIterable(it)

  protected[this] def newBuilder = new ArrayBuffer[A]

  def +=(elem: A): this.type = {
    if (end == elems.length) {
      if (start > 0) {
        Array.copy(elems, start, elems, 0, length)
        end -= start
        start = 0
      }
      else {
        val newelems = new Array[AnyRef](end * 2)
        Array.copy(elems, 0, newelems, 0, end)
        elems = newelems
      }
    }
    elems(end) = elem.asInstanceOf[AnyRef]
    end += 1
    this
  }

  def result = this

  /** New operation: destructively drop elements at start of buffer. */
  def trimStart(n: Int): Unit = start += (n max 0)

  /** Overridden to use array copying for efficiency where possible. */
  override def ++[B >: A](xs: IterableOnce[B]): ArrayBuffer[B] = xs match {
    case xs: ArrayBuffer[B] =>
      val elems = new Array[AnyRef](length + xs.length)
      Array.copy(this.elems, this.start, elems, 0, this.length)
      Array.copy(xs.elems, xs.start, elems, this.length, xs.length)
      new ArrayBuffer(elems, elems.length)
    case _ => super.++(xs)
  }

  override def take(n: Int) = {
    val elems = new Array[AnyRef](n min length)
    Array.copy(this.elems, this.start, elems, 0, elems.length)
    new ArrayBuffer(elems, elems.length)
  }

  override def className = "ArrayBuffer"
}

object ArrayBuffer extends IterableFactory[ArrayBuffer] {

  /** Avoid reallocation of buffer if length is known. */
  def fromIterable[B](coll: Iterable[B]): ArrayBuffer[B] =
    if (coll.knownSize >= 0) {
      val elems = new Array[AnyRef](coll.knownSize)
      val it = coll.iterator()
      for (i <- 0 until elems.length) elems(i) = it.next().asInstanceOf[AnyRef]
      new ArrayBuffer[B](elems, elems.length)
    }
    else new ArrayBuffer[B] ++= coll
}

class ArrayBufferView[A](val elems: Array[AnyRef], val start: Int, val end: Int) extends IndexedView[A] {
  def length = end - start
  def apply(n: Int) = elems(start + n).asInstanceOf[A]
  override def className = "ArrayBufferView"
}
