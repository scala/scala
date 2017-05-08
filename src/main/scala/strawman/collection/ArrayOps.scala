package strawman
package collection

import scala.{Array, Char, Int, AnyVal}
import scala.Predef.???
import mutable.ArrayBuffer

import scala.reflect.ClassTag
class ArrayOps[A](val xs: Array[A])
  extends AnyVal
     with SeqOps[A, immutable.Seq, Array[A]]  // should be IndexedSeq once we have an instance type
     with Buildable[A, Array[A]]
     with ArrayLike[A] {

  protected def coll = new ArrayView(xs)

  def length = xs.length
  def apply(i: Int) = xs.apply(i)

  override def view = new ArrayView(xs)

  def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

  protected[this] def fromTaggedIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]
  protected[this] def fromSpecificIterable(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)
  protected[this] def fromIterable[B](coll: Iterable[B]): immutable.Seq[B] = immutable.Seq.fromIterable(coll)

  protected[this] def newBuilder = new ArrayBuffer[A].mapResult(_.toArray(elemTag))

  override def knownSize = xs.length

  override def className = "Array"

  def iterator(): Iterator[A] = coll.iterator()
  def map[B: ClassTag](f: A => B): Array[B] = fromTaggedIterable(View.Map(coll, f))
  def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = fromTaggedIterable(View.FlatMap(coll, f))
  def ++[B >: A : ClassTag](xs: IterableOnce[B]): Array[B] = fromTaggedIterable(View.Concat(coll, xs))
  def zip[B: ClassTag](xs: IterableOnce[B]): Array[(A, B)] = fromTaggedIterable(View.Zip(coll, xs))
}

case class ArrayView[A](xs: Array[A]) extends IndexedView[A] {
  def length = xs.length
  def apply(n: Int) = xs(n)
  override def className = "ArrayView"
}
