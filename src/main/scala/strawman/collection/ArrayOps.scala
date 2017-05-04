package strawman.collection

import strawman.collection.immutable.List

import scala.{Array, Char, Int, AnyVal}
import scala.Predef.???
import strawman.collection.mutable.{ArrayBuffer, StringBuilder}

import scala.reflect.ClassTag
class ArrayOps[A](val xs: Array[A])
  extends AnyVal
     with SeqOps[A, Array[A]]
     with Buildable[A, Array[A]]
     with ArrayLike[A] {

  protected def coll = new ArrayView(xs)
  def iterator() = coll.iterator()

  def length = xs.length
  def apply(i: Int) = xs.apply(i)

  override def view = new ArrayView(xs)

  def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

  protected def fromIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]
  protected def fromSpecificIterable(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)

  protected[this] def newBuilder = new ArrayBuffer[A].mapResult(_.toArray(elemTag))

  override def knownSize = xs.length

  override def className = "Array"

  def map[B: ClassTag](f: A => B): Array[B] = fromIterable(View.Map(coll, f))
  def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = fromIterable(View.FlatMap(coll, f))
  def ++[B >: A : ClassTag](xs: IterableOnce[B]): Array[B] = fromIterable(View.Concat(coll, xs))
  def zip[B: ClassTag](xs: IterableOnce[B]): Array[(A, B)] = fromIterable(View.Zip(coll, xs))
}

case class ArrayView[A](xs: Array[A]) extends IndexedView[A] {
  def length = xs.length
  def apply(n: Int) = xs(n)
  override def className = "ArrayView"
}
