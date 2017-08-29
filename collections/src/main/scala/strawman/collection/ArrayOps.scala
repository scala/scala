package strawman
package collection

import scala.{AnyVal, Array, ArrayIndexOutOfBoundsException, Char, Int, throws, Boolean, Serializable, Unit, `inline`}
import scala.Predef.???
import mutable.{ArrayBuffer, GrowableBuilder}
import immutable.ImmutableArray
import scala.reflect.ClassTag

object ArrayOps {
  class WithFilter[A](p: A => Boolean, ao: ArrayOps[A]) extends collection.WithFilter[A, immutable.IndexedSeq] {
    protected[this] def filtered = View.Filter(ao.toIterable, p, isFlipped = false)
    def map[B](f: A => B): immutable.IndexedSeq[B] = ao.iterableFactory.from(View.Map(filtered, f))
    def flatMap[B](f: A => IterableOnce[B]): immutable.IndexedSeq[B] = ao.iterableFactory.from(View.FlatMap(filtered, f))
    def foreach[U](f: A => U): Unit = filtered.foreach(f)
    def map[B: ClassTag](f: A => B): Array[B] = ao.fromTaggedIterable(View.Map(filtered, f))
    def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = ao.fromTaggedIterable(View.FlatMap(filtered, f))
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](a => p(a) && q(a), ao)
  }
}

class ArrayOps[A](val xs: Array[A]) extends AnyVal
  with IterableOnce[A]
  with IndexedSeqOps[A, immutable.IndexedSeq, Array[A]]
  with StrictOptimizedSeqOps[A, Seq, Array[A]]
  with ArrayLike[A] {

  protected def fromTaggedIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]

  override def withFilter(p: A => Boolean): ArrayOps.WithFilter[A] = new ArrayOps.WithFilter[A](p, this)

  def toIterable: IndexedSeq[A] = ImmutableArray.unsafeWrapArray(xs)
  protected[this] def coll: Array[A] = xs
  override def toSeq: immutable.Seq[A] = fromIterable(toIterable)

  def length = xs.length
  @throws[ArrayIndexOutOfBoundsException]
  def apply(i: Int) = xs.apply(i)

  def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

  def iterableFactory = immutable.IndexedSeq

  protected[this] def fromSpecificIterable(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)

  protected[this] def newSpecificBuilder() = ArrayBuffer.newBuilder[A]().mapResult(_.toArray(elemTag))

  override def className = "Array"

  def map[B: ClassTag](f: A => B): Array[B] = fromTaggedIterable(View.Map(toIterable, f))

  def mapInPlace(f: A => A): Array[A] = {
    var i = 0
    while (i < xs.length) {
      xs.update(i, f(xs(i)))
      i = i + 1
    }
    xs
  }

  def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = fromTaggedIterable(View.FlatMap(toIterable, f))

  @`inline` final def ++[B >: A : ClassTag](xs: Iterable[B]): Array[B] = appendedAll(xs)

  def zip[B: ClassTag](that: Iterable[B]): Array[(A, B)] = fromTaggedIterable(View.Zip(toIterable, that))

  def appended[B >: A : ClassTag](x: B): Array[B] = fromTaggedIterable(View.Append(toIterable, x))
  @`inline` final def :+ [B >: A : ClassTag](x: B): Array[B] = appended(x)
  def prepended[B >: A : ClassTag](x: B): Array[B] = fromTaggedIterable(View.Prepend(x, toIterable))
  @`inline` final def +: [B >: A : ClassTag](x: B): Array[B] = prepended(x)
  def prependedAll[B >: A : ClassTag](prefix: Iterable[B]): Array[B] = fromTaggedIterable(View.Concat(prefix, toIterable))
  @`inline` final def ++: [B >: A : ClassTag](prefix: Iterable[B]): Array[B] = prependedAll(prefix)
  def appendedAll[B >: A : ClassTag](suffix: Iterable[B]): Array[B] = fromTaggedIterable(View.Concat(toIterable, suffix))
  @`inline` final def :++ [B >: A : ClassTag](suffix: Iterable[B]): Array[B] = appendedAll(suffix)
  @`inline` final def concat[B >: A : ClassTag](suffix: Iterable[B]): Array[B] = appendedAll(suffix)

  def patch[B >: A : ClassTag](from: Int, other: Iterable[B], replaced: Int): Array[B] =
    fromTaggedIterable(new View.Patched(toIterable, from, other, replaced)) //TODO optimize
}
