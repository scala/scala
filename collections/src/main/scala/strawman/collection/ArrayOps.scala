package strawman
package collection

import scala.{AnyVal, Array, ArrayIndexOutOfBoundsException, Char, Int, throws, Boolean, Serializable, Unit, `inline`}
import scala.Predef.???
import mutable.{ArrayBuffer, GrowableBuilder, WrappedArray}
import immutable.ImmutableArray
import scala.reflect.ClassTag

object ArrayOps {
  private[ArrayOps] trait LowPriorityWithFilterOps[A] {
    protected def p: A => Boolean
    protected def ao: ArrayOps[A]
    protected def filtered = View.Filter(ao.toIterable, p, isFlipped = false)
    def map[B](f: A => B): immutable.IndexedSeq[B] = ao.iterableFactory.from(View.Map(filtered, f))
    def flatMap[B](f: A => IterableOnce[B]): immutable.IndexedSeq[B] = ao.iterableFactory.from(View.FlatMap(filtered, f))
  }

  class WithFilter[A](protected val p: A => Boolean, protected val ao: ArrayOps[A]) extends collection.WithFilter[A, immutable.IndexedSeq] with LowPriorityWithFilterOps[A] {
    def foreach[U](f: A => U): Unit = filtered.foreach(f)
    def map[B: ClassTag](f: A => B): Array[B] = ao.fromTaggedIterable(View.Map(filtered, f))
    def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = ao.fromTaggedIterable(View.FlatMap(filtered, f))
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](a => p(a) && q(a), ao)
  }
}

class ArrayOps[A](val xs: Array[A]) extends AnyVal
  with IterableOnce[A]
  with IndexedSeqOps[A, immutable.IndexedSeq, Array[A]]
  with StrictOptimizedSeqOps[A, Seq, Array[A]] {

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

  def flatMap[BS, B](f: A => BS)(implicit asIterable: BS => Iterable[B], m: ClassTag[B]): Array[B] =
    fromTaggedIterable(View.FlatMap(toIterable, (x: A) => asIterable(f(x))))

  def flatten[B](implicit asIterable: A => strawman.collection.Iterable[B], m: ClassTag[B]): Array[B] = {
    val b = WrappedArray.newBuilder[B]().mapResult(_.toArray)
    val sizes = super.map {
      case is: IndexedSeq[_] => is.size
      case _ => 0
    }
    b.sizeHint(sizes.sum)
    for (xs <- this)
      b ++= asIterable(xs)
    b.result()
  }

  @`inline` final def ++[B >: A : ClassTag](xs: Iterable[B]): Array[B] = appendedAll(xs)

  def zip[B: ClassTag](that: Iterable[B]): Array[(A, B)] = fromTaggedIterable(View.Zip(toIterable, that))

  def zipWithIndex(implicit ct: ClassTag[(A, Int)]): Array[(A, Int)] = fromTaggedIterable(View.ZipWithIndex(toIterable))

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

  /** Converts an array of pairs into an array of first elements and an array of second elements.
    *
    *  @tparam A1    the type of the first half of the element pairs
    *  @tparam A2    the type of the second half of the element pairs
    *  @param asPair an implicit conversion which asserts that the element type
    *                of this Array is a pair.
    *  @param ct1    a class tag for `A1` type parameter that is required to create an instance
    *                of `Array[A1]`
    *  @param ct2    a class tag for `A2` type parameter that is required to create an instance
    *                of `Array[A2]`
    *  @return       a pair of Arrays, containing, respectively, the first and second half
    *                of each element pair of this Array.
    */
  def unzip[A1, A2](implicit asPair: A => (A1, A2), ct1: ClassTag[A1], ct2: ClassTag[A2]): (Array[A1], Array[A2]) = {
    val a1 = new Array[A1](length)
    val a2 = new Array[A2](length)
    var i = 0
    while (i < length) {
      val e = asPair(apply(i))
      a1(i) = e._1
      a2(i) = e._2
      i += 1
    }
    (a1, a2)
  }

  def transpose[B](implicit asArray: A => Array[B]): Array[Array[B]] = {
    val aClass = xs.getClass.getComponentType
    val bb = Array.newBuilder[Array[B]](ClassTag[Array[B]](aClass))
    if (isEmpty) bb.result()
    else {
      def mkRowBuilder() = Array.newBuilder[B](ClassTag[B](aClass.getComponentType))
      val bs = asArray(head) map ((x: B) => mkRowBuilder())
      for (xs <- this) {
        var i = 0
        for (x <- asArray(xs)) {
          bs(i) += x
          i += 1
        }
      }
      for (b <- bs) bb += b.result()
      bb.result()
    }
  }
}
