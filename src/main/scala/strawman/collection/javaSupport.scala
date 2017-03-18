package strawman.collection

import strawman.collection.immutable.List

import scala.{Array, Char, Int, AnyVal}
import scala.Predef.String
import strawman.collection.mutable.{ArrayBuffer, StringBuilder}

import scala.reflect.ClassTag

class StringOps(val s: String)
  extends AnyVal with IterableOps[Char]
    with SeqMonoTransforms[Char, String]
    with IterablePolyTransforms[Char, List]
    with MonoBuildable[Char, String]
    with ArrayLike[Char] {

  protected def coll = new StringView(s)
  def iterator() = coll.iterator()

  protected def fromIterableWithSameElemType(coll: Iterable[Char]): String = {
    val sb = new StringBuilder
    for (ch <- coll) sb += ch
    sb.result
  }

  def fromIterable[B](coll: Iterable[B]): List[B] = List.fromIterable(coll)

  protected[this] def newBuilderWithSameElemType = new StringBuilder

  def length = s.length
  def apply(i: Int) = s.charAt(i)

  override def knownSize = s.length

  override def className = "String"

  /** Overloaded version of `map` that gives back a string, where the inherited
    *  version gives back a sequence.
    */
  def map(f: Char => Char): String = {
    val sb = new StringBuilder
    for (ch <- s) sb += f(ch)
    sb.result
  }

  /** Overloaded version of `flatMap` that gives back a string, where the inherited
    *  version gives back a sequence.
    */
  def flatMap(f: Char => String): String = {
    val sb = new StringBuilder
    for (ch <- s) sb ++= f(ch)
    sb.result
  }

  /** Overloaded version of `++` that gives back a string, where the inherited
    *  version gives back a sequence.
    */
  def ++(xs: IterableOnce[Char]): String = {
    val sb = new StringBuilder() ++= s
    for (ch <- xs.iterator()) sb += ch
    sb.result
  }

  /** Another overloaded version of `++`. */
  def ++(xs: String): String = s + xs
}

case class StringView(s: String) extends IndexedView[Char] {
  def length = s.length
  def apply(n: Int) = s.charAt(n)
  override def className = "StringView"
}


class ArrayOps[A](val xs: Array[A])
  extends AnyVal with IterableOps[A]
    with SeqMonoTransforms[A, Array[A]]
    with MonoBuildable[A, Array[A]]
    with ArrayLike[A] {

  protected def coll = new ArrayView(xs)
  def iterator() = coll.iterator()

  def length = xs.length
  def apply(i: Int) = xs.apply(i)

  override def view = new ArrayView(xs)

  def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

  protected def fromIterableWithSameElemType(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)

  def fromIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]

  protected[this] def newBuilderWithSameElemType = new ArrayBuffer[A].mapResult(_.toArray(elemTag))

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
