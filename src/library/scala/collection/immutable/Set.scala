/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import generic._
import parallel.immutable.ParSet

/** A generic trait for immutable sets.
 *  $setNote
 *  $setTags
 *
 *  @since 1.0
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @define Coll `immutable.Set`
 *  @define coll immutable set
 */
trait Set[A] extends Iterable[A]
//                with GenSet[A]
                with scala.collection.Set[A]
                with GenericSetTemplate[A, Set]
                with SetLike[A, Set[A]]
                with Parallelizable[A, ParSet[A]]
{
  override def companion: GenericCompanion[Set] = Set


  /** Returns this $coll as an immutable set, perhaps accepting a
   *  wider range of elements.  Since it already is an
   *  immutable set, it will only be rebuilt if the underlying structure
   *  cannot be expanded to include arbitrary element types.
   *  For instance, `BitSet` and `SortedSet` will be rebuilt, as
   *  they require `Int` and sortable elements respectively.
   *
   *  When in doubt, the set will be rebuilt.  Rebuilt sets never
   *  need to be rebuilt again.
   */
  override def toSet[B >: A]: Set[B] = {
      // This way of building sets typically has the best benchmarks, surprisingly!
    val sb = Set.newBuilder[B]
    foreach(sb += _)
    sb.result()
  }

  override def seq: Set[A] = this
  protected override def parCombiner = ParSet.newCombiner[A] // if `immutable.SetLike` gets introduced, please move this there!
}

/** $factoryInfo
 *  @define Coll `immutable.Set`
 *  @define coll immutable set
 */
object Set extends ImmutableSetFactory[Set] {
  override def newBuilder[A]: mutable.Builder[A, Set[A]] = new SetBuilderImpl[A]

  /** $setCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, A, Set[A]]]
  private[this] val ReusableCBF = setCanBuildFrom[Any]

  /** An optimized representation for immutable empty sets */
  @SerialVersionUID(-2443710944435909512L)
  private object EmptySet extends AbstractSet[Any] with Set[Any] with Serializable {
    override def size: Int = 0
    def contains(elem: Any): Boolean = false
    def + (elem: Any): Set[Any] = new Set1(elem)
    def - (elem: Any): Set[Any] = this
    def iterator: Iterator[Any] = Iterator.empty
    override def foreach[U](f: Any => U): Unit = ()
    override def toSet[B >: Any]: Set[B] = this.asInstanceOf[Set[B]]

    override def ++[B >: Any, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Set[Any], B, That]): That = {
      if (bf eq Set.canBuildFrom) that match {
        case hs: HashSet[Any] if hs.size > 4 => hs.asInstanceOf[That]
        case EmptySet => EmptySet.asInstanceOf[That]
        case hs: Set1[Any] => hs.asInstanceOf[That]
        case hs: Set2[Any] => hs.asInstanceOf[That]
        case hs: Set3[Any] => hs.asInstanceOf[That]
        case hs: Set4[Any] => hs.asInstanceOf[That]
        case _ => super.++(that)
      }
      else if (bf eq HashSet.canBuildFrom) that match {
        case hs: HashSet[Any] => hs.asInstanceOf[That]
        case _ => super.++(that)
      } else super.++(that)
    }

  }
  private[collection] def emptyInstance: Set[Any] = EmptySet

  @SerialVersionUID(3L)
  private abstract class SetNIterator[A](n: Int) extends AbstractIterator[A] with Serializable {
    private[this] var current = 0
    private[this] var remainder = n
    def hasNext = remainder > 0
    def apply(i: Int): A
    def next(): A =
      if (hasNext) {
        val r = apply(current)
        current += 1
        remainder -= 1
        r
      } else Iterator.empty.next()

    override def drop(n: Int): Iterator[A] = {
      if (n > 0) {
        current += n
        remainder = Math.max(0, remainder - n)
      }
      this
    }
  }

  /** An optimized representation for immutable sets of size 1 */
  @SerialVersionUID(1233385750652442003L)
  class Set1[A] private[collection] (elem1: A) extends AbstractSet[A] with Set[A] with Serializable {
    override def size: Int = 1
    def contains(elem: A): Boolean =
      elem == elem1
    def + (elem: A): Set[A] =
      if (contains(elem)) this
      else new Set2(elem1, elem)
    def - (elem: A): Set[A] =
      if (elem == elem1) Set.empty
      else this
    def iterator: Iterator[A] =
      Iterator.single(elem1)
    override def foreach[U](f: A => U): Unit = {
      f(elem1)
    }
    override def exists(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1)
    }
    override def forall(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1)
    }
    override private[scala] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] =
      if (pred(elem1) != isFlipped) this else Set.empty
    override def find(@deprecatedName('f) p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else None
    }
    override def head: A = elem1
    override def tail: Set[A] = Set.empty
    // Why is Set1 non-final?  Need to fix that!
    @deprecatedOverriding("This immutable set should do nothing on toSet but cast itself to a Set with a wider element type.", "2.11.8")
    override def toSet[B >: A]: Set[B] = this.asInstanceOf[Set1[B]]
  }

  /** An optimized representation for immutable sets of size 2 */
  @SerialVersionUID(-6443011234944830092L)
  class Set2[A] private[collection] (elem1: A, elem2: A) extends AbstractSet[A] with Set[A] with Serializable {
    override def size: Int = 2
    def contains(elem: A): Boolean =
      elem == elem1 || elem == elem2
    def + (elem: A): Set[A] =
      if (contains(elem)) this
      else new Set3(elem1, elem2, elem)
    def - (elem: A): Set[A] =
      if (elem == elem1) new Set1(elem2)
      else if (elem == elem2) new Set1(elem1)
      else this
    def iterator: Iterator[A] = new SetNIterator[A](size) {
      def apply(i: Int) = getElem(i)
    }
    private def getElem(i: Int) = i match { case 0 => elem1 case 1 => elem2 }

    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2)
    }
    override def exists(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) || p(elem2)
    }
    override def forall(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) && p(elem2)
    }
    override private[scala] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
      var r1: A = null.asInstanceOf[A]
      var n = 0
      if (pred(elem1) != isFlipped) {             r1 = elem1; n += 1}
      if (pred(elem2) != isFlipped) { if (n == 0) r1 = elem2; n += 1}

      n match {
        case 0 => Set.empty
        case 1 => new Set1(r1)
        case 2 => this
      }
    }
    override def find(@deprecatedName('f) p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else None
    }
    override def head: A = elem1
    override def tail: Set[A] = new Set1(elem2)
    // Why is Set2 non-final?  Need to fix that!
    @deprecatedOverriding("This immutable set should do nothing on toSet but cast itself to a Set with a wider element type.", "2.11.8")
    override def toSet[B >: A]: Set[B] = this.asInstanceOf[Set2[B]]
  }

  /** An optimized representation for immutable sets of size 3 */
  @SerialVersionUID(-3590273538119220064L)
  class Set3[A] private[collection] (elem1: A, elem2: A, elem3: A) extends AbstractSet[A] with Set[A] with Serializable {
    override def size: Int = 3
    def contains(elem: A): Boolean =
      elem == elem1 || elem == elem2 || elem == elem3
    def + (elem: A): Set[A] =
      if (contains(elem)) this
      else new Set4(elem1, elem2, elem3, elem)
    def - (elem: A): Set[A] =
      if (elem == elem1) new Set2(elem2, elem3)
      else if (elem == elem2) new Set2(elem1, elem3)
      else if (elem == elem3) new Set2(elem1, elem2)
      else this
    def iterator: Iterator[A] = new SetNIterator[A](size) {
      def apply(i: Int) = getElem(i)
    }
    private def getElem(i: Int) = i match { case 0 => elem1 case 1 => elem2 case 2 => elem3 }

    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3)
    }
    override def exists(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3)
    }
    override def forall(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3)
    }
    override private[scala] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
      var r1, r2: A = null.asInstanceOf[A]
      var n = 0
      if (pred(elem1) != isFlipped) {             r1 = elem1;                             n += 1}
      if (pred(elem2) != isFlipped) { if (n == 0) r1 = elem2 else             r2 = elem2; n += 1}
      if (pred(elem3) != isFlipped) { if (n == 0) r1 = elem3 else if (n == 1) r2 = elem3; n += 1}

      n match {
        case 0 => Set.empty
        case 1 => new Set1(r1)
        case 2 => new Set2(r1, r2)
        case 3 => this
      }
    }
    override def find(@deprecatedName('f) p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else if (p(elem3)) Some(elem3)
      else None
    }
    override def head: A = elem1
    override def tail: Set[A] = new Set2(elem2, elem3)
    // Why is Set3 non-final?  Need to fix that!
    @deprecatedOverriding("This immutable set should do nothing on toSet but cast itself to a Set with a wider element type.", "2.11.8")
    override def toSet[B >: A]: Set[B] = this.asInstanceOf[Set3[B]]
  }

  /** An optimized representation for immutable sets of size 4 */
  @SerialVersionUID(-3622399588156184395L)
  class Set4[A] private[collection] (elem1: A, elem2: A, elem3: A, elem4: A) extends AbstractSet[A] with Set[A] with Serializable {
    override def size: Int = 4
    def contains(elem: A): Boolean =
      elem == elem1 || elem == elem2 || elem == elem3 || elem == elem4
    def + (elem: A): Set[A] =
      if (contains(elem)) this
      else new HashSet[A] + elem1 + elem2 + elem3 + elem4 + elem
    def - (elem: A): Set[A] =
      if (elem == elem1) new Set3(elem2, elem3, elem4)
      else if (elem == elem2) new Set3(elem1, elem3, elem4)
      else if (elem == elem3) new Set3(elem1, elem2, elem4)
      else if (elem == elem4) new Set3(elem1, elem2, elem3)
      else this
    def iterator: Iterator[A] = new SetNIterator[A](size) {
      def apply(i: Int) = getElem(i)
    }
    private def getElem(i: Int) = i match { case 0 => elem1 case 1 => elem2 case 2 => elem3 case 3 => elem4 }

    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3); f(elem4)
    }
    override def exists(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3) || p(elem4)
    }
    override def forall(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3) && p(elem4)
    }
    override private[scala] def filterImpl(pred: A => Boolean, isFlipped: Boolean): Set[A] = {
      var r1, r2, r3: A = null.asInstanceOf[A]
      var n = 0
      if (pred(elem1) != isFlipped) {             r1 = elem1;                                                         n += 1}
      if (pred(elem2) != isFlipped) { if (n == 0) r1 = elem2 else             r2 = elem2;                             n += 1}
      if (pred(elem3) != isFlipped) { if (n == 0) r1 = elem3 else if (n == 1) r2 = elem3 else             r3 = elem3; n += 1}
      if (pred(elem4) != isFlipped) { if (n == 0) r1 = elem4 else if (n == 1) r2 = elem4 else if (n == 2) r3 = elem4; n += 1}

      n match {
        case 0 => Set.empty
        case 1 => new Set1(r1)
        case 2 => new Set2(r1, r2)
        case 3 => new Set3(r1, r2, r3)
        case 4 => this
      }
    }

    override def find(@deprecatedName('f) p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else if (p(elem3)) Some(elem3)
      else if (p(elem4)) Some(elem4)
      else None
    }
    override def head: A = elem1
    override def tail: Set[A] = new Set3(elem2, elem3, elem4)
    // Why is Set4 non-final?  Need to fix that!
    @deprecatedOverriding("This immutable set should do nothing on toSet but cast itself to a Set with a wider element type.", "2.11.8")
    override def toSet[B >: A]: Set[B] = this.asInstanceOf[Set4[B]]
  }
  /** Builder for Set.
   */
  private final class SetBuilderImpl[A] extends mutable.ReusableBuilder[A, Set[A]] {
    import scala.collection.immutable.HashSet.HashSetBuilder

    private[this] var elems: Set[A] = Set.empty[A]
    private[this] var switchedToHashSetBuilder: Boolean = false
    private[this] var hashSetBuilder: HashSetBuilder[A] = _

    override def clear(): Unit = {
      elems = Set.empty[A]
      if (hashSetBuilder ne null)
        hashSetBuilder.clear()
      switchedToHashSetBuilder = false
    }

    override def result(): Set[A] =
      if (switchedToHashSetBuilder) hashSetBuilder.result() else elems

    override def +=(elem: A) = {
      if (switchedToHashSetBuilder) {
        hashSetBuilder += elem
      } else if (elems.size < 4) {
        elems = elems + elem
      } else {
        // assert(elems.size == 4)
        if (elems.contains(elem)) {
          () // do nothing
        } else {
          convertToHashSetBuilder()
          hashSetBuilder += elem
        }
      }

      this
    }

    private def convertToHashSetBuilder(): Unit = {
      switchedToHashSetBuilder = true
      if (hashSetBuilder eq null)
        hashSetBuilder = new HashSetBuilder

      hashSetBuilder ++= elems
    }

    override def ++=(xs: TraversableOnce[A]): this.type = {
      xs match {
        case _ if switchedToHashSetBuilder =>
          hashSetBuilder ++= xs

        case set: collection.Set[A] if set.size > 4 =>
          convertToHashSetBuilder()
          hashSetBuilder ++= set

        case _ => super.++= (xs)
      }
      this
    }

  }


}

