/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



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
  /** $setCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] = setCanBuildFrom[A]

  /** An optimized representation for immutable empty sets */
  private object EmptySet extends AbstractSet[Any] with Set[Any] with Serializable {
    override def size: Int = 0
    def contains(elem: Any): Boolean = false
    def + (elem: Any): Set[Any] = new Set1(elem)
    def - (elem: Any): Set[Any] = this
    def iterator: Iterator[Any] = Iterator.empty
    override def foreach[U](f: Any => U): Unit = ()
    override def toSet[B >: Any]: Set[B] = this.asInstanceOf[Set[B]]
  }
  private[collection] def emptyInstance: Set[Any] = EmptySet

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
      Iterator(elem1)
    override def foreach[U](f: A => U): Unit = {
      f(elem1)
    }
    override def exists(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1)
    }
    override def forall(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1)
    }
    override def find(@deprecatedName('f) p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else None
    }
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
    def iterator: Iterator[A] =
      Iterator(elem1, elem2)
    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2)
    }
    override def exists(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) || p(elem2)
    }
    override def forall(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) && p(elem2)
    }
    override def find(@deprecatedName('f) p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else None
    }
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
    def iterator: Iterator[A] =
      Iterator(elem1, elem2, elem3)
    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3)
    }
    override def exists(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3)
    }
    override def forall(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3)
    }
    override def find(@deprecatedName('f) p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else if (p(elem3)) Some(elem3)
      else None
    }
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
      else new HashSet[A] + (elem1, elem2, elem3, elem4, elem)
    def - (elem: A): Set[A] =
      if (elem == elem1) new Set3(elem2, elem3, elem4)
      else if (elem == elem2) new Set3(elem1, elem3, elem4)
      else if (elem == elem3) new Set3(elem1, elem2, elem4)
      else if (elem == elem4) new Set3(elem1, elem2, elem3)
      else this
    def iterator: Iterator[A] =
      Iterator(elem1, elem2, elem3, elem4)
    override def foreach[U](f: A => U): Unit = {
      f(elem1); f(elem2); f(elem3); f(elem4)
    }
    override def exists(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) || p(elem2) || p(elem3) || p(elem4)
    }
    override def forall(@deprecatedName('f) p: A => Boolean): Boolean = {
      p(elem1) && p(elem2) && p(elem3) && p(elem4)
    }
    override def find(@deprecatedName('f) p: A => Boolean): Option[A] = {
      if (p(elem1)) Some(elem1)
      else if (p(elem2)) Some(elem2)
      else if (p(elem3)) Some(elem3)
      else if (p(elem4)) Some(elem4)
      else None
    }
    // Why is Set4 non-final?  Need to fix that!
    @deprecatedOverriding("This immutable set should do nothing on toSet but cast itself to a Set with a wider element type.", "2.11.8")
    override def toSet[B >: A]: Set[B] = this.asInstanceOf[Set4[B]]
  }
}

