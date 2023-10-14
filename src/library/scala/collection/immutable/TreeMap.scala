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

import scala.annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.immutable.{RedBlackTree => RB}
import scala.collection.mutable.ReusableBuilder
import scala.runtime.AbstractFunction2

/** An immutable SortedMap whose values are stored in a red-black tree.
  *
  * This class is optimal when range queries will be performed,
  * or when traversal in order of an ordering is desired.
  * If you only need key lookups, and don't care in which order key-values
  * are traversed in, consider using * [[scala.collection.immutable.HashMap]],
  * which will generally have better performance. If you need insertion order,
  * consider a * [[scala.collection.immutable.SeqMap]], which does not need to
  * have an ordering supplied.
  *
  *  @example {{{
  *  import scala.collection.immutable.TreeMap
  *
  *  // Make a TreeMap via the companion object factory
  *  val weekdays = TreeMap(
  *    2 -> "Monday",
  *    3 -> "Tuesday",
  *    4 -> "Wednesday",
  *    5 -> "Thursday",
  *    6 -> "Friday"
  *  )
  *  // TreeMap(2 -> Monday, 3 -> Tuesday, 4 -> Wednesday, 5 -> Thursday, 6 -> Friday)
  *
  *  val days = weekdays ++ List(1 -> "Sunday", 7 -> "Saturday")
  *  // TreeMap(1 -> Sunday, 2 -> Monday, 3 -> Tuesday, 4 -> Wednesday, 5 -> Thursday, 6 -> Friday, 7 -> Saturday)
  *
  *  val day3 = days.get(3) // Some("Tuesday")
  *
  *  val rangeOfDays = days.range(2, 5) // TreeMap(2 -> Monday, 3 -> Tuesday, 4 -> Wednesday)
  *
  *  val daysUntil2 = days.rangeUntil(2) // TreeMap(1 -> Sunday)
  *  val daysTo2 = days.rangeTo(2) // TreeMap(1 -> Sunday, 2 -> Monday)
  *  val daysAfter5 = days.rangeFrom(5) //  TreeMap(5 -> Thursday, 6 -> Friday, 7 -> Saturday)
  *  }}}
  *
  *  @tparam K         the type of the keys contained in this tree map.
  *  @tparam V         the type of the values associated with the keys.
  *  @param ordering   the implicit ordering used to compare objects of type `A`.
  *
  *  @see [[https://docs.scala-lang.org/overviews/collections-2.13/concrete-immutable-collection-classes.html#red-black-trees "Scala's Collection Library overview"]]
  *  section on `Red-Black Trees` for more information.
  *
  *  @define Coll immutable.TreeMap
  *  @define coll immutable tree map
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
final class TreeMap[K, +V] private (private val tree: RB.Tree[K, V])(implicit val ordering: Ordering[K])
  extends AbstractMap[K, V]
    with SortedMap[K, V]
    with StrictOptimizedSortedMapOps[K, V, TreeMap, TreeMap[K, V]]
    with SortedMapFactoryDefaults[K, V, TreeMap, Iterable, Map]
    with DefaultSerializable {

  def this()(implicit ordering: Ordering[K]) = this(null)(ordering)
  private[immutable] def tree0: RB.Tree[K, V] = tree

  private[this] def newMapOrSelf[V1 >: V](t: RB.Tree[K, V1]): TreeMap[K, V1] = if(t eq tree) this else new TreeMap[K, V1](t)

  override def sortedMapFactory: SortedMapFactory[TreeMap] = TreeMap

  def iterator: Iterator[(K, V)] = RB.iterator(tree)

  def keysIteratorFrom(start: K): Iterator[K] = RB.keysIterator(tree, Some(start))

  override def keySet: TreeSet[K] = new TreeSet(tree)(ordering)

  def iteratorFrom(start: K): Iterator[(K, V)] = RB.iterator(tree, Some(start))

  override def valuesIteratorFrom(start: K): Iterator[V] = RB.valuesIterator(tree, Some(start))

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[(K, V), S]): S with EfficientSplit =
    shape.parUnbox(
      scala.collection.convert.impl.AnyBinaryTreeStepper.from[(K, V), RB.Tree[K, V]](
        size, tree, _.left, _.right, x => (x.key, x.value)
      )
    )

  override def keyStepper[S <: Stepper[_]](implicit shape: StepperShape[K, S]): S with EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Tree[K, V]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]   (size, tree, _.left, _.right, _.key.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]  (size, tree, _.left, _.right, _.key.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T](size, tree, _.left, _.right, _.key.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[K, T](size, tree, _.left, _.right, _.key))
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  override def valueStepper[S <: Stepper[_]](implicit shape: StepperShape[V, S]): S with EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Tree[K, V]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]    (size, tree, _.left, _.right, _.value.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]   (size, tree, _.left, _.right, _.value.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T] (size, tree, _.left, _.right, _.value.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[V, T] (size, tree, _.left, _.right, _.value.asInstanceOf[V]))
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  def get(key: K): Option[V] = RB.get(tree, key)
  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    val resultOrNull = RB.lookup(tree, key)
    if (resultOrNull eq null) default
    else resultOrNull.value
  }

  def removed(key: K): TreeMap[K,V] =
    newMapOrSelf(RB.delete(tree, key))

  def updated[V1 >: V](key: K, value: V1): TreeMap[K, V1] =
    newMapOrSelf(RB.update(tree, key, value, overwrite = true))

  override def concat[V1 >: V](that: collection.IterableOnce[(K, V1)]): TreeMap[K, V1] =
    newMapOrSelf(that match {
      case tm: TreeMap[K, V] @unchecked if ordering == tm.ordering =>
        RB.union(tree, tm.tree)
      case ls: LinearSeq[(K,V1)] =>
        if (ls.isEmpty) tree //to avoid the creation of the adder
        else {
          val adder = new Adder[V1]
          adder.addAll(ls)
          adder.finalTree
        }
      case _ =>
        val adder = new Adder[V1]
        val it = that.iterator
        while (it.hasNext) {
          adder.apply(it.next())
        }
        adder.finalTree
    })

  override def removedAll(keys: IterableOnce[K]): TreeMap[K, V] = keys match {
    case ts: TreeSet[K] if ordering == ts.ordering =>
      newMapOrSelf(RB.difference(tree, ts.tree))
    case _ => super.removedAll(keys)
  }

  /** A new TreeMap with the entry added is returned,
   *  assuming that key is <em>not</em> in the TreeMap.
   *
   *  @tparam V1    type of the values of the new bindings, a supertype of `V`
   *  @param key    the key to be inserted
   *  @param value  the value to be associated with `key`
   *  @return       a new $coll with the inserted binding, if it wasn't present in the map
   */
  @deprecated("Use `updated` instead", "2.13.0")
  def insert[V1 >: V](key: K, value: V1): TreeMap[K, V1] = {
    assert(!RB.contains(tree, key))
    updated(key, value)
  }

  def rangeImpl(from: Option[K], until: Option[K]): TreeMap[K, V] = newMapOrSelf(RB.rangeImpl(tree, from, until))

  override def minAfter(key: K): Option[(K, V)] = RB.minAfter(tree, key) match {
    case null => Option.empty
    case x => Some((x.key, x.value))
  }

  override def maxBefore(key: K): Option[(K, V)] = RB.maxBefore(tree, key) match {
    case null => Option.empty
    case x => Some((x.key, x.value))
  }

  override def range(from: K, until: K): TreeMap[K,V] = newMapOrSelf(RB.range(tree, from, until))

  override def foreach[U](f: ((K, V)) => U): Unit = RB.foreach(tree, f)
  override def foreachEntry[U](f: (K, V) => U): Unit = RB.foreachEntry(tree, f)
  override def size: Int = RB.count(tree)
  override def knownSize: Int = size

  override def isEmpty = size == 0

  override def firstKey: K = RB.smallest(tree).key

  override def lastKey: K = RB.greatest(tree).key

  override def head: (K, V) = {
    val smallest = RB.smallest(tree)
    (smallest.key, smallest.value)
  }

  override def last: (K, V) = {
    val greatest = RB.greatest(tree)
    (greatest.key, greatest.value)
  }

  override def tail: TreeMap[K, V] = new TreeMap(RB.tail(tree))

  override def init: TreeMap[K, V] = new TreeMap(RB.init(tree))

  override def drop(n: Int): TreeMap[K, V] = {
    if (n <= 0) this
    else if (n >= size) empty
    else new TreeMap(RB.drop(tree, n))
  }

  override def take(n: Int): TreeMap[K, V] = {
    if (n <= 0) empty
    else if (n >= size) this
    else new TreeMap(RB.take(tree, n))
  }

  override def slice(from: Int, until: Int) = {
    if (until <= from) empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else new TreeMap(RB.slice(tree, from, until))
  }

  override def dropRight(n: Int): TreeMap[K, V] = take(size - math.max(n, 0))

  override def takeRight(n: Int): TreeMap[K, V] = drop(size - math.max(n, 0))

  private[this] def countWhile(p: ((K, V)) => Boolean): Int = {
    var result = 0
    val it = iterator
    while (it.hasNext && p(it.next())) result += 1
    result
  }

  override def dropWhile(p: ((K, V)) => Boolean): TreeMap[K, V] = drop(countWhile(p))

  override def takeWhile(p: ((K, V)) => Boolean): TreeMap[K, V] = take(countWhile(p))

  override def span(p: ((K, V)) => Boolean): (TreeMap[K, V], TreeMap[K, V]) = splitAt(countWhile(p))

  override def filter(f: ((K, V)) => Boolean): TreeMap[K, V] =
    newMapOrSelf(RB.filterEntries[K, V](tree, (k, v) => f((k, v))))

  override def partition(p: ((K, V)) => Boolean): (TreeMap[K, V], TreeMap[K, V]) = {
    val (l, r) = RB.partitionEntries[K, V](tree, (k, v) => p((k, v)))
    (newMapOrSelf(l), newMapOrSelf(r))
  }

  override def transform[W](f: (K, V) => W): TreeMap[K, W] = {
    val t2 = RB.transform[K, V, W](tree, f)
    if(t2 eq tree) this.asInstanceOf[TreeMap[K, W]]
    else new TreeMap(t2)
  }

  private final class Adder[B1 >: V]
    extends RB.MapHelper[K, B1] with Function1[(K, B1), Unit] {
    private var currentMutableTree: RB.Tree[K,B1] = tree0
    def finalTree = beforePublish(currentMutableTree)
    override def apply(kv: (K, B1)): Unit = {
      currentMutableTree = mutableUpd(currentMutableTree, kv._1, kv._2)
    }
    @tailrec def addAll(ls: LinearSeq[(K, B1)]): Unit = {
      if (!ls.isEmpty) {
        val kv = ls.head
        currentMutableTree = mutableUpd(currentMutableTree, kv._1, kv._2)
        addAll(ls.tail)
      }
    }
  }
  override def equals(obj: Any): Boolean = obj match {
    case that: TreeMap[K @unchecked, _] if ordering == that.ordering => RB.entriesEqual(tree, that.tree)
    case _ => super.equals(obj)
  }

  override protected[this] def className = "TreeMap"
}

/** $factoryInfo
  *  @define Coll immutable.TreeMap
  *  @define coll immutable tree map
  */
@SerialVersionUID(3L)
object TreeMap extends SortedMapFactory[TreeMap] {

  def empty[K : Ordering, V]: TreeMap[K, V] = new TreeMap()

  def from[K, V](it: IterableOnce[(K, V)])(implicit ordering: Ordering[K]): TreeMap[K, V] =
    it match {
      case tm: TreeMap[K, V] if ordering == tm.ordering => tm
      case sm: scala.collection.SortedMap[K, V] if ordering == sm.ordering =>
        new TreeMap[K, V](RB.fromOrderedEntries(sm.iterator, sm.size))
      case _ =>
        var t: RB.Tree[K, V] = null
        val i = it.iterator
        while (i.hasNext) {
          val (k, v) = i.next()
          t = RB.update(t, k, v, overwrite = true)
        }
        new TreeMap[K, V](t)
    }

  def newBuilder[K, V](implicit ordering: Ordering[K]): ReusableBuilder[(K, V), TreeMap[K, V]] = new TreeMapBuilder[K, V]

  private class TreeMapBuilder[K, V](implicit ordering: Ordering[K])
    extends RB.MapHelper[K, V]
      with ReusableBuilder[(K, V), TreeMap[K, V]] {
    type Tree = RB.Tree[K, V]
    private var tree:Tree = null

    def addOne(elem: (K, V)): this.type = {
      tree = mutableUpd(tree, elem._1, elem._2)
      this
    }
    private object adder extends AbstractFunction2[K, V, Unit] {
      // we cache tree to avoid the outer access to tree
      // in the hot path (apply)
      private[this] var accumulator :Tree = null
      def addForEach(hasForEach: collection.Map[K, V]): Unit = {
        accumulator = tree
        hasForEach.foreachEntry(this)
        tree = accumulator
        // be friendly to GC
        accumulator = null
      }

      override def apply(key: K, value: V): Unit = {
        accumulator = mutableUpd(accumulator, key, value)
      }
    }

    override def addAll(xs: IterableOnce[(K, V)]): this.type = {
      xs match {
        // TODO consider writing a mutable-safe union for TreeSet/TreeMap builder ++=
        // for the moment we have to force immutability before the union
        // which will waste some time and space
        // calling `beforePublish` makes `tree` immutable
        case ts: TreeMap[K, V] if ts.ordering == ordering =>
          if (tree eq null) tree = ts.tree0
          else tree = RB.union(beforePublish(tree), ts.tree0)
        case that: collection.Map[K, V]                  =>
          //add avoiding creation of tuples
          adder.addForEach(that)
        case _                                            =>
          super.addAll(xs)
      }
      this
    }

    override def clear(): Unit = {
      tree = null
    }

    override def result(): TreeMap[K, V] = new TreeMap[K, V](beforePublish(tree))
  }
}
