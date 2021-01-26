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

import java.io.IOException

import generic._
import immutable.{NewRedBlackTree => RB}
import mutable.Builder
import scala.annotation.tailrec
import scala.runtime.{AbstractFunction1, AbstractFunction2}
import scala.util.hashing.MurmurHash3

/** $factoryInfo
 *  @define Coll immutable.TreeMap
 *  @define coll immutable tree map
 */
object TreeMap extends ImmutableSortedMapFactory[TreeMap] {
  def empty[A, B](implicit ord: Ordering[A]) = new TreeMap[A, B]()(ord)
  /** $sortedMapCanBuildFromInfo */
  implicit def canBuildFrom[A, B](implicit ord: Ordering[A]): CanBuildFrom[Coll, (A, B), TreeMap[A, B]] = new SortedMapCanBuildFrom[A, B]

  override def newBuilder[A, B](implicit ord: Ordering[A]): mutable.Builder[(A, B), TreeMap[A, B]] = new TreeMapBuilder

  private class TreeMapBuilder[A, B](implicit ordering: Ordering[A])
    extends RB.MapHelper[A, B]
      with Builder[(A, B), TreeMap[A, B]] {
    type Tree = RB.Tree[A, B]
    private var tree:Tree = null

    def +=(elem: (A, B)): this.type = {
      tree = mutableUpd(tree, elem._1, elem._2)
      this
    }
    private object adder extends AbstractFunction2[A, B, Unit] {
      // we cache tree to avoid the outer access to tree
      // in the hot path (apply)
      private[this] var accumulator :Tree = null
      def addForEach(hasForEach: HasForeachEntry[A, B]): Unit = {
        accumulator = tree
        hasForEach.foreachEntry(this)
        tree = accumulator
        // be friendly to GC
        accumulator = null
      }

      override def apply(key: A, value: B): Unit = {
        accumulator = mutableUpd(accumulator, key, value)
      }
    }

    override def ++=(xs: TraversableOnce[(A, B)]): this.type = {
      xs match {
        // TODO consider writing a mutable-safe union for TreeSet/TreeMap builder ++=
        // for the moment we have to force immutability before the union
        // which will waste some time and space
        // calling `beforePublish` makes `tree` immutable
        case ts: TreeMap[A, B] if ts.ordering == ordering =>
          if (tree eq null) tree = ts.tree0
          else tree = RB.union(beforePublish(tree), ts.tree0)
        case that: HasForeachEntry[A, B] =>
          //add avoiding creation of tuples
          adder.addForEach(that)
        case _ =>
          super.++=(xs)
      }
      this
    }

    override def clear(): Unit = {
      tree = null
    }

    override def result(): TreeMap[A, B] = new TreeMap[A, B](beforePublish(tree))
  }
  private val legacySerialisation = System.getProperty("scala.collection.immutable.TreeMap.newSerialisation", "false") == "false"

  @SerialVersionUID(-5672253444750945796L)
  private class TreeMapProxy[A, B](
    @transient private[this] var tree: RB.Tree[A, B],
    @transient private[this] var ordering: Ordering[A]) extends Serializable {

    @throws[IOException]
    private[this] def writeObject(out: java.io.ObjectOutputStream) = {
      out.writeInt(RB.count(tree))
      out.writeObject(ordering)
      RB.foreachEntry(tree, {
        (k: A, v: B) =>
          out.writeObject(k)
          out.writeObject(v)
      })
    }
    @throws[IOException]
    private[this] def readObject(in: java.io.ObjectInputStream) = {
      val size = in.readInt()
      ordering = in.readObject().asInstanceOf[Ordering[A]]
      size match {
        case 0 => //tree is null already
        case 1 =>
          val key   = in.readObject().asInstanceOf[A]
          val value = in.readObject().asInstanceOf[B]
          tree = RB.update(null, key, value, true)(ordering)
        case _ =>
          val keys   = new Array[Any](size)
          val values = new Array[Any](size)
          var i      = 0
          while (i < size) {
            keys(i) = in.readObject()
            values(i) = in.readObject()
            i += 1
          }
          tree = RB.fromOrderedEntries(
            keys.iterator.asInstanceOf[Iterator[A]],
            values.iterator.asInstanceOf[Iterator[B]],
            size)
      }
    }
    @throws[IOException]
    private[this] def readResolve(): AnyRef =
      new TreeMap(tree)(ordering)
  }

}

/** This class implements immutable maps using a tree.
 *
 *  @tparam A         the type of the keys contained in this tree map.
 *  @tparam B         the type of the values associated with the keys.
 *  @param ordering   the implicit ordering used to compare objects of type `A`.
 *
 *  @author  Erik Stenman
 *  @author  Matthias Zenger
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#red-black-trees "Scala's Collection Library overview"]]
 *  section on `Red-Black Trees` for more information.
 *
 *  @define Coll immutable.TreeMap
 *  @define coll immutable tree map
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(4714724050750123970L)
final class TreeMap[A, +B] private (tree: RB.Tree[A, B])(implicit val ordering: Ordering[A])
  extends SortedMap[A, B]
     with SortedMapLike[A, B, TreeMap[A, B]]
     with MapLike[A, B, TreeMap[A, B]]
     with Serializable
     with HasForeachEntry[A, B] {
  // Manually use this from inner classes to avoid having scalac rename `tree` to an expanded name which is not
  // serialization compatible.
  private[immutable] def tree0: RB.Tree[A, B] = tree

  override protected[this] def newBuilder : Builder[(A, B), TreeMap[A, B]] =
    TreeMap.newBuilder[A, B]

  override def size = RB.count(tree)

  def this()(implicit ordering: Ordering[A]) = this(null)(ordering)

  override def rangeImpl(from: Option[A], until: Option[A]): TreeMap[A, B] = newMapOrSelf[B](RB.rangeImpl(tree, from, until))
  override def range(from: A, until: A): TreeMap[A, B] = newMapOrSelf[B](RB.range(tree, from, until))
  override def from(from: A): TreeMap[A, B] = newMapOrSelf[B](RB.from(tree, from))
  override def to(to: A): TreeMap[A, B] = newMapOrSelf[B](RB.to(tree, to))
  override def until(until: A): TreeMap[A, B] = newMapOrSelf[B](RB.until(tree, until))

  override def firstKey = RB.smallest(tree).key
  override def lastKey = RB.greatest(tree).key
  override def compare(k0: A, k1: A): Int = ordering.compare(k0, k1)

  override def head = {
    val smallest = RB.smallest(tree)
    (smallest.key, smallest.value)
  }
  override def headOption = if (RB.isEmpty(tree)) None else Some(head)
  override def last = {
    val greatest = RB.greatest(tree)
    (greatest.key, greatest.value)
  }
  override def lastOption = if (RB.isEmpty(tree)) None else Some(last)

  override def tail = newMapOrSelf(RB.delete(tree, firstKey))
  override def init = newMapOrSelf(RB.delete(tree, lastKey))

  override def drop(n: Int) = {
    if (n <= 0) this
    else if (n >= size) empty
    else newMapOrSelf(RB.drop(tree, n))
  }

  override def take(n: Int) = {
    if (n <= 0) empty
    else if (n >= size) this
    else newMapOrSelf(RB.take(tree, n))
  }

  private def newMapOrSelf[B1 >: B](newTree: RB.Tree[A, B1]): TreeMap[A, B1] = {
    if (newTree eq tree) this
    else new TreeMap(newTree)
  }

  override def slice(from: Int, until: Int) = {
    if (until <= from) empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else newMapOrSelf(RB.slice(tree, from, until))
  }

  override def dropRight(n: Int) = take(size - math.max(n, 0))
  override def takeRight(n: Int) = drop(size - math.max(n, 0))
  override def splitAt(n: Int) = (take(n), drop(n))

  private[this] def countWhile(p: ((A, B)) => Boolean): Int = {
    var result = 0
    val it = iterator
    while (it.hasNext && p(it.next())) result += 1
    result
  }
  override def dropWhile(p: ((A, B)) => Boolean) = drop(countWhile(p))
  override def takeWhile(p: ((A, B)) => Boolean) = take(countWhile(p))
  override def span(p: ((A, B)) => Boolean) = splitAt(countWhile(p))

  /** A factory to create empty maps of the same type of keys.
   */
  override def empty: TreeMap[A, B] = newMapOrSelf(null)

  /** A new TreeMap with the entry added is returned,
   *  if key is <em>not</em> in the TreeMap, otherwise
   *  the key is updated with the new entry.
   *
   *  @tparam B1     type of the value of the new binding which is a supertype of `B`
   *  @param key     the key that should be updated
   *  @param value   the value to be associated with `key`
   *  @return        a new $coll with the updated binding
   */
  override def updated [B1 >: B](key: A, value: B1): TreeMap[A, B1] =
    newMapOrSelf(RB.update(tree, key, value, overwrite = true))

  /** Add a key/value pair to this map.
   *  @tparam   B1   type of the value of the new binding, a supertype of `B`
   *  @param    kv   the key/value pair
   *  @return        A new $coll with the new binding added to this map
   */
  override def + [B1 >: B] (kv: (A, B1)): TreeMap[A, B1] = updated(kv._1, kv._2)

  /** Adds two or more elements to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @tparam B1   type of the values of the new bindings, a supertype of `B`
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return      a new $coll with the updated bindings
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): TreeMap[A, B1] =
    this + elem1 + elem2 ++ elems

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param xs     the traversable object.
   */
  override def ++[B1 >: B] (xs: GenTraversableOnce[(A, B1)]): TreeMap[A, B1] = {
    xs match {
      case tm: TreeMap[A, B] if ordering == tm.ordering =>
        newMapOrSelf(RB.union(tree, tm.tree0))
      case ls: LinearSeq[(A,B1)] =>
        if (ls.isEmpty) this //to avoid the creation of the adder
        else {
          val adder = new Adder[B1]
          adder addAll ls
          newMapOrSelf(adder.finalTree)
        }
      case _ =>
        val adder = new Adder[B1]
        xs foreach adder
        newMapOrSelf(adder.finalTree)
    }
  }
  private final class Adder[B1 >: B]
    extends RB.MapHelper[A, B1] with Function1[(A, B1), Unit] {
    private var currentMutableTree: RB.Tree[A,B1] = tree0
    def finalTree = beforePublish(currentMutableTree)
    override def apply(kv: (A, B1)): Unit = {
      currentMutableTree = mutableUpd(currentMutableTree, kv._1, kv._2)
    }
    @tailrec def addAll(ls: LinearSeq[(A, B1)]): Unit = {
      if (!ls.isEmpty) {
        val kv = ls.head
        currentMutableTree = mutableUpd(currentMutableTree, kv._1, kv._2)
        addAll(ls.tail)
      }
    }
  }


  /** A new TreeMap with the entry added is returned,
   *  assuming that key is <em>not</em> in the TreeMap.
   *
   *  @tparam B1    type of the values of the new bindings, a supertype of `B`
   *  @param key    the key to be inserted
   *  @param value  the value to be associated with `key`
   *  @return       a new $coll with the inserted binding, if it wasn't present in the map
   */
  def insert [B1 >: B](key: A, value: B1): TreeMap[A, B1] = {
    assert(!RB.contains(tree, key))
    newMapOrSelf(RB.update(tree, key, value, overwrite = true))
  }

  def - (key:A): TreeMap[A, B] =
   newMapOrSelf(RB.delete(tree, key))

  private[collection] def removeAllImpl(xs: GenTraversableOnce[A]): TreeMap[A, B] =  xs match {
    case ts: TreeSet[A] if ordering == ts.ordering =>
      newMapOrSelf(RB.difference(tree, ts.tree))
    case _ =>
      //TODO add an implementation of a mutable subtractor similar to ++
      //but at least this doesnt create a TreeMap for each iteration
      object sub extends AbstractFunction1[A, Unit] {
        var currentTree = tree0
        override def apply(k: A): Unit = {
          currentTree = RB.delete(currentTree, k)
        }
      }
      xs.foreach(sub)
      newMapOrSelf(sub.currentTree)
  }


  /** Check if this map maps `key` to a value and return the
   *  value if it exists.
   *
   *  @param  key     the key of the mapping of interest
   *  @return         the value of the mapping, if it exists
   */
  override def get(key: A): Option[B] = RB.get(tree, key)
  override def getOrElse[V1 >: B](key: A, default: => V1): V1 = {
    val resultOrNull = RB.lookup(tree, key)
    if (resultOrNull eq null) default
    else resultOrNull.value
  }


  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  override def iterator: Iterator[(A, B)] = RB.iterator(tree)
  override def iteratorFrom(start: A): Iterator[(A, B)] = RB.iterator(tree, Some(start))

  override def keysIterator: Iterator[A] = RB.keysIterator(tree)
  override def keysIteratorFrom(start: A): Iterator[A] = RB.keysIterator(tree, Some(start))

  override def valuesIterator: Iterator[B] = RB.valuesIterator(tree)
  override def valuesIteratorFrom(start: A): Iterator[B] = RB.valuesIterator(tree, Some(start))

  override def contains(key: A): Boolean = RB.contains(tree, key)
  override def isDefinedAt(key: A): Boolean = RB.contains(tree, key)

  override def foreach[U](f : ((A,B)) => U) = RB.foreach(tree, f)

  override private[immutable] def foreachEntry[U](f: (A, B) => U): Unit = RB.foreachEntry(tree, f)

  override def hashCode(): Int = {
    if (isEmpty) {
      MurmurHash3.emptyMapHash
    } else {
      val hasher = new Map.HashCodeAccumulator()
      RB.foreachEntry(tree, hasher)
      hasher.finalizeHash
    }
  }
  override def keySet: SortedSet[A] = new TreeSet[A](tree)(ordering)

  override def equals(obj: Any): Boolean = obj match {
    case that: TreeMap[A, B] if ordering == that.ordering => RB.entriesEqual(tree, that.tree0)
    case _ => super.equals(obj)
  }

  override def values: scala.Iterable[B] = new DefaultValuesIterable {
    override def foreach[U](f: B => U): Unit = RB.foreachEntry(tree0, {(_: A, value: B) => f(value)})
  }
  override private[scala] def filterImpl(f: ((A, B)) => Boolean, isFlipped: Boolean) =
    newMapOrSelf(RB.filterEntries[A, B](tree, (k, v) => isFlipped ^ f((k, v))))

  override def partition(p: ((A, B)) => Boolean): (TreeMap[A, B], TreeMap[A, B]) = {
    val (l, r) = RB.partitionEntries[A, B](tree, (k, v) => p((k, v)))
    (newMapOrSelf(l), newMapOrSelf(r))
  }
  private def sameCBF(bf: CanBuildFrom[_,_,_]): Boolean = {
    bf match {
      case cbf: SortedMapFactory[_]#SortedMapCanBuildFrom[_,_] => {
        val factory:AnyRef = cbf.factory
        ((factory eq TreeMap) || (factory eq immutable.SortedMap) || (factory eq collection.SortedMap)) &&
          cbf.ordering == ordering
      }
      case w: WrappedCanBuildFrom[_,_,_] => sameCBF(w.wrapped)
      case _ => false
    }
  }


  override def transform[W, That](f: (A, B) => W)(implicit bf: CanBuildFrom[TreeMap[A, B], (A, W), That]): That = {
    if (sameCBF(bf))
      newMapOrSelf(RB.transform[A, B, W](tree, f)).asInstanceOf[That]
    else super.transform(f)
  }

  @throws[IOException]
  private[this] def writeReplace(): AnyRef =
    if (TreeMap.legacySerialisation) this else new TreeMap.TreeMapProxy(tree, ordering)

  @throws[IOException]
  private[this] def writeObject(out: java.io.ObjectOutputStream) = {
    out.writeObject(ordering)
    out.writeObject(immutable.RedBlackTree.from(tree))
  }


}
